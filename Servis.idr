module Servis

import HTTP.URL
import Data.Vect
import Data.HVect

%access public export

interface  Route u where
  el : u -> Type
  route : (v : u) -> (handler : el v) -> (url : URL) -> (requestBody : Maybe String) -> Maybe (IO String)

interface Route u => FromQueryParam u where
  fromQueryParam : (v : u) -> String -> Maybe (el v)

interface Route u => FromCapture u where
  fromCapture : (v : u) -> String -> Maybe (el v)

interface Route u => FromRequest u where
  fromRequest : (v : u) -> String -> Maybe (el v)

interface Route u => ToResponse u where
  toResponse : (v : u) -> el v -> String

data Handler : req -> resp -> Type where
  GET : (responseType : resp) -> Handler req resp
  POST : (requestType : req) -> (responseType : resp) -> Handler req resp


implementation (ToResponse resp, FromRequest req) => Route (Handler req resp) where
  el (GET responseType) = IO (el responseType)
  el (POST requestType responseType) = el requestType -> IO (el responseType)
  route (GET responseType) handler url requestBody =
    pure (map (toResponse responseType) handler)
  route (POST requestType responseType) handler url requestBody = do
    body <- requestBody
    request <- fromRequest requestType body
    pure (map (toResponse responseType) (handler request))

data PathPart : capture -> query -> Type where
  Const : (path : String) -> PathPart capture query
  Capture : (name : String) -> (type : capture) -> PathPart capture query
  QueryParam : (name : String) -> (type : query) -> PathPart capture query

data Path : capture -> query -> req -> resp -> Type where
-- (:>) : (left : PathPart capture query) -> (el left => (right : Path capture query req resp)) -> Path capture query req respA
-- (>>) : PathPArt -> Path -> Path
-- pp >> p = pp  :> const ps
  (:>) : (left : PathPart capture query) -> (right : Path capture query req resp) -> Path capture query req resp
  Outputs : Handler req resp -> Path capture query req resp
infixr 5 :>


implementation ( FromCapture capture
               , FromQueryParam query
               , FromRequest req
               , ToResponse resp
               ) => Route (Path capture query req resp) where
  el (Const path :> right) =  el right
  el (Capture name type :> right) = el type -> el right
  el (QueryParam name type :> right) = el type -> el right
  el (Outputs handler) = el handler
  route ((Const path) :> right) handler (MkURL pathParts params) requestBody =
    --  Check if path in url. pop url
    --  Do nothing. because const carries no information
    -- recurse on `right`
    case pathParts of
      [] => Nothing
      (x::xs) => do
        guard (x == path)
        route right handler (MkURL xs params) requestBody
  route ((Capture name type) :> right) handler (MkURL pathParts params) requestBody =
    --  Parse capture from url, pop url.
    -- partially apply  handler with parsed result
    -- recurse on  `right` with popped url and new handler
    case pathParts of
      [] => Nothing
      (x::xs) => do
        capture <- fromCapture type x
        route right (handler capture) (MkURL xs params) requestBody

  route ((QueryParam name type) :> right) handler (MkURL pathParts params) requestBody =
    -- Get query param from url, pop query param
    -- partially apply handler with parsed query param
    -- recurse on `right` with popped query handler and new partially applied handler
    case params of
      [] => Nothing
      -- BUG: Gotta do a lookup here
      ((name', val) :: xs) => do
        guard (name == name')
        param <- fromQueryParam type val
        route right (handler param) (MkURL pathParts xs) requestBody

  route (Outputs x) handler url requestBody = route x handler url requestBody

--extractPathInfo : Path capture query req resp -> List (PathPart capture query)
--extractPathInfo (Handle x) = []
--extractPathInfo (left :> right) = left :: extractPathInfo right

data Api : capture -> query -> req -> resp -> Type where
  |||
  ||| @ paths  a list of paths we can choose from
  -- ||| @ noOverlappingPaths  A Proof that we have no paths that overlap
  OneOf : (paths : Vect (S n) (Path capture query req resp)) ->
          Api capture query req resp

implementation (FromCapture capture
              , FromQueryParam query
              , FromRequest req
              , ToResponse resp) =>
  Route (Api capture query req resp) where
  el (OneOf xs) = HVect (map el xs)

  route (OneOf (path :: [])) (handler :: []) url requestBody =
    -- if no handlers are left but this one. We should allow it to fail
    route path handler url requestBody
  route (OneOf (path :: x :: xs)) (handler :: h :: handlers) url requestBody =
    -- if other handlers are present. Try the handler and otherwise try others
    case route path handler url requestBody of
      Nothing =>
        route (OneOf (x :: xs))
              (the (HVect _ ) (h :: handlers))  -- Note this is to help the compiler
              url
              requestBody
      a => a



{-data Disjoint : Path -> Path -> Type where
  Base : Disjoint pp1 pp2 ->  Disjoint (pp1 :> p1) (pp2 :> p2)
  Step : Disjoint p1 p2 -> Disjoint (p :> p1)  (p :> p2)

data Disjoint : PP -> PP -> Type where
  ConstDisjoint : Not (str = str') -> Disjoint (Const Str) (Const str)


 for depedendnets:

  Step : pp, p1, p2
-}
