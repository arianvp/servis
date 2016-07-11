module Servis

import HTTP.URL

%access public export

||| An interface for defining universes of types that can be used in an API
||| @ u the universe to be defined
interface Universe u where
  ||| Decides to what types we should project elements of `u`
  el : u -> Type

interface Universe u => FromQueryParam u where
  fromQueryParam : (v : u) -> String -> el v

interface Universe u => FromCapture u where
  fromCapture : (v : u) -> String -> el v

interface Universe u => FromRequest u where
  fromRequest : (v : u) -> String -> el v

interface Universe u => ToResponse u where
  toResponse : (v : u) -> el v -> String


interface Universe u => Route u where
  route : (v : u) -> (handler : el v) -> (url : URL) -> (requestBody : Maybe String) -> Maybe (IO String)

data Handler : req -> resp -> Type where
  GET : (responseType : resp) -> Handler req resp
  POST : (requestType : req) -> (responseType : resp) -> Handler req resp


implementation (Universe req, Universe resp) => Universe (Handler req resp) where
  el (GET responseType) = IO (el responseType)
  el (POST requestType responseType) = el requestType -> IO (el responseType)

implementation (ToResponse resp, FromRequest req) => Route (Handler req resp) where
  route (GET responseType) handler url requestBody =
    Just (map (toResponse responseType) handler)
  route (POST requestType responseType) handler url requestBody =
    case requestBody of
      Nothing => Nothing
      Just body => Just (map (toResponse responseType) (handler (fromRequest requestType body)))

data PathPart : capture -> query -> Type where
  Const : (path : String) -> PathPart capture query
  Capture : (name : String) -> (type : capture) -> PathPart capture query
  QueryParam : (name : String) -> (type : query) -> PathPart capture query

implementation (Eq capture, Eq query) => Eq (PathPart capture query) where
  (Const a) == (Const b) = a == b
  (Capture _ type1) == (Capture _ type2) = type1 == type2
  (QueryParam _ type1) == (QueryParam _ type2) = type2 == type2
  _ == _ =  False

data Path : capture -> query -> req -> resp -> Type where
  (:>) : (left : PathPart capture query) -> (right : Path capture query req resp) -> Path capture query req resp
  Outputs : Handler req resp -> Path capture query req resp
infixr 5 :>


implementation ( Universe capture
               , Universe query
               , Universe req
               , Universe resp
               ) => Universe (Path capture query req resp) where
  el (Const path :> right) = el right
  el (Capture name type :> right) = el type -> el right
  el (QueryParam name type :> right) = el type -> el right
  el (Outputs handler) = el handler

implementation ( FromCapture capture
               , FromQueryParam query
               , FromRequest req
               , ToResponse resp
               ) => Route (Path capture query req resp) where
  route ((Const path) :> right) handler url requestBody =
    --  Check if path in url. pop url
    --  Do nothing. because const carries no information
    -- recurse on `right`
    ?d_3
  route ((Capture name type) :> right) handler url requestBody =
    --  Parse capture from url, pop url.
    -- partially apply  handler with parsed result
    -- recurse on  `right` with popped url and new handler
    ?d_4
  route ((QueryParam name type) :> right) handler url requestBody =
    -- Get query param from url, pop query param
    -- partially apply handler with parsed query param
    -- recurse on `right` with popped query handler and new partially applied handler
    ?d_5

  route (Outputs x) handler url requestBody = route x handler url requestBody

extractPathInfo : Path capture query req resp -> List (PathPart capture query)
extractPathInfo (Handle x) = []
extractPathInfo (left :> right) = left :: extractPathInfo right

||| Super cool
data Api : capture -> query -> req -> resp -> Type where
  |||
  ||| @ paths  a list of paths we can choose from
  ||| @ thereShouldBePaths  A proof that we have at least one path defined
  ||| @ noOverlappingPaths  A Proof that we have no paths that overlap
  OneOf : (Eq capture, Eq query)
        => (paths : List (Path capture query req resp))
       -> {auto thereShouldBePaths: NonEmpty paths}
       -> {auto noOverlappingPaths: map Servis.extractPathInfo paths = nub (map Servis.extractPathInfo paths)}
       -> Api capture query req resp

-- PSEUDO CODE:
implementation Route Api where
  route = ?d -- keep trying a route in paths until one does not return Nothing
