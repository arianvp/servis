module Servis.Server

import public HTTP.URL
import Servis.API
import Data.Vect
import Data.HVect
import Data.List

%default total
%access public export

interface Universe u => FromQueryParam u where
  fromQueryParam : (v : u) -> String -> Maybe (el v)

interface Universe u => FromCapture u where
  fromCapture : (v : u) -> String -> Maybe (el v)

interface Universe u => FromRequest u where
  fromRequest : (v : u) -> String -> Maybe (el v)

interface Universe u => ToResponse u where
  toResponse : (v : u) -> el v -> String

interface Universe u => HasServer u where
  route : (v : u) -> (handler : el v) -> (url : URL) -> (requestBody : Maybe String) -> Maybe (IO String)

(ToResponse resp, FromRequest req) => HasServer (Handler req resp) where
  route (GET responseType) handler url requestBody =
    pure (map (toResponse responseType) handler)
  route (DGET responseType statuses) (MkDPair status prf, handler) ur requestBody =
          Just $ do
            putStrLn status
            map (toResponse responseType) handler
  route (POST requestType responseType) handler url requestBody = do
    body <- requestBody
    request <- fromRequest requestType body
    pure (map (toResponse responseType) (handler request))


( FromCapture capture
, FromQueryParam query
, FromRequest req
, ToResponse resp
) => HasServer (Path capture query req resp) where
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

  route ((QueryParam name type) :> right) handler (MkURL pathParts params) requestBody = do
    -- Get query param from url, pop query param
    -- partially apply handler with parsed query param
    -- recurse on `right` with popped query handler and new partially applied handler
    val <- lookup name params
    param <- fromQueryParam type val
    let newParams = dropWhile ((== name) . fst) params
    route right (handler param) (MkURL pathParts newParams) requestBody

  route (Outputs x) handler url requestBody = route x handler url requestBody

--extractPathInfo : Path capture query req resp -> List (PathPart capture query)
--extractPathInfo (Handle x) = []
--extractPathInfo (left :> right) = left :: extractPathInfo right


( FromCapture capture
, FromQueryParam query
, FromRequest req
, ToResponse resp) =>
  HasServer (API capture query req resp) where

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

