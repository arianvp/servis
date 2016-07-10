module Servis3

import HTTP.URL

%access public export

||| An interface for defining universes of types that can be used in an API
||| @ u the universe to be defined
interface Universe u where
  ||| Decides to what types we should project elements of `u`
  el : u -> Type


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


data Path : query -> Type where
  Const : (path : String) -> Path inPath
  Capture : (name : String) -> (type : inPath) -> Path inPath
  QueryParam : (name : String) -> (type : inPath) -> Path inPath
  (:>) : (left : Path inPath) -> (right : Path u) -> Path inPath

data Router : query -> req -> resp -> Type where
  Routes : Path inPath -> Handler req resp -> Router inPath req resp

data Api : query -> req -> resp -> Type where
  -- OneOf : Eq u => (routers : List (Router u)) -> {auto ok : NonEmpty routers} -> {auto ok: paths (routers) = nub (paths routers)} -> Api u
  OneOf : (routers : List (Router query req resp)) -> {auto ok : NonEmpty routers} -> Api query req resp

interface Universe u => Parse u where
  parse : (v : u) -> String -> Maybe (el v)
