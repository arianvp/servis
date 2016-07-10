module Servis3

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


data Path : capture -> query -> Type where
  Const : (path : String) -> Path capture query
  Capture : (name : String) -> (type : capture) -> Path capture query
  QueryParam : (name : String) -> (type : query) -> Path capture query
  (:>) : (left : Path capture query) -> (right : Path capture query) -> Path capture query

data Router : capture -> query -> req -> resp -> Type where
  Routes : Path capture query -> Handler req resp -> Router capture  query req resp

data Api : capture -> query -> req -> resp -> Type where
  -- OneOf : Eq u => (routers : List (Router u)) -> {auto ok : NonEmpty routers} -> {auto ok: paths (routers) = nub (paths routers)} -> Api u
  OneOf : (routers : List (Router capture query req resp)) -> {auto ok : NonEmpty routers} -> Api capture query req resp

interface Universe u => Parse u where
  parse : (v : u) -> String -> Maybe (el v)
