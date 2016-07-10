module Servis3

import HTTP.URL

%access public export

||| An interface for defining universes of types that can be used in an API
||| @ u the universe to be defined
interface ApiUniverse u where
  ||| Decides to what types we should project elements of `u`
  el : u -> Type

||| Describes a part of an ApiUniverseRI path
|||
||| @ u the universe which we work in
data PathPart : u -> Type where
  ||| Dummy for routing.
  Const : (path : String) -> PathPart u
  ||| A route segment.    like  /users/:userid.
  ||| Captures data from a route.
  |||
  ||| @ name  the name of the capture. Purely for documentation purposes
  ||| @ type  the type of the capture. Can be any type in `u`
  Segment : (name : String) -> (type : u) -> PathPart u
  Wildcard : PathPart u

ApiUniverse u => ApiUniverse (PathPart u) where
  el (Const path) = ()
  el (Segment name type) = el type
  el Wildcard = ()

Eq u => Eq (PathPart u) where
  (Const path1) == (Const path2) = path1 == path2
  (Segment name1 type1) == (Segment name2 type2) =
    name1 == name2 && type1 == type2
  Wildcard == Wildcard = True
  _ == _ = False


||| Describes outputs of endpoints
|||
||| @ u the universe which we work in
data Output : u -> Type where
  ||| A HTTP GET.
  |||
  ||| @ responseType:  the response type of the GET
  GET : (responseType : u) -> Output u
  ||| A HTTP POST
  |||
  ||| @ requestType   the request body type of a POST
  ||| @ responseType  the response body type of a POST
  POST : (requestType : u) -> (responseType : u) -> Output u

ApiUniverse u => ApiUniverse (Output u) where
  el (GET responseType) = el responseType -> IO ()
  el (POST requestType responseType) = el requestType -> el responseType -> IO ()

Eq u => Eq (Output u) where
  (GET responseType1) == (GET responseType2) =
    responseType1 == responseType2
  (POST requestType1 responseType1) == (POST requestType2 responseType2) =
    requestType1 == requestType2 && responseType1 == responseType2
  _ == _ = False

||| Describes an HTTP Handler
data Handler : u -> Type where
  ||| Handling of a QueryParam. Allows us to capture query params
  |||
  ||| @ name  The name of the query param to be captured
  ||| @ type  The type of the query param to be captured
  QueryParam : (name : String) -> (type : u) -> (handler : Handler u) -> Handler u
  ||| A Handler outputs something
  ||| @ output  the output of the handler
  Outputs : (output : Output u) -> Handler u

ApiUniverse u => ApiUniverse (Handler u) where
  el (QueryParam name type handler) =
    el type -> el handler
  el (Outputs output) = el output

Eq u => Eq (Handler u) where
  (QueryParam name1 type1 handler1) == (QueryParam name2 type2 handler2) =
    name1 == name2 && type1 == type2 && handler1 == handler2
  (Outputs output1) == (Outputs output2) =
      output1 == output2
  _ == _ = False

data Api : u -> Type where
  Endpoint : Handler u -> Api u
  -- TODO nonempty
  OneOf : List (Api u) -> Api u
  (:>) : PathPart u -> Api u ->  Api u

infixr 5 :>

ApiUniverse u => ApiUniverse (Api u) where
  el (Endpoint handler) = el handler
  el (OneOf []) = ()
  el (OneOf (x::xs)) = (el x , el (OneOf xs))
  el (pathPart :> api) = el pathPart -> el api

Eq u => Eq (Api u) where
  (Endpoint handler1) == (Endpoint handler2) =
    handler1 == handler2
  (OneOf xs) == (OneOf ys) = xs == ys
  (pathPart1 :> api1) == (pathPart2 :> api2) =
    pathPart1 == pathPart2 && api1 == api2
  _ == _ = False

interface ApiUniverse u => Parse u where
  parse : (v : u) -> String -> Maybe (el v)

route : ApiUniverse u
      => (api : Api u)
      -> (handlers : el api)
      -> (url : URL)
      -> (requestBody : Maybe String)
      -> IO ()
