module Servis3
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
  Const : String -> PathPart u
  ||| A route segment.    like  /users/:userid.
  ||| Captures data from a route.
  |||
  ||| @ name  the name of the capture. Purely for documentation purposes
  ||| @ typ   the type of the capture. Can be any type in `u`
  Segment : (name : String) -> (typ:u) -> PathPart u
  Wildcard : PathPart u


|||
ApiUniverse u => ApiUniverse (PathPart u) where
  el (Const str) = ()
  el (Segment str u) = el u
  el Wildcard = ()

||| Structural equality instance
Eq u => Eq (PathPart u) where
  (Const str1) == (Const str2) = str1 == str2
  (Segment name1 typ1) == (Segment name2 typ2) =
    name1 == name2 && typ1 == typ2
  Wildcard == Wildcard = True
  _ == _ = False



||| Describes outputs of endpoints
|||
||| @ u the universe which we work in
data Output : u -> Type where
  ||| A HTTP GET.
  |||
  ||| @ response  the response type of the GET
  GET : (response : u) -> Output u
  ||| A HTTP POST
  |||
  ||| @ request   the request body type of a POST
  ||| @ response  the response body type of a POST
  POST : (request : u) -> (response : u) -> Output u

ApiUniverse u => ApiUniverse (Output u) where
  el (GET response) = el response -> IO ()
  el (POST request response) = el request -> el response -> IO ()

Eq u => Eq (Output u) where
  (GET response1) == (GET response2) =
    response1 == response2
  (POST request1 response1) == (POST request2 response2) =
    request1 == request2 && response1 == response2

||| Describes an HTTP Handler
data Handler : u -> Type where
  ||| Handling of a QueryParam. Allows us to capture query params
  |||
  ||| @ paramName   The name of the query param to be captured
  ||| @ type        The type of the query param to be captured
  QueryParam : (paramName : String) -> (type : u) -> Handler u -> Handler u
  Header : String -> (header: u) -> Handler u -> Handler u
  Outputs : Output u -> Handler u

ApiUniverse u => ApiUniverse (Handler u) where
  el (QueryParam s queryParam handler) =
    el queryParam -> el handler
  el (Header s header handler) =
    el header -> el handler
  el (Outputs output) = el output

Eq u => Eq (Handler u) where
  (QueryParam paramName1 type1 handler1) == (QueryParam paramName2 type2 handler2) =
    paramName1 == paramName2 && type1 == type2 && handler1 == handler2

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
interface ApiUniverse u => Parse u where
  ||| @ v     the API description
  ||| @ input the text to parse
  parse : (v : u) -> (input : String) -> Maybe (el v)

Parse ExampleUniv where
  parse NAT = Just . cast

Parse u => Parse (PathPart u) where
  parse (Segment str type) = parse type
  parse (Const str) = const Nothing
  parse (Wildcard) = const Nothing
