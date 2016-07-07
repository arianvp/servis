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

ApiUniverse u => ApiUniverse (PathPart u) where
  el (Const str) = ()
  el (Segment str u) = el u
  el Wildcard = ()

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
  el (GET response) = el response
  el (POST request response) = el request -> el response

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


