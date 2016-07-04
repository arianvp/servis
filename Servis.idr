module Servis


data UserSpec : Type where
  USER : UserSpec

record User where
  constructor MkUser
  username : String
  password : String

data IntSpec : Type where
  INT : IntSpec


-- Casts an element from our Universe to a type
public export
interface Interpret (a : Type) where
  total
  interpret : a -> Type

public export
interface  Interpret a => FromSegment a where
  total
  fromSegment : {b : a} -> String -> interpret b


public export
interface Interpret a => FromQueryParam (a : Type) where
  total
  fromQueryParam : {b : a} -> String -> interpret b

public export
interface Interpret a => FromJSON (a : Type) where
  total
  fromJSON : {b : a} -> String -> interpret b

public export
interface Interpret a => ToJSON (a : Type) where
  total
  toJSON : {b : a} -> interpret b -> String

data StringU : Type where
  STRING : StringU

Interpret StringU where
  interpret _ = String

FromSegment StringU where
  fromSegment str = str

public export data PathPart :  FromSegment a => a -> Type where
  Const : String -> PathPart
  Segment : String -> a -> PathPart

public export
PathPartType : PathPart a -> Maybe Type
PathPartType (Const s) = Nothing
PathPartType (Segment s seg) = Just $ interpret seg

parsePathPart : FromSegment a => String -> PathPart a -> Maybe a
parsePathPart string pathPart =
  case PathPartType pathPart of
    Nothing => Nothing
    Just typ => the typ (fromSegment string)


public export
data Method : Type where
  GET : ToJSON response => response -> Method
  POST : (FromJSON request, ToJSON response) => request -> response -> Method
  QueryParam : (FromQueryParam queryParam) => String -> queryParam -> Method -> Method

public export
MethodType : Method -> Type
MethodType (GET response) = IO (interpret response)
MethodType (POST request response) = interpret request -> IO (interpret response)
MethodType (QueryParam paramName queryParam sub) = interpret queryParam -> MethodType sub

{-
public export
MethodType : Method  -> Type
MethodType (GET response) =  IO response
MethodType (POST requestBody response)  = requestBody -> IO response
MethodType (QueryParam paramName queryParam sub)  = queryParam -> MethodType sub


public export data Api : Type where
  Endpoint : Method -> Api
  -- OneOf    : List Api -> Api  TODO implement
  (:>)     : PathPart -> Api -> Api

  (:<|>) : Api -> Api -> Api

infixr 9 :>
infixr 8 :<|>

public export
path : String -> Api -> Api
path path api = foldr (:>) api . map Const . split (== '/') $ path

public export
ApiType : Api -> Type
ApiType (path :> api) = PathPartType path (ApiType api)
ApiType (Endpoint x) =  MethodType x
ApiType (api1 :<|> api2) = (ApiType api1, ApiType api2)

-}
-- Tussen 10 en 12 en 2 tot 4
