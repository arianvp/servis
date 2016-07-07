module Servis

public export data PathPart : Type where
  Const : String -> PathPart
  Segment : String -> Type -> PathPart


public export
PathPartType : PathPart -> Maybe Type
PathPartType (Const s) = Nothing
PathPartType (Segment s type) = Just type



-- TODO: Currently we assume request body is json or whatever.
-- and so is response. we dont really do any Content-Type stuff yet


public export data Method : Type where

  -- | A GET
  GET  : (response : Type) -> Method

  -- | Handle a post with an optional body
  POST : (requestBody : Type) -> (response : Type) -> Method

  -- | Capture any query parameters
  QueryParam : (paramName : String) -> (queryParam : Type) -> (sub : Method) -> Method

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
ApiType (path :> api) =
  case PathPartType path of
    Just typ =>  typ -> ApiType api
    Nothing => ApiType api
ApiType (Endpoint x) =  MethodType x
ApiType (api1 :<|> api2) = (ApiType api1, ApiType api2)


-- Tussen 10 en 12 en 2 tot 4
