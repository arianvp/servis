module Servis




public export data Path : Type where
  Const : String -> Path
  Segment : String -> Type -> Path

public export
pathType : Path -> Type -> Type
pathType (Const s) type = type
pathType (Segment s type1) type2 = type1 -> type2


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
methodType : Method  -> Type
methodType (GET response) =  IO response
methodType (POST requestBody response)  = requestBody -> IO response
methodType (QueryParam paramName queryParam sub)  = queryParam -> methodType sub


public export data Api : Type where
  Endpoint : Method -> Api
  -- OneOf    : List Api -> Api  TODO implement
  (:>)     : Path -> Api -> Api

  (:<|>) : Api -> Api -> Api

infixr 9 :>
infixr 8 :<|>

public export
apiType : Api -> Type
apiType (path :> api) = pathType path (apiType api)
apiType (Endpoint x) =  methodType x
apiType (api1 :<|> api2) = (apiType api1, apiType api2)


-- Tussen 10 en 12 en 2 tot 4
