module Servis

import Data.SortedMap

data Path : Type where
  Const : String -> Path
  Segment : String -> Type -> Path

data Handler : Type where

data Verb : Type where
  GET : Verb
  POST : Verb

Eq Verb where
  GET == GET = True
  POST == POST = True
  _  == _ = False

Ord Verb where
  compare GET POST  = LT
  compare POST POST = EQ
  compare GET GET = EQ
  compare POST GET = GT



data Api : Type where
  Endpoint : SortedMap Verb Handler -> Api
  -- OneOf    : List Api -> Api  TODO implement
  (:>)     : Path -> Api -> Api

infixr 5 :>

data ApiUser

--  "GET /users/1"
userApi : Api
userApi = Segment "userId" Int :> Endpoint (fromList [(GET, ?api)])


el : Api -> Type
el api = ?a

getFromDatabase : Int -> IO ApiUser
getFromDatabase userId = ?query "select * from users where id={userId}"

-- here is where the magic happens!!!
handleUserApi : el userApi
handleUserApi = getFromDatabase

{-data Path : Type where
  Const    : String -> Path
  Capture  : String -> Type -> Path
  Wildcard : Path


data Handler : Type where
  Outputs : Body -> Handler
    -- ^ TODO: StatusCodes?
  CaptureBody : ContentType -> Type -> Handler

data Output : Type where
  Respond : SortedMap HeaderName Type -> Body -> Output

data Body : Type where
  HasBody : ContentType -> Type -> Body

data ContentType : Type where
  JSON : ContentType


api : Api
api =
  OneOf
    ["users" :> Capture "userId" Int :> Endpoint handlers]

handlers : SortedMap Verb Handler
handlers =
  fromList
    [ (GET, getHandler)
    , (PPUT, putHandler)
    ]

getHandler : Handler
getHandler = Outputs User
-}
