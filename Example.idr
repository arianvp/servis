module Example
import Servis
import Data.Vect

data User = MkUser Int String


userApi : Api
userApi
  =
    Const "v1" :> Const "user" :> Segment "userId" Int :>
       Endpoint (GET User)
  :<|>
    Const "v1" :> Const "user" :>
      Endpoint (QueryParam "limit" Nat $ GET (List User))
  :<|>
    Const "v1" :> Const "user" :>
      Endpoint (POST User User)

getUser : (userId : Int) -> IO User
getUser userId = ?query "select * from users where id={userId}"

getUsers : Nat -> IO (List User)

postUser : User -> IO User


api : apiType Example.userApi
api = (getUser, getUsers, postUser)
