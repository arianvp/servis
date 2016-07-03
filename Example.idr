module Example
import Servis
import Data.Vect

data User = MkUser Int String


userApi : Api
userApi
  =
    path "v1/user" (Segment "userId" Int :> Endpoint (GET User))
  :<|>
    path "v1/user" (Endpoint $ QueryParam "limit" Nat $ GET $ List User)
  :<|>
    path "v1/user" (Endpoint $ POST User User)

getUser : (userId : Int) -> IO User
getUser userId = ?query "select * from users where id={userId}"

getUsers : Nat -> IO (List User)
getUsers limit = ?query "select * from users limit {limit}"

postUser : User -> IO User


api : apiType Example.userApi
api = (getUser, getUsers, postUser)
