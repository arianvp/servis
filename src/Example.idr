module Example
import Servis.API
import Servis.Server
import Data.Vect
import Data.HVect


-- universe of path captures
data CaptureU = CINT | CSTRING

implementation Universe CaptureU where
  el CINT = Int
  el CSTRING = String

-- universe of queryparams
data QueryU = QINT | QSTRING

implementation Universe QueryU where
  el QINT = Int
  el QSTRING = String

data NoU

implementation Universe NoU where
  el _ = ()


-- universe of response types
data RespU = USER | LIST RespU


record User where
  constructor MkUser
  name : String
  id : Int

implementation Universe RespU where
  el USER = User
  el (LIST x) = List (el x)


UserApi : Api CaptureU QueryU NoU RespU
UserApi = OneOf
  [ Const "user" :> Capture "userId" CINT :> Outputs (GET USER)
  , Const "user" :> QueryParam "limit" QINT :> Outputs (GET (LIST USER))
  ]

getUserById: Int -> IO User
getUserById x = return (MkUser "hey" 0)

getUsersLimit : Int -> IO (List User)
getUsersLimit x = return ([MkUser "hey" 0])

userApi : el UserApi
userApi = [getUserById, getUsersLimit]
