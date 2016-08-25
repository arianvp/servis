module Example
import Servis.API
import Servis.Server
import Servis.Docs
import Data.Vect
import Data.HVect


-- universe of path captures
data CaptureU = CINT | CSTRING

Universe CaptureU where
  el CINT = Int
  el CSTRING = String

HasDocs CaptureU where
  docs CINT = "Int"
  docs CSTRING = "String"

-- universe of queryparams
data QueryU = QINT | QSTRING

Universe QueryU where
  el QINT = Int
  el QSTRING = String

HasDocs QueryU where
  docs QINT = "Int"
  docs QSTRING = "String"

data NoU

DecEq NoU where
  decEq a b impossible

Universe NoU where
  el _ = ()

HasDocs NoU where
  docs _ = "()"

-- universe of response types
data RespU = USER | LIST RespU


record User where
  constructor MkUser
  name : String
  id : Int

HasDocs User where
  docs _ = "User"

Universe RespU where
  el USER = User
  el (LIST x) = List (el x)

HasDocs RespU where
  docs USER = "User"
  docs (LIST x) = "List (" ++ docs x ++ ")"


lel : List (Path CaptureU QueryU NoU RespU)
lel = 
  [ Const "user" :> Outputs (GET USER)
  , Const "user2" :> Outputs (GET (LIST USER))
  ]

bo : List (Path CaptureU QueryU NoU RespU)
bo = checkIt lel

UserApi : API CaptureU QueryU NoU RespU
UserApi = OneOf
  [ Const "user" :> Capture "userId" CINT :> Outputs (GET USER)
  , Const "user" :> QueryParam "limit" QINT :> Outputs (GET (LIST USER))
  ]

getUserById: Int -> IO User
getUserById x = return (MkUser "hey" 0)

getUsersLimit : Int -> IO (List User)
getUsersLimit x = return ([MkUser "hey" 0])

documentation : String
documentation = docs path
  where path : Path CaptureU NoU NoU RespU
        path = (Const "user" :> Capture "userId" CINT :> Outputs (GET USER))

userApi : el UserApi
userApi = [getUserById, getUsersLimit]



