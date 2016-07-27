module Servis.Example
import Servis.API
import Servis.Server
import Servis.Docs
import Data.Vect
import Data.HVect
import Data.BoundedList


-- universe of path captures
data CaptureU = CINT | CSTRING

Universe CaptureU where
  el CINT = Int
  el CSTRING = String

HasDocs CaptureU where
  docs CINT = "Int"
  docs CSTRING = "String"

-- universe of queryparams
data QueryU = QINT | QSTRING | QNAT

Universe QueryU where
  el QNAT = Nat
  el QINT = Int
  el QSTRING = String

HasDocs QueryU where
  docs QINT = "Int"
  docs QSTRING = "String"

data EmptyU

Universe EmptyU where
  el _ = ()

HasDocs EmptyU where
  docs _ = "()"

-- universe of response types
data RespU = USER | LIST RespU | BLIST Nat RespU


record User where
  constructor MkUser
  name : String
  id : Int

HasDocs User where
  docs _ = "User"

Universe RespU where
  el USER = User
  el (LIST x) = List (el x)
  el (BLIST n x) = BoundedList n (el x)

HasDocs RespU where
  docs USER = "User"
  docs (LIST x) = "List (" ++ docs x ++ ")"
  docs (BLIST n x) = "BoundedList " ++ show n ++ "(" ++ docs x ++ ")"

UserApi : API CaptureU QueryU EmptyU RespU
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
  where path : Path CaptureU EmptyU EmptyU RespU
        path = (Const "user" :> Capture "userId" CINT :> Outputs (GET USER))

userApi : el UserApi
userApi = [getUserById, getUsersLimit]


dependentAPI : API EmptyU QueryU EmptyU RespU

 -- "/user?begin=0&end=5 => returns a Vect 5 User"
 -- "/user?begin=0&end=0 => returns a List User"
dependentAPI = OneOf
 [  Const "user" 
 :> QueryParam "begin" QNAT
 :*> \begin => QueryParam "end" QNAT 
 :*> \end => Outputs $ case isLTE begin end of
                          Yes bLTEe => GET (BLIST ((-) end begin {smaller=bLTEe}) USER)
                          No _ => GET (LIST USER)
 ]
