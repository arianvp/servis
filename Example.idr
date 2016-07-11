module Example
import Servis

-- universe of queryparams
data QueryParamU = QINT | QSTRING

-- universe of path captures
data CaptureU = CINT | CSTRING

-- universe of response types
data RespU = USER | LIST RespU


ApiUniverse () where
  el () = ()

record User where
  constructor MkUser
  userId : Int
  username : String

ApiUniverse RespU where
  el USER = Example.User
  el (LIST u) = List (el u)

ToResponse RespU where
  toResponse USER v =  ("MkUser " ++ username v ++ " " ++  (the String (cast (userId v))))
  toResponse (LIST u) v = (?renderAList)

ApiUniverse QueryParamU where
  el QINT = Int
  el QSTIRNG = String

FromQueryParam QueryParamU where
  fromQueryParam QINT str = ?d
  fromQueryParam QSTRING str = str

FromCapture CaptureU where
  fromCapture CINT str = ?d
  fromCapture CSTRING str = str

UserApi : Api CaptureU QueryU () RespU
UserApi = OneOf
  [ Const "user" :> Segment "userId" INT :> Outputs (GET USER)
  , Const "user" :> QueryParam "limit" INT :> Outputs (GET (LIST USER))
  ]


-- very cool: this is a type error:
{-
OneOf
  [ Const "user" :> Segment "userId" Int :> blah
  , Const "user" :> Segment "blahId" String :> blah
  ]

because of the noOverlappingPaths constraint!!! :D
-}





userApi : el UserApi
