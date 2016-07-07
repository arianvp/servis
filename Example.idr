module Example
import Servis


data UserApiUniverse
  = USER
  | INT
  | STRING
  | LIST UserApiUniverse

data User = MkUser Int String

ApiUniverse UserApiUniverse where
  el USER = Example.User
  el INT = Int
  el STRING = String
  el (LIST u) = List (el u)


UserApi : Api UserApiUniverse
UserApi = OneOf
  [ Segment "yo" INT :> (Endpoint (Outputs  (GET USER)))
  ]


userApi : el UserApi



