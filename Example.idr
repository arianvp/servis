module Example
import Servis

data UserApiUniverse
  = USER
  | INT
  | STRING
  | LIST UserApiUniverse

record User where
  constructor MkUser
  userId : Int
  username : String

ApiUniverse UserApiUniverse where
  el USER = Example.User
  el INT = Int
  el STRING = String
  el (LIST u) = List (el u)


ToResponse UserApiUniverse where
  toResponse USER v = Just ("MkUser " ++ username v ++ " " ++  (the String (cast (userId v))))
  toResponse _ _ = Nothing


UserApi : Api UserApiUniverse
UserApi = OneOf
  [ Segment "yo" INT :> (Endpoint (Outputs  (GET USER)))
  ]




userApi : el UserApi
