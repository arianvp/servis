module Servis.Dependent.Example

import Servis.Dependent.API

data EmptyU

data QueryU : Type where
  NAT : QueryU

Universe QueryU where
  el (NAT) = Nat

data RespU : Type where
  USER : RespU
  VECT : Nat -> RespU -> RespU
  LIST : RespU -> RespU

data User = MkUser String

Universe RespU where
  el (VECT n type) = Vect n (el type)
  el (LIST type) = List (el type)
  el (USER) = User



api : API EmptyU QueryU EmptyU RespU

 -- "/user?begin=0&end=5 => returns a Vect 5 User"
 -- "/user?begin=0&end=0 => returns a List User"
api = OneOf
 [  Const "user" 
 :> QueryParam "begin" NAT
 :*> \begin => QueryParam "end" NAT 
 :*> \end => Outputs $ case isLTE begin end of
                          Yes bLTEe => GET (VECT ((-) end begin {smaller=bLTEe}) USER)
                          No _ => GET (LIST USER)
 ]
