module ExampleProof

import Servis.API
import Data.So
import Data.Vect
import Data.List.Quantifiers

data RespU = USER | BLAH

userNotBlah : (USER = BLAH) -> Void
userNotBlah Refl impossible

Eq RespU where
  USER == USER = True
  BLAH == BLAH = True
  _ == _ = False
DecEq RespU where
  decEq USER USER = Yes Refl
  decEq BLAH BLAH = Yes Refl
  decEq USER BLAH = No userNotBlah
  decEq BLAH USER = No $ negEqSym userNotBlah


pp2 : PathPart EmptyU EmptyU
pp2 = Const "yo"
pp1 : PathPart EmptyU EmptyU
pp1 = Const "yo2"

path1 : Path EmptyU EmptyU EmptyU RespU
path1 = Const "users" :> Capture "name" VOID :> Const "friends" :> Outputs (GET USER)

path2 : Path EmptyU EmptyU EmptyU RespU
path2 = Const "users" :> Const "yo" :> Const "do" :> Outputs (GET USER)


api : API EmptyU EmptyU EmptyU RespU
api = OneOf [path1, path2]

checked : (api : API a b c d) ->
          {auto ok: DisjointAPI api} -> API a b c d
checked api = api

api2' : List (Path EmptyU EmptyU EmptyU RespU)
api2' =  (Capture "name" VOID :> Const "a" :> Outputs (GET USER))
      +> (Capture "name" VOID :> Const "a"
                       :> Outputs (GET (USER)))
      +> Nil
api' : List (Path EmptyU EmptyU EmptyU RespU)
api' =  path1 +> path2 +> Nil

api'' : API EmptyU EmptyU EmptyU RespU
api'' = OneOf (fromList api')


itIsTrue : (pp1 : PathPart EmptyU EmptyU)
      -> (pp2 : PathPart EmptyU EmptyU)
      -> {auto ok : DisjointPP pp1 pp2}
      -> PathPart EmptyU EmptyU
itIsTrue pp1 pp2 = pp2
itIsEq : ( path1 : Path EmptyU EmptyU EmptyU RespU)
      -> ( path2 : Path EmptyU EmptyU EmptyU RespU)
      -> {auto ok: DisjointPath (withoutQueryParams path1) (withoutQueryParams path2)}
      -> Path EmptyU EmptyU EmptyU RespU

itIsEq path1 path2 = path1

pp3 : PathPart EmptyU EmptyU
pp3 = itIsTrue pp1 pp2

path3 : Path EmptyU EmptyU EmptyU RespU
path3 = itIsEq path1 path2




