module Servis.API

import Data.Vect
import Data.HVect
import Data.List
import Data.List.Quantifiers

%default total
%access public export

interface Universe u where
  el : u -> Type

data Handler : req -> res -> Type where
  GET : (responseType : res) -> Handler req res
  POST : (requestType : req) -> (responseType : res) -> Handler req res

injGET : (GET x = GET y) -> x = y
injGET Refl = Refl

injPOSTReq : (POST requestType1 responseType1 = POST requestType2 responseType2) -> requestType1 = requestType2
injPOSTReq Refl = Refl

injPOSTResp : (POST requestType1 responseType1 = POST requestType2 responseType2) -> responseType1 = responseType2
injPOSTResp Refl = Refl

getNotPost : (GET responseType = POST requestType responseType1) -> Void
getNotPost Refl impossible

cong_POST : (prf2 : responseType1 = responseType2) -> (prf1 : requestType1 = requestType2) -> POST requestType1 responseType1 = POST requestType2 responseType2
cong_POST Refl Refl = Refl

(DecEq req, DecEq res) => DecEq (Handler req res) where
  decEq (GET responseType1) (GET responseType2) =
    (case decEq responseType1 responseType2 of
          (Yes prf) => Yes (cong prf)
          (No contra) => No ((\h => contra (injGET h))))
  decEq (POST requestType1 responseType1) (POST requestType2 responseType2) =
    (case decEq requestType1 requestType2 of
          (Yes prf1) => (case decEq responseType1 responseType2 of
                             (Yes prf2) => Yes (cong_POST prf2 prf1)
                             (No contra) => No (\h => contra (injPOSTResp h)))
          (No contra) => No (\h => contra (injPOSTReq h)))
  decEq (GET _) (POST _ _) = No getNotPost
  decEq (POST _ _) (GET _) = No (negEqSym getNotPost)


(Universe resp, Universe req) => Universe (Handler req resp) where
  el (GET responseType) = IO (el responseType)
  el (POST requestType responseType) = el requestType -> IO (el responseType)

data PathPart : capture -> query -> Type where
  Const : (path : String) -> PathPart capture query
  Capture : (name : String) -> (type : capture) -> PathPart capture query
  QueryParam : (name : String) -> (type : query) -> PathPart capture query

( Universe capture
, Universe query
) => Universe (PathPart capture query) where
  el (Const path) = ()
  el (Capture name type) = el type
  el (QueryParam name type) = el type

data Path : capture -> query -> req -> res -> Type where
  Outputs : Handler req res -> Path capture query req res
  (:>) : PathPart capture query -> Path capture query req res -> Path capture query req res
infixr 5 :>

( Universe capture
, Universe query
, Universe req
, Universe resp
) => Universe (Path capture query req resp) where
  -- special case because we don't like () in our functions
  el (Const path :> right) =  el right
  el (pathPart :> right) = el pathPart -> el right
  el (Outputs handler) = el handler



data API : capture -> query -> req -> res -> Type where
  OneOf : (paths : Vect (S n) (Path capture query req res)) -> API capture query req res

( Universe capture
, Universe query
, Universe req
, Universe res
) => Universe (API capture query req res) where
  el (OneOf xs) = HVect (map el xs)


data DisjointPP : PathPart capture query -> PathPart capture query -> Type where
  ConstD : Not (str = str') -> DisjointPP (Const str) (Const str')

data DisjointPath : Path capture query req res -> Path capture query req res -> Type where
  OutputsD : Not (handler1 = handler2) -> DisjointPath (Outputs handler1) (Outputs handler2)
  PathBase : DisjointPP pp1 pp2 -> DisjointPath (pp1 :> p1) (pp2 :> p2)
  PathStep : DisjointPath p1 p2 -> DisjointPath (p :> p1) (p :> p2)


data DisjointAPI : List (Path capture query req res) -> Type where
  Base : DisjointAPI []
  Step : (x : Path capture query req res) -> All (DisjointPath x) xs -> DisjointAPI (x::xs)


checkIt : (list : List (Path capture query req res)) -> {auto ok: DisjointAPI list } -> List (Path capture query req res)
checkIt a = a


