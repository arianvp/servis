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

||| A Proof that the GET dtor is injective over its requestType
injGET : (GET x = GET y) -> x = y
injGET Refl = Refl

||| A proof that the POST dtor is injective over its requestType
injPOSTReq : (POST requestType1 responseType1 = POST requestType2 responseType2) -> requestType1 = requestType2
injPOSTReq Refl = Refl

||| A Proof that the POST dtor is injective over its responseType
injPOSTResp : (POST requestType1 responseType1 = POST requestType2 responseType2) -> responseType1 = responseType2
injPOSTResp Refl = Refl

||| A Proof that a GET is not a POST
getNotPost : (GET responseType = POST requestType responseType1) -> Void
getNotPost Refl impossible

||| A proof that GET is congruent
congGET : (prf : responseType1 = responseType2) -> GET responseType1 = GET responseType2
congGET = cong

||| A proof POST is congruent
congPOST : (prf2 : responseType1 = responseType2) -> (prf1 : requestType1 = requestType2) -> POST requestType1 responseType1 = POST requestType2 responseType2
congPOST Refl Refl = Refl

(DecEq req, DecEq res) => DecEq (Handler req res) where
  decEq (GET responseType1) (GET responseType2) =
    (case decEq responseType1 responseType2 of
          (Yes prf) => Yes (cong prf)
          (No contra) => No ((\h => contra (injGET h))))
  decEq (POST requestType1 responseType1) (POST requestType2 responseType2) =
    (case decEq requestType1 requestType2 of
          (Yes prf1) => (case decEq responseType1 responseType2 of
                             (Yes prf2) => Yes (congPOST prf2 prf1)
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

injConst : (Const x = Const y) -> x = y
injConst Refl = Refl

constNotCapture : (Const path = Capture name type) -> Void
constNotCapture Refl impossible

constNotQueryParam : (Const path = QueryParam name type) -> Void
constNotQueryParam Refl impossible

captureNotQueryParam : (Capture name type = QueryParam x y) -> Void
captureNotQueryParam Refl impossible

congCapture : (prf2 : type1 = type2) -> (prf1 : name1 = name2) -> Capture name1 type1 = Capture name2 type2
congCapture Refl Refl = Refl

injCaptureType : (h : Capture name1 type1 = Capture name2 type2) -> type1 = type2
injCaptureType Refl = Refl

injCaptureName : (h : Capture name1 type1 = Capture name2 type2) -> name1 = name2
injCaptureName Refl = Refl

congQueryParam : (prf2 : type1 = type2) -> (prf1 : name1 = name2) -> QueryParam name1 type1 = QueryParam name2 type2
congQueryParam Refl Refl = Refl

injQueryParamType : (h : QueryParam name1 type1 = QueryParam name2 type2) -> type1 = type2
injQueryParamType Refl = Refl

injQueryParamName : (h : QueryParam name1 type1 = QueryParam name2 type2) -> name1 = name2
injQueryParamName Refl = Refl


(DecEq capture, DecEq query) => DecEq (PathPart capture query) where
  decEq (Const x) (Const y) =
    case decEq x y of
         (Yes prf) => Yes (cong prf)
         (No contra) => No (\h => contra (injConst h))
  decEq (Const path) (Capture name type) = No constNotCapture
  decEq (Const path) (QueryParam name type) = No constNotQueryParam
  decEq (Capture name type) (Const path) = No (negEqSym constNotCapture)
  decEq (Capture name1 type1) (Capture name2 type2) =
    case decEq name1 name2 of
         (Yes prf1) => case decEq type1 type2 of
                            (Yes prf2) => Yes ((congCapture prf2 prf1))
                            (No contra) => No (\h => contra (injCaptureType h))
         (No contra) => No (\h => contra (injCaptureName h))
  decEq (Capture name type) (QueryParam x y) = No captureNotQueryParam
  decEq (QueryParam name type) (Const path) = No (negEqSym constNotQueryParam)
  decEq (QueryParam name type) (Capture x y) = No (negEqSym captureNotQueryParam)
  decEq (QueryParam name1 type1) (QueryParam name2 type2) =
    case decEq name1 name2 of
         (Yes prf1) => case decEq type1 type2 of
                            (Yes prf2) => Yes ((congQueryParam prf2 prf1))
                            (No contra) => No (\h => contra (injQueryParamType h))
         (No contra) => No (\h => contra (injQueryParamName h))

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

injOutputs : Outputs x = Outputs y -> x = y
injOutputs Refl = Refl



outputsNotSeq : (Outputs x = (y :> z)) -> Void
outputsNotSeq Refl impossible

congSeq : (y = w) -> (x = z) -> (x :> y) = (z :> w)
congSeq Refl Refl = Refl

injSeqRHS : (x :> y) = (z :> w) -> y = w
injSeqRHS Refl = Refl

injSeqLHS : (x :> y) = (z :> w) -> x = z
injSeqLHS Refl = Refl

( DecEq capture
, DecEq query
, DecEq req
, DecEq res
) => DecEq (Path capture query req res) where
  decEq (Outputs x) (Outputs y) =
    case decEq x y of
         (Yes prf) => Yes (cong prf)
         (No contra) => No (\h => contra (injOutputs h))
  decEq (Outputs x) (y :> z) = No outputsNotSeq
  decEq (x :> y) (Outputs z) = No (negEqSym outputsNotSeq)
  decEq (x :> y) (z :> w) =
    case decEq x z of
         (Yes prf1) =>
           case decEq y w of
                (Yes prf2) => Yes (congSeq prf2 prf1)
                (No contra) => No (\h => contra (injSeqRHS h))
         (No contra) => No (\h => contra (injSeqLHS h))

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

{-congOneOfBase: (x = y) -> OneOf [x] = OneOf [y]
congOneOfBase Refl = Refl

( DecEq capture
, DecEq query
, DecEq req
, DecEq res
) => DecEq (API capture query req res) where
  decEq (OneOf (x :: [])) (OneOf (y :: [])) =
    -- base step. can only decide equality if we can decide x = y
    case decEq x y of
         (Yes prf) => Yes (congOneOfBase prf)
         (No contra) => No ?a_4
  decEq (OneOf (x :: xs)) (OneOf (y :: ys)) =
    case decEq x y of
         (Yes prf) => (case decEq (OneOf xs) (OneOf ys of
                            case_val => ?a_23)
         (No contra) => No ?a_3

-}
data DisjointPP : PathPart capture query -> PathPart capture query -> Type where
  ConstD : Not (str = str') -> DisjointPP (Const str) (Const str')

data DisjointPath : Path capture query req res -> Path capture query req res -> Type where
  OutputsD : Not (handler1 = handler2) -> DisjointPath (Outputs handler1) (Outputs handler2)
  PathBase : DisjointPP pp1 pp2 -> DisjointPath (pp1 :> p1) (pp2 :> p2)
  PathStep : DisjointPath p1 p2 -> DisjointPath (p :> p1) (p :> p2)


data DisjointAPI : List (Path capture query req res) -> Type where
  Base : DisjointAPI []
  Step : (x : Path capture query req res) -> All (DisjointPath x) xs -> DisjointAPI (x::xs)


checkItTo : (path1 : Path capture query req res) -> (path2 : Path capture query req res) -> {auto ok: DisjointPath path1 path2} -> Path capture query req res
checkItTo a b = a

checkIt : (list : List (Path capture query req res)) -> {auto ok: DisjointAPI list } -> List (Path capture query req res)
checkIt a = a


