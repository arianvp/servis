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


