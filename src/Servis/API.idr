module Servis.API

import Data.List
import Data.Vect
import Data.HVect
import Data.Vect.Quantifiers
import Data.List.Quantifiers

%default total
%access public export

interface Universe (u : Type) where
  el : u -> Type


||| The empty universe
data EmptyU = VOID
Universe EmptyU where
  el VOID = ()
data Handler : (req : Type)  -> (res : Type) -> Type where
  GET : (responseType : res) -> Handler req res
  POST : (requestType : req) -> (responseType : res) -> Handler req res
  DGET : (responseType : res) -> List Int -> Handler req res

(Universe resp, Universe req) => Universe (Handler req resp) where
  el (GET responseType) = IO (el responseType)
  el (POST requestType responseType) = el requestType -> IO (el responseType)
  el (DGET responseType statuses) = (DPair Int (\n => Elem n statuses), IO (el responseType))


def : Handler EmptyU EmptyU
def = DGET VOID [200,401,404]

handler : el Servis.API.def
handler = (MkDPair 401 (There Here), putStrLn "hey")

||| PathPart is a part of a path.
||| @ capture the universe of captures
||| @ query   the universe of query params
data PathPart : (capture : Type) -> (query : Type) -> Type where
  ||| A constant piece of text
  ||| @ path the path part
  Const : (path : String) -> PathPart capture query
  ||| A capture of a variable name of a specific type
  ||| @ name  the name of the variable
  ||| @ type  the type of the variable
  Capture : (name : String) -> (type : capture) -> PathPart capture query
  ||| A query param of a variable name of a specific type
  ||| @ name  the name of the variable
  ||| @ type  the type of the variable
  QueryParam : (name : String) -> (type : query) -> PathPart capture query

( Universe capture
, Universe query
) => Universe (PathPart capture query) where
  el (Const path) = ()
  el (Capture name type) = el type
  el (QueryParam name type) = el type

||| Describes a Path. A Path consists of path parts followed by a handler
data Path : (capture : Type) -> (query : Type) -> (req : Type) -> (res : Type) -> Type where
  ||| Ends a path with a handler
  ||| @ handler the handler
  Outputs : (handler : Handler req res) -> Path capture query req res
  ||| Conses a pathpart to a path
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


data API : (capture : Type) -> (query : Type) -> (req : Type) -> (res : Type) -> Type where
  OneOf : (paths : Vect (S n) (Path capture query req res)) -> API capture query req res



( Universe capture
, Universe query
, Universe req
, Universe res
) => Universe (API capture query req res) where
  el (OneOf xs) = HVect (map el xs)


withoutQueryParams : Path a b c d -> Path a b c d
withoutQueryParams (Outputs handler) = Outputs handler
withoutQueryParams ((QueryParam name type) :> y) = withoutQueryParams y
withoutQueryParams (a :> y) = a :> withoutQueryParams y

data IsNo : Dec a -> Type where
  ItIsNo : IsNo (No x)

data DisjointPP : PathPart capture query -> PathPart capture query -> Type where
  ConstD : IsNo (decEq str str') -> DisjointPP (Const str) (Const str')

data DisjointPath : Path capture query req res -> Path capture query req res -> Type where
  PBase : DisjointPP a b ->  DisjointPath (a :> p) (b :> p2)
  PBaseOutputs1 : DisjointPath (pp :> p) (Outputs h)
  PBaseOutputs2 : DisjointPath (Outputs h) (pp :> p)
  PStep : DisjointPath a b -> DisjointPath (pp :> a) (pp2 :> b)

infixr 8 +>
(+>) :  (path : Path capture query req res) -> (xs : List (Path capture query req res)) ->
       {auto ok: All (\x => DisjointPath (withoutQueryParams path) (withoutQueryParams x)) xs} ->
       List (Path capture query req res)
(+>) x xs = x::xs


data DisjointAPI : API capture query req res -> Type where
  Base : DisjointAPI (OneOf [x])
  Step : (x : Path capture query req res) -> All (\x' => DisjointPath x x') xs -> DisjointAPI (OneOf (x :: xs))
  
{-data DisjointAPI : List (Path capture query req res) -> Type where
  Base : DisjointAPI []
  Step : (x : Path capture query req res) -> All (DisjointPath x) xs -> DisjointAPI (x::xs)
-}
