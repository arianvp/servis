module Servis.API

import Data.Vect

%default total
%access public export

interface Universe u where
  el : u -> Type

data Handler : req -> res -> Type where
  GET : (responseType : res) -> Handler req res
  POST : (requestType : req) -> (responseType : res) -> Handler req res

data PathPart : capture -> query -> Type where
  Const : (path : String) -> PathPart capture query
  Capture : (name : String) -> (type : capture) -> PathPart capture query
  QueryParam : (name : String) -> (type : query) -> PathPart capture query


data Path : capture -> query -> req -> res -> Type where
  Outputs : Handler req res -> Path capture query req res
  (:>) : PathPart capture query -> Path capture query req res -> Path capture query req res

infixr 5 :>

data API : capture -> query -> req -> res -> Type where
  OneOf : (paths : Vect (S n) (Path capture query req res)) -> API capture query req res

infixr 7 ::
