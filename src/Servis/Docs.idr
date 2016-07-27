module Serivs.Docs

import Servis.API
%access public export

interface HasDocs u where
  docs : u -> String

HasDocs String where
  docs = id

(HasDocs req, HasDocs res) => HasDocs (Handler req res) where
  docs (GET responseType) =
    "## GET\nResponse type: " ++ docs responseType

  docs (POST requestType responseType) =
    "## POST\nRequest type: "  ++ docs requestType ++ "\n" ++
    "Response type: " ++ docs responseType ++ "\n"

renderParams : HasDocs query => List (String, query) -> String
renderParams = renderParams' . reverse
  where
    renderParams' : List (String, query) -> String
    renderParams' [] = ""
    renderParams' ((a, b) :: []) =
      "?" ++ a ++ "=<"++ docs b ++ ">"
    renderParams' ((a, b) :: xs) =
      renderParams' xs ++ "&" ++ a ++ "=<"++ docs b ++ ">"

renderPath : 
  ( HasDocs capture
  , HasDocs query
  , HasDocs req
  , HasDocs resp) => Path capture query req resp -> String -> String
renderPath (Outputs x) params = 
    params ++ "\n" ++ docs x
renderPath ((Const x) :> y) params =
    "/" ++ x ++ renderPath y params
renderPath ((Capture name type) :> y) params =
  "/<" ++ name ++ ":" ++ docs type ++ ">" ++ renderPath y params
renderPath _ _ = ""

( HasDocs capture
, HasDocs query
, HasDocs req
, HasDocs resp
) => HasDocs (Path capture query req resp) where
  docs path =
    "# " ++ renderPath path (renderParams . getParams $ path) ++ "\n"
    where

      getParams : Path capture query req resp -> List (String, query)
      getParams (Outputs x) = []
      getParams ((Const x) :> y) = getParams y
      getParams ((Capture name type) :> y) = getParams y
      getParams ((QueryParam name type) :> y) = (name, type) :: getParams y

