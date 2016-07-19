module HTTP.URL
%access public export

record URL where
  constructor MkURL
  pathParts : List String
  params : List (String, String)
