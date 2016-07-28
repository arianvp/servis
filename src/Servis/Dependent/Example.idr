module Servis.Dependent.Example

import Servis.Dependent.Server
import Servis.Dependent.API
import HTTP.URL

import Data.BoundedList

data EmptyU
Universe EmptyU where
  el _ = ()

FromQueryParam EmptyU where
  fromQueryParam _ _ = Nothing

FromCapture EmptyU where
  fromCapture _ _ = Nothing

FromRequest EmptyU where
  fromRequest _ _ = Nothing

ToResponse EmptyU where
  toResponse _ _ = ""

data QueryU : Type where
  NAT : QueryU

Universe QueryU where
  el (NAT) = Nat

FromQueryParam QueryU where
  fromQueryParam NAT str = Just (cast str)


data RespU : Type where
  USER : RespU
  BLIST : Nat -> RespU -> RespU
  LIST : RespU -> RespU


data User = MkUser String

Universe RespU where
  el (LIST type) = List (el type)
  el (BLIST n type) = BoundedList n (el type)
  el (USER) = User

f : String -> String -> String
f a b = a ++ "," ++ b

ToResponse RespU where
  toResponse USER (MkUser name) = name
  toResponse (BLIST n type) list = 
    "[" ++  foldr f "" (map (toResponse type) list) ++ "]"
  toResponse (LIST type) list =
    "[" ++  foldr f "" (map (toResponse type) list) ++ "]"


-- Hey a simple example that works!!! :D
exampleOfDependentPath : Path EmptyU QueryU EmptyU RespU
exampleOfDependentPath = QueryParam "limit" NAT :*> \limit => Outputs (GET (BLIST limit USER))

-- our database of users
users : List User
users = [ MkUser "Wouter Swiestra"
        , MkUser "Arian van Putten"
        , MkUser "Marijn van Putten"
        , MkUser "Foo Bar"
        ]

getUsersLimit : (limit : Nat) -> IO (BoundedList limit User)
getUsersLimit limit = pure . take limit $ users

exampleOfDependentPathHandler : el Example.exampleOfDependentPath
exampleOfDependentPathHandler = getUsersLimit

router : (url : URL) -> (requestBody : Maybe String) -> Maybe (IO String)
router = route exampleOfDependentPath  exampleOfDependentPathHandler


execRouter : (url : URL) -> (requestBody : Maybe String) -> IO ()
execRouter url requestBody =
  case router url requestBody of
       Nothing => return ()
       Just x => x >>= putStrLn
complicatedExample : Path EmptyU QueryU EmptyU RespU
complicatedExample = QueryParam "begin" NAT
        :*> \begin => QueryParam "end" NAT 
        :*> \end => Outputs $ case isLTE begin end of
                                  Yes bLTEe => GET (BLIST ((-) end begin {smaller=bLTEe}) USER)
                                  No _ => GET (BLIST 0 USER)

complicatedExampleHandler : el Example.complicatedExample

{-api : API EmptyU QueryU EmptyU RespU

 -- "/user?begin=0&end=5 => returns a Vect 5 User"
 -- "/user?begin=0&end=0 => returns a List User"
api = OneOf [dependentLel ] 

getUsers : (n : Nat) -> (m : Nat) ->
           (case isLTE begin end of
              Yes bLTEe => IO (BoundedList ((-) end begin {smaller=bLTEe}) User)
              No _      => IO (List User))


handler : el Example.api
handler = [getUsers]-}
