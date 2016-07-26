Servant is a type-level DSL for describing Web APIs.
Given an API-type, it can be interpreted in several ways:
as a server that can process requests, interprets them and 
dispatches them; as a client that can correctly query endpoints of the API;
as systematic documentation for the API. [0]

An example of a minimalistic "echo" service in Servant would be defined as follows [0]:
```
type Echo =  "echo"
          :> ReqBody '[PlainText] String
          :> Post    '[PlainText] String
```

The type describes an API that accepts a POST request with a PlainText String requestBody,
and then responds with a PlainText String. From the name we can deduce that the response
will probably be the same as the content of the request body.

The simplest interpretation for an API type is the one for generating documentation. This is
of course also very convenient for the end user.

We have some function
```
docs :: HasDocs api => Proxy api -> API
```

that reifies the type-level api description into a term-level abstract datatype called API.
We can then pattern match and traverse over the API datatype to generate documentation.

The datatype `Proxy api` is a type-witness. It is defined as `data Proxy a = Proxy`.  Because the datatype `API` is not parameterized, it gives no clue to the compiler which `HasDocs` instance API uses. This is why we pass along a witness of `a` to hint the compiler which HasDocs isntance to select.

Servant provides a convenient function

```
markdown :: API -> String
```
that generates documentation in the markdown format given a reified API.


