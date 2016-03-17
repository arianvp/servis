Servant is a library that allows us to describe an HTTP API with a type-level DSL,
and from this DSL lets us deduce the types of the handlers for each endpoint.

Also it allows us to deduce implementations for clients and generate documentation from these types.

There are a few shortcomings that I have identified in working with servant.

Though servant does a lot of type-level magic, the actual DSL (that is in the type level) isn't typed. We can write servant expressions that don't make sense without the compiler complaining. This sounds bad but it's also a blessing. Due to the untyped-ness we can extend the DSL without modifying the core library. People can write new combinators easily and use them in your project.


Another problem with servant is that it is happy-path oriented. We describe neatly in this DSL how our endpoints respond if everything goes well, but there isn't really a way to describe failure in the DSL.  As especially in HTTP, errors are really part of the domain and your business-logic, this is a serious shortcoming. 

>alpounet | well, jkarni did some research into making the descriptions "exhaustive", as in they specify all error cases etc
>alpounet | but this was indeed quite heavy on the eyes


This leads us to another shortcoming. The original authors did explore encoding error cases in the type-level, but this made the API descriptions cluttered.   The type-level API looks a bit messy at times and when APIs grow big, reading them could be a burden.



# Ideas
To make the DSL easier to use, we could make a variant of dependently-typed printf.
We can write our routes as strings. Parse them, and return the corresponding handler type

We could take a standard like swagger for this.

For example: https://www.itu.dk/people/drc/pubs/dependent-type-providers.pdf 

```
-- this could be a typeclass function
HandlerType : handlerDescription -> Type

handler (h : handlerDescription) -> (acc : String) -> HandlerType h

toHandler : ( xs : List Char) -> handlerDescription

handler : (route : String) -> HandlerType (toHandler (unpack route))
handler route = handler _ ""



```
