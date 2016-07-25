# Overlapping routes
At my work (Channable) we use flask-restful to build our REST APIs.

Today I was debugging a bug where certain actions on our API wouldn't come through. After a long and boring debugging sessions
I realised the issue was a very trivial mistake in this piece of code:

```python
api.add_resource(ApiCategoriesResource, '/users/<int:user_id>/projects/<int:project_id>/apis/<int:api_id>/categories')
api.add_resource(ApiCategoriesAttributesResource, '/users/<int:user_id>/projects/<int:project_id>/apis/<int:api_id>/categories/<string:category_id>')
api.add_resource(ApiCategoriesDatastreamResource, '/users/<int:user_id>/projects/<int:project_id>/apis/<int:api_id>/categories/<string:category_id>/datastream')
api.add_resource(CategoriesResource, '/users/<int:user_id>/projects/<int:project_id>/<string:channel>/<int:channel_id>/categories')
api.add_resource(CategoriesCopyResource, '/users/<int:user_id>/projects/<int:project_id>/<string:channel>/<int:channel_id>/categories/copy')
api.add_resource(CategoriesSearchResource, '/users/<int:user_id>/projects/<int:project_id>/<string:channel>/<int:channel_id>/categories/search')
api.add_resource(CategoriesQueryResource, '/users/<int:user_id>/projects/<int:project_id>/<string:channel>/<int:channel_id>/categories/query')
api.add_resource(CategoriesGenerateResource, '/users/<int:user_id>/projects/<int:project_id>/<string:channel>/<int:channel_id>/categories/generate')
```

Can you spot it?


It resides in these two definitions:
```python
api.add_resource(ApiCategoriesResource, '/users/<int:user_id>/projects/<int:project_id>/apis/<int:api_id>/categories')
api.add_resource(CategoriesResource, '/users/<int:user_id>/projects/<int:project_id>/<string:channel_type>/<int:channel_id>/categories')
```

if   `channel_type == "apis"` then these two routes overlap! Thus it this request will be handled by whatever the underlying framework chooses!
Which is not very nice. 


Servant suffers from a similar problem:

```
TODO: example here
```




In Servis, we can prove statically, that no route can overlap. If such a definition is made, a compiler error is thrown



# Dependent routing
Sometimes, we want interdependencies in our routes. An example of a very common REST query:

```
/users?limit=100
```

If would be nice if we could verif at compile time that this route never returns more than 100 elements.  In haskell, this is currently not possible, because types can not depend on values, but in idris, we _can_ accomplish this by extending the Servant DSL with Pi-types.
