# Type system
Here is a list of valid types for expressions:
## Monotypes
* `()` (unit value)
* `bool`
* `int`
* `string`
* SomeIdent, defined as a record type via `struct SomeIdent { ...}` definition
## Polytypes
* `Fn([T]) -> T`, where `T` is any valid type, `[T]` is a sequence thereof.