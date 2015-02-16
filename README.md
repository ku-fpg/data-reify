# data-reify [![Hackage version](https://img.shields.io/hackage/v/data-reify.svg?style=flat)](http://hackage.haskell.org/package/data-reify) [![Build Status](https://img.shields.io/travis/ku-fpg/data-reify.svg?style=flat)](https://travis-ci.org/ku-fpg/data-reify)

`data-reify` provided the ability to turn recursive structures into explicit graphs. Many (implicitly or explicitly) recursive data structure can be given this ability, via a type class instance. This gives an alternative to using `Ref` for observable sharing.

Observable sharing in general is unsafe, so we use the IO monad to bound this effect, but can be used safely even with `unsafePerformIO` if some simple conditions are met. Typically this package will be used to tie the knot with DSLs that depend of observable sharing, like Lava.

Providing an instance for `MuRef` is the mechanism for allowing a structure to be reified into a graph, and several examples of this are provided.

History: Version 0.1 used unsafe pointer compares. Version 0.2 of `data-reify` used StableNames, and was much faster. Version 0.3 provided two versions of `MuRef`, the mono-typed version, for trees of a single type, and the dynamic-typed version, for trees of different types. Version 0.4 used `Int` as a synonym for `Unique` rather than `Data.Unique` for node ids, by popular demand. Version 0.5 merged the mono-typed and dynamic version again, by using `DynStableName`, an unphantomized version of `StableName`.