## 0.6.4 [2024.10.27]
* Drop support for pre-8.0 versions of GHC.

## 0.6.3 [2020.10.12]
* Fix a bug introduced in `data-reify-0.6.2` where `reifyGraph` could return
  `Graph`s with duplicate key-value pairs.

## 0.6.2 [2020.09.30]
* Use `HashMap`s and `IntSet`s internally for slightly better performance.

## 0.6.1
* Fix warnings in GHC 7.10.

## 0.5
* Merge the mono-typed and dynamic version again, by using 'DynStableName', an
  unphantomized version of StableName.

## 0.4
* Use 'Int' as a synonym for 'Unique' rather than 'Data.Unique' for node ids,
  by popular demand.

## 0.3
* Provide two versions of 'MuRef', the mono-typed version, for trees of a
  single type, and the dynamic-typed version, for trees of different types.

## 0.2
* Use 'StableName's, making `data-reify` much faster.

## 0.1
* Use unsafe pointer compares.
