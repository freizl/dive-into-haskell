what are pros and cons between?

```{.haskell}
data FT = forall a. (Show (FactorSettings a), IsFactor a) => FT
  { factor :: a
  , settings :: FactorSettings a
  }
```

```{.haskell}
data FT a = forall a. (Show (FactorSettings a)) => FT
  { factor :: a
  , settings :: FactorSettings a
  }
```
