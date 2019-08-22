# GADT Parser

simple but complete example on how to parse GADT-style expressions

```haskell
data Expr (res :: Type) where
  IntE    :: Int -> Expr Int
  AddE    :: Expr Int -> Expr Int -> Expr Int
  BoolE   :: Bool -> Expr Bool
  IsNullE :: Expr Int -> Expr Bool
  IfE     :: Expr Bool -> Expr res -> Expr res -> Expr res
```

using [Megaparsec](https://www.stackage.org/haddock/lts-14.2/megaparsec-7.0.5/Text-Megaparsec.html)

see the [GADT-Module](./src/lib/GADT) for details.

There is also a simple [ADT implementation](./src/lib/ADT) for comparision.

## Build / Run / Test

This repository uses [hpack](https://hackage.haskell.org/package/hpack) and is tested with [stack](https://docs.haskellstack.org/en/stable/README/).

- Clone this repository
- `cd` into the project folder
- run 
  - `stack build` to build
  - `stack run` to run a simple "REPL" for the DSL
  - `stack test` to run the parser specs and doctests
  
I tried to document the modules so `stack haddock --open` should give you a nice
looking documentation.
