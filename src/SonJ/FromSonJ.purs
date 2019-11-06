module SonJ.FromSonJ where

import Prelude

import Data.Argonaut (Json)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), from, to)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Data.Variant (expand, inj, on) as Variant
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, kind RowList)
import SonJ (class SonJ)
import SonJ.FromSonJ.Builder (Builder(..), Unique(..), build, mapArray, modifyRecord, modifyVariant)
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

class FromSonJ a b | b → a where
  load ∷ Builder a b

instance numberFromSonJ ∷ FromSonJ (Unique Number) (Unique Number) where
  load = Coerce

else instance intFromSonJ ∷ FromSonJ (Unique Int) (Unique Int) where
  load = Coerce

else instance stringFromSonJ ∷ FromSonJ (Unique String) (Unique String) where
  load = Coerce

else instance jsonFromSonJ ∷ FromSonJ (Unique Json) (Unique Json) where
  load = Coerce

else instance booleanFromSonJ ∷ FromSonJ (Unique Boolean) (Unique Boolean) where
  load = Coerce

else instance arrayFromSonJ ∷ (FromSonJ (Unique a) (Unique b)) ⇒ FromSonJ (Unique (Array a)) (Unique (Array b)) where
  load = mapArray load

else instance recordFromSonJ ∷ (RowToList row rl, RowListFromSonJ rl row row') ⇒ FromSonJ (Unique (Record row)) (Unique (Record row')) where
  load = loadObject (RLProxy ∷ RLProxy rl)

else instance variantFromSonJ ∷ (RowToList row rl, RowListFromSonJ rl row row') ⇒ FromSonJ (Unique (Variant row)) (Unique (Variant row'))where
  load = loadVariant (RLProxy ∷ RLProxy rl)

else instance genericFromSonJ ∷ (Generic b rep, GenericFromSonJ a rep b) ⇒ FromSonJ (Unique a) (Unique b) where
  load = (Coerce ∷ Builder (Unique a) a) >>> loadGeneric >>> (Coerce ∷ Builder b (Unique b))

class GenericFromSonJ a rep b | b → a, b → rep where
  loadGeneric ∷ Builder a b

instance genericFromSonJSinlgeConstructor ∷ (Newtype b a, SonJ a) ⇒ GenericFromSonJ a (Constructor name (Argument a)) b where
  loadGeneric = Coerce

-- else instance genericFromSonJConstructor ∷ GenericFromSonJ a (Sum a b) where
--

class RowListFromSonJ (rl ∷ RowList) (row ∷ # Type) (row' ∷ # Type) | rl → row, rl → row' where
  loadObject ∷ RLProxy rl → Builder (Unique (Record row)) (Unique (Record row'))
  loadVariant ∷ RLProxy rl → Builder (Unique (Variant row)) (Unique (Variant row'))

instance nilRowListFromSonJ ∷ RowListFromSonJ RowList.Nil () () where
  loadObject _ = Coerce
  loadVariant _ = Coerce

else instance consRowListFromSonJ
  ∷ ( IsSymbol s
    , FromSonJ (Unique a) (Unique b)
    , Row.Cons s a t row
    , Row.Cons s b t row'
    , RowListFromSonJ tail row' row''
    )
  ⇒ RowListFromSonJ (RowList.Cons s a tail) row row''
  where
  loadObject _ = modifyRecord (SProxy ∷ SProxy s) (load ∷ Builder (Unique a) (Unique b)) >>> (loadObject (RLProxy ∷ RLProxy tail))

  loadVariant _ = builder >>> loadVariant (RLProxy ∷ RLProxy tail)
    where
      builder = modifyVariant (SProxy ∷ SProxy s) (load ∷ Builder (Unique a) (Unique b))

-- data X = X Int String
-- 
-- derive instance genericX ∷ Generic X _
-- 
-- class Generic a rep ⇐ Fake a rep | a → rep
-- 
-- instance fakeX ∷ Fake X (Constructor "X" (Product (Argument Int) (Argument String)))

-- x ∷ Fake X ⇒ Generic X (Constructor "Y" NoArguments) ⇒ X → (Constructor "Y" NoArguments)
-- x = fake -- (Constructor NoArguments ∷ Constructor "Y" NoArguments)

-- -- data Builder a b
-- --   = Mutator (a → b)
-- --   | Coerce (a → b)
-- -- 
-- -- unsafe ∷ Builder a b → (a → b)
-- -- unsafe (Coerce f) = f
-- -- unsafe (Mutator f) = f
-- -- 
-- -- instance semigroupoidBuilder ∷ Semigroupoid Builder where
-- --   (Mutator b2c) a2b = Mutator $ unsafe a2b >>> b2c
-- --   b2c (Mutator a2b) = Mutator $ a2b >>> unsafe b2c
-- --   b2c a2b = Coerce (unsafe a2b) (unsafe b2c)
-- 
-- -- mapArray ∷ ∀ i a. Builder a b → Builder (Array a) (Array b)
-- 
-- -- modifyRec ∷ ∀ i a. (RowCons l a t r) ⇒ (RowCons l b t r') ⇒ Builder a b → Builder r r'
-- 
