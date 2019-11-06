module SonJ.FromSonJ.Builder where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (jsonParser) as Argonaut
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant (on) as Variant
import Data.Variant.Internal (VariantRep(..))
import Prelude (unit) as Prelude
import Prim.Row (class Cons) as Row
import Record.Builder (Builder) as Record.Builder
import SonJ (class SonJ)
import SonJ (load) as SonJ
import Type.Prelude (class IsSymbol, SProxy, reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | Unique value doesn't contain any shared refrences to mutable
-- | javascript objects (it can be for example a result of `Argounaut.jsonParser`).
-- | This allows us to make safe, controlled mutations of this value.
-- |
-- | Currently this type is not really usable outside of the scope.
-- | It lacks constructors and something like `Clone` class instances
-- | (from `rightfold/purescript-substructal`).
newtype Unique a = Unique a

unUnique ∷ ∀ a. Unique a → a
unUnique (Unique a) = a

int ∷ Number → Unique Number
int = Unique

number ∷ Number → Unique Number
number = Unique

string ∷ String → Unique String
string = Unique

unit ∷ Unique Unit
unit = unsafeCoerce Prelude.unit

array ∷ ∀ a b. Builder b (Unique (Array a))
array = Mutator (const $ Unique [])

-- | Rename `Mutator` to `Transform`
data Builder a b = Mutator (a → b) | Coerce
type Builder' a = Builder a a

instance semigroupoidBuilder ∷ Semigroupoid Builder where
  compose (Mutator b2c) (Mutator a2b) = Mutator $ a2b >>> b2c
  compose (Mutator m) _ = Mutator $ unsafeCoerce m
  compose _ (Mutator m) = Mutator $ unsafeCoerce m
  compose Coerce Coerce = Coerce

instance categoryBuilder ∷ Category Builder where
  identity = Coerce

foreign import unsafeMapArray ∷ ∀ a b. (Unique a → Unique b) → Unique (Array a) → Unique (Array b)
foreign import unsafeModifyRecord ∷ ∀ a b r1 r2. String → (Unique a → Unique b) → Unique (Record r1) → Unique (Record r2)

mapArray ∷ ∀ a b. Builder (Unique a) (Unique b) → Builder (Unique (Array a)) (Unique (Array b))
mapArray Coerce = Coerce
mapArray (Mutator f) = Mutator $ unsafeMapArray f

mapMaybe ∷ ∀ a b. Builder (Unique a) (Unique b) → Builder (Unique (Maybe a)) (Unique (Maybe b))
mapMaybe Coerce = Coerce
mapMaybe (Mutator f) = Mutator $ case _ of
  (Unique Nothing) → Unique Nothing
  (Unique (Just a)) → let Unique b = f (Unique a) in Unique (Just b)

modifyRecord
  ∷ ∀ a b l r r' t
  . IsSymbol l
  ⇒ Row.Cons l a t r
  ⇒ Row.Cons l b t r'
  ⇒ SProxy l
  → Builder (Unique a) (Unique b)
  → Builder (Unique (Record r)) (Unique (Record r'))
modifyRecord l Coerce = Coerce
modifyRecord l (Mutator f) = Mutator $ unsafeModifyRecord (reflectSymbol l) f

modifyVariant
  ∷ ∀ a b l r r' t
  . IsSymbol l
  ⇒ Row.Cons l a t r
  ⇒ Row.Cons l b t r'
  ⇒ SProxy l
  → Builder (Unique a) (Unique b)
  → Builder (Unique (Variant r)) (Unique (Variant r'))
modifyVariant l Coerce = Coerce
modifyVariant l (Mutator f) =
  let
    label = reflectSymbol l

    coerceV ∷ Variant r → VariantRep a
    coerceV = unsafeCoerce

    coerceVR ∷ VariantRep b → Variant r'
    coerceVR = unsafeCoerce

    -- | `Variant.expand` is too general with its `Union` constraint here
    expand' ∷ Row.Cons l b t r' ⇒ Variant t → Variant r'
    expand' = unsafeCoerce

    f' (Unique v) = Unique $ Variant.on
      l
      -- | Below we have mutation which is an equivalent of this:
      -- | (\a → Variant.inj label (let Unique b = (f (Unique a)) in b))
      ( \a →
          let
            (VariantRep vr) = coerceV v
          in
            coerceVR (VariantRep (let Unique v' = unsafeModifyRecord "value" f (Unique vr) in v'))
      )
      expand'
      v
  in
    Mutator f'

fromRecordBuilder ∷ ∀ a b. Record.Builder.Builder a b → Builder a b
fromRecordBuilder b = Mutator (unsafeCoerce b)

build ∷ ∀ a b. a → Builder a b → b
build a (Mutator f) = f a
build a Coerce = unsafeCoerce a

unsafeUnique ∷ ∀ a. Builder a (Unique a)
unsafeUnique = Coerce

sonJ ∷ ∀ a. SonJ a ⇒ Builder String (Unique (Maybe a))
sonJ = Mutator (Argonaut.jsonParser >>> hush >=> SonJ.load) >>> unsafeUnique

jsonParser ∷ Builder String (Unique (Maybe Json))
jsonParser = Mutator (unsafeCoerce Argonaut.jsonParser)

