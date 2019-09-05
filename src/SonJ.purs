module SonJ where

import Prelude

import Data.Argonaut (Json, isNull, toArray, toBoolean, toNumber, toObject, toString)
import Data.Array (foldl)
import Data.Foldable (fold, foldMap)
import Data.Int (fromNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Foreign.Object (Object)
import Foreign.Object (lookup) as Object
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Null ∷ Type

foreign import null ∷ Null

instance eqNull ∷ Eq Null where
  eq _ _ = true

class SonJ a where
  load ∷ Json → Maybe a

instance numberSonJ ∷ SonJ Number where
  load = toNumber

else instance intSonJ ∷ SonJ Int where
  load = toNumber >=> Int.fromNumber

else instance stringSonJ ∷ SonJ String where
  load = toString

else instance jsonSonJ ∷ SonJ Json where
  load = Just

else instance booleanSonJ ∷ SonJ Boolean where
  load = toBoolean

else instance recordSonJ ∷ (RowToList row rl, RowListSonJ row rl) ⇒ SonJ (Record row) where
  load = toObject >=> loadObject (RLProxy ∷ RLProxy rl)

else instance variantSonJ ∷ (RowToList row rl, RowListSonJ row rl) ⇒ SonJ (Variant row) where
  load = load >=> VariantRep >>> loadVariant (RLProxy ∷ RLProxy rl)

else instance sonJNull ∷ SonJ Null where
  load json = if isNull json
    then Just (unsafeCoerce json)
    else Nothing

else instance arraySonJ ∷ (SonJ a) ⇒ SonJ (Array a) where
  load json = (toArray >=> foldl step (Just null) $ json) *> Just (unsafeCoerce json)
    where
      step accum elem = case accum of
        Nothing → Nothing
        otherwise → (load elem ∷ Maybe a) *> accum

else instance newtypeSonJ ∷ (Newtype n a, SonJ a) ⇒ SonJ n where
  load = load >>> map wrap

class RowListSonJ (row ∷ # Type) (rl ∷ RowList) where
  loadObject ∷ RLProxy rl → Object Json → Maybe (Record row)
  loadVariant ∷ RLProxy rl → VariantRep Json → Maybe (Variant row)

instance nilRowListSonJ ∷ RowListSonJ row Nil where
  loadObject _ obj = Just (unsafeCoerce obj)
  loadVariant _ _ = Nothing

else instance consRowListSonJ ∷ (IsSymbol s, SonJ a, RowListSonJ row tail) ⇒ RowListSonJ row (Cons s a tail) where
  loadObject _ obj = Object.lookup label obj *> loadObject tail obj
    where
      label = reflectSymbol (SProxy ∷ SProxy s)
      tail = RLProxy ∷ RLProxy tail
  loadVariant _ vr@(VariantRep v) = if v.type == label
    then (load v.value ∷ Maybe a) *> Just (unsafeCoerce v)
    else loadVariant tail vr
    where
      label = reflectSymbol (SProxy ∷ SProxy s)
      tail = RLProxy ∷ RLProxy tail

dump ∷ ∀ a. SonJ a ⇒ a → Json
dump = unsafeCoerce

unsafeLoad ∷ ∀ a. SonJ a ⇒ Json → a
unsafeLoad = unsafeCoerce

