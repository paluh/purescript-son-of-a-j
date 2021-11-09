module SonJ where

import Prelude
import Data.Argonaut (Json, isNull, toArray, toBoolean, toNumber, toObject, toString)
import Data.Array (foldl)
import Data.Int (fromNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Foreign.Object (Object)
import Foreign.Object (lookup) as Object
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Null ∷ Type

foreign import null ∷ Null

instance Eq Null where
  eq _ _ = true

class SonJ a where
  load ∷ Json → Maybe a

instance SonJ Number where
  load = toNumber
else instance SonJ Int where
  load = toNumber >=> Int.fromNumber
else instance SonJ String where
  load = toString
else instance SonJ Json where
  load = Just
else instance SonJ Boolean where
  load = toBoolean
else instance (RowToList row rl, RowListSonJ row rl) ⇒ SonJ (Record row) where
  load = toObject >=> loadObject (RLProxy ∷ RLProxy rl)
else instance (RowToList row rl, RowListSonJ row rl) ⇒ SonJ (Variant row) where
  load = load >=> VariantRep >>> loadVariant (RLProxy ∷ RLProxy rl)
else instance SonJ Null where
  load json =
    if isNull json then
      Just (unsafeCoerce json)
    else
      Nothing
else instance (SonJ a) ⇒ SonJ (Array a) where
  load json = (toArray >=> foldl step (Just null) $ json) *> Just (unsafeCoerce json)
    where
    -- | `(.. >>= \_ → ...)` is a lazy version of `*>`
    step accum elem = accum >>= (\_ → (load elem ∷ Maybe a) *> accum)
else instance (Newtype n a, SonJ a) ⇒ SonJ n where
  load = load >>> map wrap

class RowListSonJ (row ∷ Row Type) (rl ∷ RowList Type) where
  loadObject ∷ RLProxy rl → Object Json → Maybe (Record row)
  loadVariant ∷ RLProxy rl → VariantRep Json → Maybe (Variant row)

instance RowListSonJ row Nil where
  loadObject _ obj = Just (unsafeCoerce obj)
  loadVariant _ _ = Nothing
else instance (IsSymbol s, SonJ a, RowListSonJ row tail) ⇒ RowListSonJ row (Cons s a tail) where
  loadObject _ obj = Object.lookup label obj >>= (load ∷ (Json → Maybe a)) >>= \_ → loadObject tail obj
    where
    label = reflectSymbol (SProxy ∷ SProxy s)

    tail = RLProxy ∷ RLProxy tail
  loadVariant _ vr@(VariantRep v) =
    if v.type == label then
      (load v.value ∷ Maybe a) *> Just (unsafeCoerce v)
    else
      loadVariant tail vr
    where
    label = reflectSymbol (SProxy ∷ SProxy s)

    tail = RLProxy ∷ RLProxy tail

dump ∷ ∀ a. SonJ a ⇒ a → Json
dump = unsafeCoerce

unsafeLoad ∷ ∀ a. SonJ a ⇒ Json → a
unsafeLoad = unsafeCoerce
