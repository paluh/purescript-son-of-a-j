module SonJ where

import Data.Argonaut (Json)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Variant (Variant)
import Prelude (class Eq)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Null ∷ Type

foreign import null ∷ Null

instance eqNull ∷ Eq Null where
  eq _ _ = true

class SonJ a

instance numberSonJ ∷ SonJ Number

else instance intSonJ ∷ SonJ Int

else instance stringSonJ ∷ SonJ String

else instance booleanSonJ ∷ SonJ Boolean

else instance nullableSonJ ∷ SonJ a ⇒ SonJ (Nullable a)

else instance recordSonJ ∷ (RowToList row rl, RowListSonJ rl) ⇒ SonJ (Record row)

else instance variantSonJ ∷ (RowToList row rl, RowListSonJ rl) ⇒ SonJ (Variant row)

else instance sonJNull ∷ SonJ Null

else instance newtypeSonJ ∷ (Newtype n a, SonJ a) ⇒ SonJ n

class RowListSonJ (rl ∷ RowList)

instance nilRowListSonJ ∷ RowListSonJ Nil

else instance consRowListSonJ ∷ (SonJ a, RowListSonJ tail) ⇒ RowListSonJ (Cons s a tail)

dump ∷ ∀ a. SonJ a ⇒ a → Json
dump = unsafeCoerce

unsafeLoad ∷ ∀ a. SonJ a ⇒ Json → a
unsafeLoad = unsafeCoerce
