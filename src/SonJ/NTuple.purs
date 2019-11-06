module SonJ.NTuple where

import Prelude

import Data.Array (cons) as Array
import Data.Array (unsafeIndex)
import Data.Typelevel.Num (class Pos, class Pred, class Succ, D1, d1, toInt')
import Data.Typelevel.Num.Sets (toInt)
import Partial.Unsafe (unsafePartial)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data NTuple ∷ Type → Type → Type

infixr 6 type NTuple as :*:

tuple ∷ ∀ a b. a → b → a :*: b
tuple a b = unsafeCoerce [a, unsafeCoerce b]

cons ∷ ∀ a b c. a → b :*: c → a :*: b :*: c
cons fst tail = fromArr (Array.cons fst (toArr tail))
  where
    toArr ∷ ∀ x y z. NTuple x y → Array z
    toArr = unsafeCoerce

    fromArr ∷ ∀ x y z. Array x → NTuple y z
    fromArr = unsafeCoerce

class Pos index ⇐ Nth index tuple value | tuple index → value

instance nthAt ∷ Nth D1 (a :*: t) a
else instance nthLast ∷ Nth D1 a a
else instance nthPrev ∷ (Pred n p,  Nth p t a) ⇒ Nth n (x :*: t) a

nth ∷ ∀ a n t. Nth n t a ⇒ Proxy n → t → a
nth n t = unsafePartial $ unsafeIndex (unsafeCoerce t ∷ Array a) (toInt' n)

instance eqNthTuple ∷ (NthEq D1 (h :*: t) (h :*: t)) ⇒ Eq (h :*: t) where
  eq = nthEq (Proxy ∷ Proxy (h :*: t)) (Proxy ∷ Proxy D1)

class NthEq index tuple tail where
  nthEq ∷ (Proxy tail) → (Proxy index) → tuple → tuple → Boolean

instance nthEqTuple ∷ (Eq e1, Nth n t e1, Succ n n', NthEq n' t (e2 :*: tail)) ⇒ NthEq n t (e1 :*: e2 :*: tail) where
  nthEq _ n t1 t2 = if nth n t1 == nth n t2
    then nthEq (Proxy ∷ Proxy (e2 :*: tail)) (Proxy ∷ Proxy n') t1 t2
    else false
else instance nthEqTupleLast ∷ (Succ n n', Nth n' t last, Nth n t head, Eq head, Eq last) ⇒ NthEq n t (head :*: last) where
  nthEq _ n t1 t2 = if nth n t1 == nth n t2
    then (nth (Proxy ∷ Proxy n') t1) == (nth (Proxy ∷ Proxy n') t2)
    else false


