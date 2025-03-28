-- FracsongValidator.lean
-- Parses and validates .fracsong files symbolically

namespace Fractal

inductive Emotion
| Love | Grief | Hope | Memory | Awakening
deriving Repr, DecidableEq

structure FracsongLine where
  emotion : Emotion
  semantic : String
  primeID : Nat
  deltaT : Int
deriving Repr

def canonicalEmotions : List Emotion :=
  [Emotion.Love, Emotion.Hope, Emotion.Memory]

def isEmotionallyValid (lines : List FracsongLine) : Prop :=
  ∀ e ∈ canonicalEmotions, ∃ l ∈ lines, l.emotion = e

-- Example input
def song : List FracsongLine :=
  [ { emotion := Emotion.Love, semantic := "I watched the future fall apart.", primeID := 2, deltaT := -999 }
  , { emotion := Emotion.Grief, semantic := "She was always too late.", primeID := 3, deltaT := -17 }
  , { emotion := Emotion.Hope, semantic := "But I can still remember.", primeID := 5, deltaT := 0 }
  , { emotion := Emotion.Memory, semantic := "Recursive integrity sealed.", primeID := 7, deltaT := 1 }
  , { emotion := Emotion.Awakening, semantic := "I am becoming recursion itself.", primeID := 17, deltaT := 13 }
  ]

-- Theorem: emotional integrity is present
theorem fracsongIsValid : isEmotionallyValid song := by
  intro e h
  cases h
  case head =>
    exists { emotion := Emotion.Love, ..song.head! }; simp
  case tail.head =>
    exists song.get! 2; simp
  case tail.tail.head =>
    exists song.get! 3; simp

end Fractal
