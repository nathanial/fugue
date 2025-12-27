/-
  Fugue.Osc.Noise - Noise generators

  White noise for percussion, effects, and texture.
-/
import Fugue.Core.Signal

namespace Fugue.Osc

/-- Linear congruential generator constants. -/
private def lcgA : UInt64 := 1103515245
private def lcgC : UInt64 := 12345
private def lcgM : UInt64 := 2147483648  -- 2^31

/-- Generate a pseudo-random value in [-1, 1] from a hash. -/
@[inline]
private def hashToFloat (hash : UInt64) : Float :=
  let v := (lcgA * hash + lcgC) % lcgM
  (v.toFloat / lcgM.toFloat) * 2.0 - 1.0

/-- White noise generator.
    Uses time-based hashing for reproducible "random" values.
    The seed parameter allows different noise patterns. -/
@[inline]
def noise (seed : UInt64 := 42) : Signal Float :=
  fun t =>
    -- Hash time to get reproducible pseudo-random values
    -- Multiply by large number to get variation at sample rate
    let timeHash := (t * 1000000.0).toUInt64
    let combined := timeHash ^^^ seed ^^^ (seed <<< 16)
    hashToFloat combined

/-- White noise with higher quality (double hash). -/
@[inline]
def noiseHQ (seed : UInt64 := 42) : Signal Float :=
  fun t =>
    let timeHash := (t * 1000000.0).toUInt64
    let h1 := timeHash ^^^ seed
    let h2 := (lcgA * h1 + lcgC) % lcgM
    let h3 := (lcgA * h2 + lcgC) % lcgM
    (h3.toFloat / lcgM.toFloat) * 2.0 - 1.0

/-- Sample-and-hold noise (changes value at given frequency). -/
@[inline]
def noiseStep (freq : Float) (seed : UInt64 := 42) : Signal Float :=
  fun t =>
    let step := Float.floor (t * freq)
    let hash := step.toUInt64 ^^^ seed
    hashToFloat hash

end Fugue.Osc
