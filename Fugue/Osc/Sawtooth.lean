/-
  Fugue.Osc.Sawtooth - Sawtooth wave oscillator

  Contains all harmonics - bright, buzzy sound.
-/
import Fugue.Core.Signal

namespace Fugue.Osc

/-- Sawtooth wave oscillator at given frequency (Hz).
    Rises linearly from -1 to 1 over each period. -/
@[inline]
def sawtooth (freq : Float) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    2.0 * phase - 1.0

/-- Inverse (descending) sawtooth wave.
    Falls linearly from 1 to -1 over each period. -/
@[inline]
def sawtoothDown (freq : Float) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    1.0 - 2.0 * phase

/-- Alias for ascending sawtooth. -/
@[inline]
def saw (freq : Float) : Signal Float := sawtooth freq

end Fugue.Osc
