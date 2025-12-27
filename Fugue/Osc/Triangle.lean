/-
  Fugue.Osc.Triangle - Triangle wave oscillator

  Contains only odd harmonics with rapid rolloff - mellow sound.
-/
import Fugue.Core.Signal

namespace Fugue.Osc

/-- Triangle wave oscillator at given frequency (Hz).
    Rises from -1 to 1 then falls back to -1, linearly. -/
@[inline]
def triangle (freq : Float) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    if phase < 0.5 then
      4.0 * phase - 1.0
    else
      3.0 - 4.0 * phase

/-- Alias for triangle wave. -/
@[inline]
def tri (freq : Float) : Signal Float := triangle freq

end Fugue.Osc
