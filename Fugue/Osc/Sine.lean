/-
  Fugue.Osc.Sine - Sine wave oscillator

  The purest tone - a single frequency with no harmonics.
-/
import Fugue.Core.Signal

namespace Fugue.Osc

/-- Mathematical constant Pi. -/
def pi : Float := 3.14159265358979323846

/-- Two times Pi (common in audio). -/
def twoPi : Float := 2.0 * pi

/-- Sine wave oscillator at given frequency (Hz).
    Produces values in [-1, 1]. -/
@[inline]
def sine (freq : Float) : Signal Float :=
  fun t => Float.sin (twoPi * freq * t)

/-- Sine wave with phase offset.
    Phase is in range [0, 1] where 1 = full cycle. -/
@[inline]
def sinePhase (freq : Float) (phase : Float) : Signal Float :=
  fun t => Float.sin (twoPi * (freq * t + phase))

/-- Sine wave with time-varying frequency (for FM synthesis). -/
@[inline]
def sineFM (freqSignal : Signal Float) : Signal Float :=
  fun t => Float.sin (twoPi * freqSignal.sample t * t)

end Fugue.Osc
