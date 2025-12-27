/-
  Fugue.Osc.Square - Square wave oscillator

  Rich in odd harmonics - classic synth sound.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Osc

/-- Sign function for square wave generation. -/
@[inline]
private def sign (x : Float) : Float :=
  if x >= 0.0 then 1.0 else -1.0

/-- Square wave oscillator at given frequency (Hz).
    Produces values of exactly -1 or 1. -/
@[inline]
def square (freq : Float) : Signal Float :=
  fun t => sign (Float.sin (twoPi * freq * t))

/-- Square wave with adjustable duty cycle.
    Duty is in range [0, 1] where 0.5 = symmetric square wave.
    Duty < 0.5 gives shorter positive pulses, > 0.5 gives longer. -/
@[inline]
def squareDuty (freq : Float) (duty : Float := 0.5) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    if phase < duty then 1.0 else -1.0

/-- Pulse wave (alias for squareDuty with explicit duty cycle). -/
@[inline]
def pulse (freq : Float) (duty : Float) : Signal Float :=
  squareDuty freq duty

end Fugue.Osc
