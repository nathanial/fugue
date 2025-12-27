/-
  Fugue.Combine.Scale - Amplitude scaling and modulation

  Control the volume and dynamics of signals.
-/
import Fugue.Core.Signal
import Fugue.Core.Duration

namespace Fugue.Combine

/-- Scale signal amplitude by a constant factor. -/
@[inline]
def scale (factor : Float) (sig : Signal Float) : Signal Float :=
  Signal.scale factor sig

/-- Modulate amplitude with another signal (ring modulation). -/
@[inline]
def modulate (modulator : Signal Float) (carrier : Signal Float) : Signal Float :=
  Signal.mul modulator carrier

/-- Clamp signal values to [-1, 1] range (soft limiting). -/
@[inline]
def clip (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t
    if v > 1.0 then 1.0
    else if v < -1.0 then -1.0
    else v

/-- Soft clipping using tanh for smoother saturation. -/
@[inline]
def softClip (sig : Signal Float) : Signal Float :=
  fun t => Float.tanh (sig.sample t)

/-- Apply gain in decibels. -/
@[inline]
def gainDb (db : Float) (sig : Signal Float) : Signal Float :=
  let factor := Float.pow 10.0 (db / 20.0)
  scale factor sig

/-- Invert signal polarity. -/
@[inline]
def invert (sig : Signal Float) : Signal Float :=
  Signal.neg sig

/-- Scale a DSignal. -/
@[inline]
def scaleD (factor : Float) (ds : DSignal Float) : DSignal Float :=
  { signal := scale factor ds.signal, duration := ds.duration }

/-- Fade in over given duration. -/
def fadeIn (fadeTime : Float) (sig : Signal Float) : Signal Float :=
  fun t =>
    let envelope := if t < fadeTime then t / fadeTime else 1.0
    envelope * sig.sample t

/-- Fade out starting at given time. -/
def fadeOut (startTime : Float) (fadeTime : Float) (sig : Signal Float) : Signal Float :=
  fun t =>
    let envelope :=
      if t < startTime then 1.0
      else if t < startTime + fadeTime then 1.0 - (t - startTime) / fadeTime
      else 0.0
    envelope * sig.sample t

end Fugue.Combine
