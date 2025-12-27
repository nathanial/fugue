/-
  Fugue.Core.Duration - Duration-aware signals

  A DSignal is a signal with a known finite duration.
  This is useful for notes, envelopes, and sequencing.
-/
import Fugue.Core.Signal

namespace Fugue

/-- A duration-aware signal has a known length in seconds. -/
structure DSignal (α : Type) where
  /-- The underlying signal function. -/
  signal : Signal α
  /-- Duration in seconds. -/
  duration : Float

namespace DSignal

/-- Create a duration-aware signal from a signal and duration. -/
@[inline]
def create (sig : Signal α) (dur : Float) : DSignal α :=
  { signal := sig, duration := dur }

/-- Sample the signal at time t, returning None if outside duration. -/
@[inline]
def sample? (ds : DSignal α) (t : Float) : Option α :=
  if t >= 0.0 && t < ds.duration then
    some (ds.signal.sample t)
  else
    none

/-- Sample the signal, clamping time to valid range. -/
@[inline]
def sampleClamped (ds : DSignal α) (t : Float) : α :=
  let upper := ds.duration - 0.0001
  let t' := if t < 0.0 then 0.0 else if t > upper then upper else t
  ds.signal.sample t'

/-- Truncate an infinite signal to a specific duration. -/
@[inline]
def take (dur : Float) (sig : Signal α) : DSignal α :=
  create sig dur

/-- Extend a DSignal with a default value after its duration. -/
@[inline]
def withDefault (default : α) (ds : DSignal α) : Signal α :=
  fun t => if t < ds.duration then ds.signal.sample t else default

/-- Convert back to an infinite signal (samples 0 outside duration). -/
@[inline]
def toSignal [OfNat α 0] (ds : DSignal α) : Signal α :=
  ds.withDefault 0

/-- Map over the signal values. -/
@[inline]
def map (f : α → β) (ds : DSignal α) : DSignal β :=
  { signal := f <$> ds.signal, duration := ds.duration }

/-- Scale the duration (stretches time). -/
@[inline]
def scaleDuration (factor : Float) (ds : DSignal α) : DSignal α :=
  { signal := ds.signal.stretch (1.0 / factor), duration := ds.duration * factor }

instance : Functor DSignal where
  map := DSignal.map

end DSignal

/-- Type alias for audio signals (values in [-1, 1]). -/
abbrev Audio := Signal Float

/-- Type alias for finite audio clips. -/
abbrev AudioClip := DSignal Float

end Fugue
