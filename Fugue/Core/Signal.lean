/-
  Fugue.Core.Signal - The core Signal type

  A Signal is a function from time (in seconds) to a value.
  This is the fundamental building block for sound synthesis.
-/

namespace Fugue

/-- A signal is a function from time (in seconds) to a value.
    For audio, we typically use `Signal Float` where values are in [-1, 1]. -/
def Signal (α : Type) := Float → α

namespace Signal

/-- Sample a signal at a given time. -/
@[inline]
def sample (sig : Signal α) (t : Float) : α := sig t

/-- Constant signal that always returns the same value. -/
@[inline]
def const (x : α) : Signal α := fun _ => x

/-- The identity signal - returns time itself. -/
@[inline]
def time : Signal Float := id

/-- Map a function over signal values. -/
@[inline]
def map (f : α → β) (sig : Signal α) : Signal β :=
  fun t => f (sig t)

/-- Apply a signal of functions to a signal of values. -/
@[inline]
def ap (sf : Signal (α → β)) (sa : Signal α) : Signal β :=
  fun t => sf t (sa t)

/-- Combine two signals pointwise with a binary function. -/
@[inline]
def zipWith (f : α → β → γ) (sa : Signal α) (sb : Signal β) : Signal γ :=
  fun t => f (sa t) (sb t)

/-- Bind for signals (monadic sequencing based on time). -/
@[inline]
def bind (sa : Signal α) (f : α → Signal β) : Signal β :=
  fun t => f (sa t) t

instance : Functor Signal where
  map := Signal.map

instance : Pure Signal where
  pure := Signal.const

instance : Applicative Signal where
  seq sf sa := Signal.ap sf (sa ())

instance : Monad Signal where
  bind := Signal.bind

/-- Add two numeric signals. -/
@[inline]
def add [Add α] (a b : Signal α) : Signal α :=
  zipWith (· + ·) a b

/-- Multiply two numeric signals. -/
@[inline]
def mul [Mul α] (a b : Signal α) : Signal α :=
  zipWith (· * ·) a b

/-- Scale a signal by a constant factor. -/
@[inline]
def scale [Mul α] (factor : α) (sig : Signal α) : Signal α :=
  map (factor * ·) sig

/-- Negate a signal. -/
@[inline]
def neg [Neg α] (sig : Signal α) : Signal α :=
  map (- ·) sig

/-- Delay a signal by shifting it forward in time.
    Values before the delay time will be sampled at t=0. -/
@[inline]
def delay (dt : Float) (sig : Signal α) : Signal α :=
  fun t =>
    let t' := t - dt
    sig (if t' < 0.0 then 0.0 else t')

/-- Time-shift a signal (can shift backward or forward). -/
@[inline]
def shift (dt : Float) (sig : Signal α) : Signal α :=
  fun t => sig (t - dt)

/-- Speed up or slow down a signal.
    factor > 1 speeds up, factor < 1 slows down. -/
@[inline]
def stretch (factor : Float) (sig : Signal α) : Signal α :=
  fun t => sig (t * factor)

instance [Add α] : Add (Signal α) where
  add := Signal.add

instance [Mul α] : Mul (Signal α) where
  mul := Signal.mul

instance [Neg α] : Neg (Signal α) where
  neg := Signal.neg

end Signal
end Fugue
