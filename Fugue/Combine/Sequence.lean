/-
  Fugue.Combine.Sequence - Sequential composition

  Play sounds one after another.
-/
import Fugue.Core.Signal
import Fugue.Core.Duration

namespace Fugue.Combine

/-- Append two duration-aware signals (play b after a). -/
def append (a b : DSignal Float) : DSignal Float :=
  { signal := fun t =>
      if t < a.duration then a.signal.sample t
      else b.signal.sample (t - a.duration)
    duration := a.duration + b.duration }

/-- Sequence multiple DSignals one after another. -/
def sequence (signals : List (DSignal Float)) : DSignal Float :=
  signals.foldl append { signal := Signal.const 0.0, duration := 0.0 }

/-- Operator for appending signals. -/
instance : Append (DSignal Float) where
  append := append

/-- Delay a signal by a given time offset.
    Values before the delay are zero. -/
@[inline]
def delay (time : Float) (sig : Signal Float) : Signal Float :=
  fun t => if t < time then 0.0 else sig.sample (t - time)

/-- Delay a DSignal (adds silence before it). -/
@[inline]
def delayD (time : Float) (ds : DSignal Float) : DSignal Float :=
  { signal := delay time ds.signal
    duration := ds.duration + time }

/-- Create a signal that repeats a DSignal indefinitely. -/
def loop (ds : DSignal Float) : Signal Float :=
  if ds.duration <= 0.0 then Signal.const 0.0
  else fun t =>
    let phase := t - Float.floor (t / ds.duration) * ds.duration
    ds.signal.sample phase

/-- Repeat a DSignal n times. -/
def repeatN (n : Nat) (ds : DSignal Float) : DSignal Float :=
  { signal := loop ds
    duration := ds.duration * n.toFloat }

/-- Insert silence between DSignals. -/
def intersperse (gap : Float) (signals : List (DSignal Float)) : DSignal Float :=
  let silence := { signal := Signal.const 0.0, duration := gap : DSignal Float }
  match signals with
  | [] => silence
  | [x] => x
  | x :: xs => xs.foldl (fun acc ds => acc ++ silence ++ ds) x

end Fugue.Combine
