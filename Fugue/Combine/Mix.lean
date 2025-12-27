/-
  Fugue.Combine.Mix - Signal mixing (parallel composition)

  Combine multiple signals by adding them together.
-/
import Fugue.Core.Signal
import Fugue.Core.Duration

namespace Fugue.Combine

/-- Maximum of two floats. -/
@[inline]
private def maxFloat (a b : Float) : Float := if a > b then a else b

/-- Mix (add) two signals together. -/
@[inline]
def mix (a b : Signal Float) : Signal Float :=
  Signal.add a b

/-- Mix multiple signals with equal weight.
    Automatically normalizes by dividing by count. -/
def mixAll (signals : List (Signal Float)) : Signal Float :=
  match signals with
  | [] => Signal.const 0.0
  | sigs =>
    let count := sigs.length.toFloat
    fun t => sigs.foldl (fun acc sig => acc + sig.sample t) 0.0 / count

/-- Mix multiple signals without normalization (raw sum). -/
def mixRaw (signals : List (Signal Float)) : Signal Float :=
  fun t => signals.foldl (fun acc sig => acc + sig.sample t) 0.0

/-- Mix signals with explicit weights. -/
def mixWeighted (signals : List (Float × Signal Float)) : Signal Float :=
  fun t => signals.foldl (fun acc (w, sig) => acc + w * sig.sample t) 0.0

/-- Mix two DSignals (uses the longer duration). -/
def mixD (a b : DSignal Float) : DSignal Float :=
  { signal := mix a.signal b.signal
    duration := maxFloat a.duration b.duration }

/-- Mix multiple DSignals. -/
def mixAllD (signals : List (DSignal Float)) : DSignal Float :=
  match signals with
  | [] => { signal := Signal.const 0.0, duration := 0.0 }
  | sigs =>
    let maxDur := sigs.foldl (fun acc ds => maxFloat acc ds.duration) 0.0
    let count := sigs.length.toFloat
    { signal := fun t => sigs.foldl (fun acc ds =>
        acc + (if t < ds.duration then ds.signal.sample t else 0.0)) 0.0 / count
      duration := maxDur }

end Fugue.Combine
