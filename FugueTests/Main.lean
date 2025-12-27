/-
  Fugue Test Suite
-/
import Crucible
import Fugue

open Crucible
open Fugue
open Fugue.Osc
open Fugue.Env
open Fugue.Combine
open Fugue.Render
open Fugue.Effects

-- Helper to get absolute value of float
def absFloat (x : Float) : Float := if x < 0.0 then -x else x

-- Helper to check if a float is approximately equal
def approxEq (a b : Float) (eps : Float := 0.0001) : Bool :=
  absFloat (a - b) < eps

-- ============================================================================
-- Signal Tests
-- ============================================================================

namespace Tests.Signal

testSuite "Signal"

test "const returns constant value" := do
  let sig := Signal.const 42.0
  (sig.sample 0.0 == 42.0) ≡ true
  (sig.sample 1.0 == 42.0) ≡ true
  (sig.sample 100.0 == 42.0) ≡ true

test "time returns time value" := do
  let sig := Signal.time
  (sig.sample 0.0 == 0.0) ≡ true
  (sig.sample 1.5 == 1.5) ≡ true

test "map transforms values" := do
  let sig := Signal.const 2.0 |> Signal.map (· * 3.0)
  (sig.sample 0.0 == 6.0) ≡ true

test "add combines signals" := do
  let a := Signal.const 3.0
  let b := Signal.const 4.0
  let sum := Signal.add a b
  (sum.sample 0.0 == 7.0) ≡ true

test "scale multiplies by factor" := do
  let sig := Signal.const 5.0 |> Signal.scale 2.0
  (sig.sample 0.0 == 10.0) ≡ true

#generate_tests

end Tests.Signal

-- ============================================================================
-- Oscillator Tests
-- ============================================================================

namespace Tests.Oscillator

testSuite "Oscillator"

test "sine at t=0 is 0" := do
  let sig := sine 440.0
  approxEq (sig.sample 0.0) 0.0 ≡ true

test "sine at quarter period is 1" := do
  let freq := 440.0
  let sig := sine freq
  let t := 1.0 / (4.0 * freq)
  approxEq (sig.sample t) 1.0 ≡ true

test "square alternates between -1 and 1" := do
  let sig := square 1.0
  approxEq (sig.sample 0.1) 1.0 ≡ true
  approxEq (sig.sample 0.6) (-1.0) ≡ true

test "sawtooth rises linearly" := do
  let sig := sawtooth 1.0
  approxEq (sig.sample 0.0) (-1.0) ≡ true
  approxEq (sig.sample 0.5) 0.0 ≡ true

test "triangle peaks at midpoint" := do
  let sig := triangle 1.0
  approxEq (sig.sample 0.0) (-1.0) ≡ true
  approxEq (sig.sample 0.25) 0.0 ≡ true
  approxEq (sig.sample 0.5) 1.0 ≡ true

test "noise produces values in range" := do
  let sig := noise 42
  let samples := List.range 100 |>.map fun i =>
    sig.sample (i.toFloat * 0.001)
  samples.all (fun v => v >= -1.0 && v <= 1.0) ≡ true

#generate_tests

end Tests.Oscillator

-- ============================================================================
-- ADSR Tests
-- ============================================================================

namespace Tests.ADSR

testSuite "ADSR"

test "starts at 0" := do
  let env := ADSR.create (attack := 0.1) (decay := 0.1) (sustain := 0.5) (release := 0.1)
  approxEq (env.sample 0.0) 0.0 ≡ true

test "reaches 1 at end of attack" := do
  let env := ADSR.create (attack := 0.1) (decay := 0.1) (sustain := 0.5) (release := 0.1)
  approxEq (env.sample 0.1) 1.0 ≡ true

test "reaches sustain level after decay" := do
  let env := ADSR.create (attack := 0.1) (decay := 0.1) (sustain := 0.5) (release := 0.1)
  approxEq (env.sample 0.2) 0.5 ≡ true

test "ends at 0 after release" := do
  let env := ADSR.create (attack := 0.1) (decay := 0.1) (sustain := 0.5) (release := 0.1)
  approxEq (env.sample 0.35) 0.0 ≡ true

test "duration is sum of phases" := do
  let env := ADSR.create (attack := 0.1) (decay := 0.2) (sustain := 0.5) (release := 0.3)
  approxEq env.duration 0.6 ≡ true

test "percussive has zero sustain" := do
  let env := ADSR.percussive (decay := 0.5)
  (env.sustain == 0.0) ≡ true

#generate_tests

end Tests.ADSR

-- ============================================================================
-- Combinator Tests
-- ============================================================================

namespace Tests.Combine

testSuite "Combine"

test "mix adds two signals" := do
  let a := Signal.const 2.0
  let b := Signal.const 3.0
  let mixed := mix a b
  (mixed.sample 0.0 == 5.0) ≡ true

test "mixAll normalizes by count" := do
  let sigs := [Signal.const 2.0, Signal.const 4.0]
  let mixed := mixAll sigs
  (mixed.sample 0.0 == 3.0) ≡ true

test "scale multiplies amplitude" := do
  let sig := sine 440.0 |> scale 0.5
  let original := sine 440.0
  approxEq (sig.sample 0.25) (original.sample 0.25 * 0.5) ≡ true

test "clip limits to range" := do
  let loud := Signal.const 5.0 |> clip
  (loud.sample 0.0 == 1.0) ≡ true
  let negative := Signal.const (-5.0) |> clip
  (negative.sample 0.0 == -1.0) ≡ true

test "append sequences signals" := do
  let a : DSignal Float := { signal := Signal.const 1.0, duration := 1.0 }
  let b : DSignal Float := { signal := Signal.const 2.0, duration := 1.0 }
  let seq := append a b
  (seq.duration == 2.0) ≡ true
  (seq.signal.sample 0.5 == 1.0) ≡ true
  (seq.signal.sample 1.5 == 2.0) ≡ true

#generate_tests

end Tests.Combine

-- ============================================================================
-- Render Tests
-- ============================================================================

namespace Tests.Render

testSuite "Render"

test "renderSignal produces correct samples" := do
  let config := Config.cdQuality
  let sig := sine 440.0
  let buffer := renderSignal config 1.0 sig
  (buffer.size == 44100) ≡ true

test "renderSignal with low sample rate" := do
  let config : Config := { sampleRate := 1000.0 }
  let sig := sine 100.0
  let buffer := renderSignal config 0.5 sig
  (buffer.size == 500) ≡ true

test "peakAmplitude finds max" := do
  let buffer := FloatArray.empty
    |>.push 0.5 |>.push (-0.8) |>.push 0.3
  approxEq (peakAmplitude buffer) 0.8 ≡ true

test "normalize scales to peak 1.0" := do
  let buffer := FloatArray.empty
    |>.push 0.5 |>.push (-0.25)
  let normalized := normalize buffer
  approxEq (peakAmplitude normalized) 1.0 ≡ true

#generate_tests

end Tests.Render

-- ============================================================================
-- Effects Tests
-- ============================================================================

namespace Tests.Effects

testSuite "Effects"

test "hardClip limits to threshold" := do
  let loud := Signal.const 5.0
  let clipped := hardClip 1.0 loud
  (clipped.sample 0.0 == 1.0) ≡ true
  let negative := Signal.const (-5.0)
  let clippedNeg := hardClip 1.0 negative
  (clippedNeg.sample 0.0 == -1.0) ≡ true

test "hardClip passes values within threshold" := do
  let quiet := Signal.const 0.5
  let clipped := hardClip 1.0 quiet
  (clipped.sample 0.0 == 0.5) ≡ true

test "softClip approaches but doesn't exceed 1" := do
  let loud := Signal.const 10.0
  let clipped := softClip 1.0 loud
  let v := clipped.sample 0.0
  (v < 1.0 && v > 0.99) ≡ true

test "overdrive saturates signal" := do
  let sig := Signal.const 0.5
  let driven := overdrive 4.0 sig
  -- tanh(0.5 * 4) = tanh(2) ≈ 0.964
  let v := driven.sample 0.0
  (v > 0.9 && v < 1.0) ≡ true

test "delay shifts signal in time" := do
  let sig := Signal.const 1.0
  let delayed := Effects.delay 0.5 sig
  (delayed.sample 0.3 == 0.0) ≡ true
  (delayed.sample 0.6 == 1.0) ≡ true

test "delay with zero time is identity" := do
  let sig := sine 440.0
  let delayed := Effects.delay 0.0 sig
  approxEq (sig.sample 0.1) (delayed.sample 0.1) ≡ true

test "delayWithFeedback produces echoes" := do
  -- Create an impulse at t=0
  let impulse : Signal Float := fun t => if t < 0.001 then 1.0 else 0.0
  let echoed := delayWithFeedback 0.1 0.5 5 impulse
  -- At t=0, we get the impulse
  (echoed.sample 0.0 > 0.9 && true) ≡ true
  -- At t=0.1, we get first echo (0.5 amplitude)
  approxEq (echoed.sample 0.1) 0.5 ≡ true
  -- At t=0.2, we get second echo (0.25 amplitude)
  approxEq (echoed.sample 0.2) 0.25 ≡ true

test "tremolo modulates amplitude" := do
  let sig := Signal.const 1.0
  let tremmed := tremolo 10.0 1.0 sig
  -- At different times, amplitude should vary
  let v1 := tremmed.sample 0.0
  let v2 := tremmed.sample 0.025  -- Quarter period at 10Hz
  (v1 != v2) ≡ true

test "tremolo at zero depth is identity" := do
  let sig := sine 440.0
  let tremmed := tremolo 5.0 0.0 sig
  approxEq (sig.sample 0.1) (tremmed.sample 0.1) ≡ true

test "ringMod multiplies by carrier" := do
  let sig := Signal.const 1.0
  let modded := ringMod 100.0 sig
  -- At t=0, sin(0) = 0
  approxEq (modded.sample 0.0) 0.0 ≡ true

test "bitcrush quantizes values" := do
  let sig := Signal.const 0.5
  let crushed := bitcrush 4 sig  -- 16 levels
  -- Value should be quantized
  let v := crushed.sample 0.0
  (v >= -1.0 && v <= 1.0) ≡ true

test "chorus output is bounded" := do
  let sig := sine 440.0
  let chorused := chorus {} sig
  let samples := List.range 100 |>.map fun i =>
    chorused.sample (i.toFloat * 0.001)
  samples.all (fun v => v >= -2.0 && v <= 2.0) ≡ true

test "reverb mixes dry and wet" := do
  let sig := Signal.const 1.0
  let config : ReverbConfig := { wetDry := 0.5 }
  let reverbed := reverb config sig
  -- At t=0, should have dry component
  (reverbed.sample 0.0 > 0.0 && true) ≡ true

test "earlyReflections adds echoes" := do
  let impulse : Signal Float := fun t => if t < 0.001 then 1.0 else 0.0
  let reflected := earlyReflections 0.5 impulse
  -- First reflection at 0.005s, sample at 0.0055 catches impulse via reflection
  -- The wet signal samples at 0.0055 - 0.005 = 0.0005 which is in impulse range
  (reflected.sample 0.0055 > 0.0 && true) ≡ true

#generate_tests

end Tests.Effects

-- ============================================================================
-- Main
-- ============================================================================

def main : IO UInt32 := do
  runAllSuites
