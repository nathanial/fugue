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
-- Main
-- ============================================================================

def main : IO UInt32 := do
  runAllSuites
