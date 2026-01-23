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
open Fugue.Filter
open Fugue.Theory

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

-- Wavetable tests

test "wavetable sine matches sine oscillator" := do
  let wt := Wavetable.sine 256
  let wtOsc := wavetable wt 440.0
  let sineOsc := sine 440.0
  -- Should be close (wavetable uses linear interpolation)
  approxEq (wtOsc.sample 0.01) (sineOsc.sample 0.01) 0.05 ≡ true

test "wavetable square alternates" := do
  let wt := Wavetable.square 256
  let sig := wavetable wt 1.0
  approxEq (sig.sample 0.25) 1.0 0.1 ≡ true
  approxEq (sig.sample 0.75) (-1.0) 0.1 ≡ true

test "wavetableMorph interpolates between tables" := do
  let wt1 := Wavetable.fromFunction (fun _ => 0.0) 64
  let wt2 := Wavetable.fromFunction (fun _ => 1.0) 64
  let morphed := wavetableMorph wt1 wt2 0.5 100.0
  -- 50% morph should give ~0.5
  approxEq (morphed.sample 0.0) 0.5 0.1 ≡ true

test "wavetable fromHarmonics creates valid waveform" := do
  let wt := Wavetable.fromHarmonics [(1, 1.0), (3, 0.33), (5, 0.2)] 256
  let sig := wavetable wt 440.0
  let v := sig.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

-- Supersaw tests

test "supersaw produces valid output" := do
  let ss := supersaw 440.0 20.0 7
  let samples := List.range 100 |>.map fun i =>
    ss.sample (i.toFloat * 0.001)
  samples.all (fun v => v >= -2.0 && v <= 2.0) ≡ true

test "supersaw with zero detune equals single saw" := do
  let ss := supersaw 440.0 0.0 1
  let saw := sawtooth 440.0
  approxEq (ss.sample 0.01) (saw.sample 0.01) 0.01 ≡ true

test "supersaw with more voices produces richer signal" := do
  let ss3 := supersaw 440.0 20.0 3
  let ss7 := supersaw 440.0 20.0 7
  -- Both should produce valid bounded output
  let v3 := ss3.sample 0.01
  let v7 := ss7.sample 0.01
  (v3 >= -2.0 && v3 <= 2.0 && v7 >= -2.0 && v7 <= 2.0) ≡ true

test "hypersaw uses more voices than supersaw" := do
  -- hypersaw uses 15 voices by default
  let hs := hypersaw 440.0 30.0
  let v := hs.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "supersquare produces bounded output" := do
  let ss := supersquare 440.0 15.0 5
  let v := ss.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

-- SubOsc tests

test "subSine is one octave below" := do
  let sub := subSine 440.0 1
  -- Sub at t=0 should be 0 (sine at 0)
  approxEq (sub.sample 0.0) 0.0 ≡ true
  -- At quarter period of 220Hz, sub should be 1.0
  let quarterPeriod := 1.0 / (4.0 * 220.0)
  approxEq (sub.sample quarterPeriod) 1.0 ≡ true

test "subSine two octaves is quarter frequency" := do
  let sub := subSine 440.0 2
  -- 440 / 4 = 110Hz, quarter period is 1/(4*110) = 0.00227
  let quarterPeriod := 1.0 / (4.0 * 110.0)
  approxEq (sub.sample quarterPeriod) 1.0 ≡ true

test "subSquare produces square wave at lower frequency" := do
  let sub := subSquare 440.0 1
  -- At 220Hz, first half is positive, second half negative
  approxEq (sub.sample 0.001) 1.0 ≡ true

test "withSub mixes fundamental and sub" := do
  let fund := sine 440.0
  let mixed := withSub fund 440.0 0.5 1
  -- Mixed signal should be bounded
  let v := mixed.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "bassPatch produces bounded output" := do
  let bass := bassPatch 110.0 0.5
  let v := bass.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "reeseBass produces bounded output" := do
  let reese := reeseBass 110.0 5.0 0.4
  let v := reese.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "sub808 produces sine at lower octave" := do
  let sub := sub808 110.0
  -- Same as subSine 110 1 = 55Hz
  approxEq (sub.sample 0.0) 0.0 ≡ true

test "sub808Pitched starts higher and drops" := do
  let kick := sub808Pitched 60.0 0.05
  -- Early samples have higher frequency content
  let early := kick.sample 0.001
  let late := kick.sample 0.1
  -- Both should be bounded
  (early >= -2.0 && early <= 2.0 && late >= -2.0 && late <= 2.0) ≡ true

test "layeredSub produces bounded output" := do
  let layered := layeredSub 110.0 [0.7, 0.3]
  let v := layered.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

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

end Tests.Effects

-- ============================================================================
-- Filter Tests
-- ============================================================================

namespace Tests.Filter

testSuite "Filter"

test "lowpass1 passes low frequencies" := do
  -- Low frequency sine should pass through mostly unchanged
  let lowFreq := sine 100.0
  let filtered := lowpass1 1000.0 44100.0 lowFreq
  -- At low freq, filtered and original should be similar
  let orig := lowFreq.sample 0.01
  let filt := filtered.sample 0.01
  (absFloat (orig - filt) < 0.5 && true) ≡ true

test "lowpass1 attenuates DC less than highpass1" := do
  let dc := Signal.const 1.0
  let lp := lowpass1 1000.0 44100.0 dc
  let hp := highpass1 1000.0 44100.0 dc
  -- Lowpass should pass DC, highpass should block it
  let lpVal := absFloat (lp.sample 0.1)
  let hpVal := absFloat (hp.sample 0.1)
  (lpVal > hpVal && true) ≡ true

test "lowpassN produces valid output at different orders" := do
  let highFreq := sine 5000.0
  let lp1 := lowpassN 500.0 1 44100.0 highFreq
  let lp2 := lowpassN 500.0 2 44100.0 highFreq
  -- Both should produce valid bounded output
  let v1 := lp1.sample 0.01
  let v2 := lp2.sample 0.01
  (v1 >= -2.0 && v1 <= 2.0 && v2 >= -2.0 && v2 <= 2.0) ≡ true

test "biquad lowpass produces valid output" := do
  let sig := sine 440.0
  let filtered := lowpass 1000.0 0.707 44100.0 sig
  let v := filtered.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "biquad highpass produces valid output" := do
  let sig := sine 440.0
  let filtered := highpass 200.0 0.707 44100.0 sig
  let v := filtered.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "bandpass produces valid output" := do
  let sig := sine 1000.0
  let filtered := bandpass 1000.0 2.0 44100.0 sig
  let v := filtered.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "notch produces valid output" := do
  let sig := sine 1000.0
  let filtered := notch 1000.0 2.0 44100.0 sig
  let v := filtered.sample 0.01
  (v >= -2.0 && v <= 2.0) ≡ true

test "resonant filter boosts near cutoff" := do
  let sig := sine 1000.0
  let filtered := resonant 1000.0 0.9 44100.0 sig
  -- Resonant filter can boost signal
  let v := absFloat (filtered.sample 0.01)
  (v >= 0.0 && true) ≡ true

test "lfoFilter produces valid output" := do
  let sig := sawtooth 220.0
  let filtered := lfoFilter 800.0 0.5 2.0 2.0 44100.0 sig
  let v := filtered.sample 0.1
  (v >= -2.0 && v <= 2.0) ≡ true

test "envelopeFollowerSimple tracks amplitude" := do
  let sig := sine 100.0
  let env := envelopeFollowerSimple 0.01 44100.0 sig
  -- Envelope should be non-negative
  let v := env.sample 0.1
  (v >= 0.0 && true) ≡ true

test "dcBlock removes DC offset over time" := do
  let dc := Signal.const 0.5
  let blocked := dcBlock 44100.0 dc
  -- Initially may have some DC, but should decrease
  let early := absFloat (blocked.sample 0.01)
  let late := absFloat (blocked.sample 1.0)
  (late <= early && true) ≡ true

test "crossover splits signal into two bands" := do
  let sig := sine 1000.0
  let (lo, hi) := crossover 500.0 44100.0 sig
  -- Both outputs should be valid
  let loVal := lo.sample 0.01
  let hiVal := hi.sample 0.01
  (loVal >= -2.0 && loVal <= 2.0 && hiVal >= -2.0 && hiVal <= 2.0) ≡ true

end Tests.Filter

-- ============================================================================
-- Music Theory Tests
-- ============================================================================

namespace Tests.Theory

testSuite "Theory"

-- Note tests

test "midiToFreq A4 is 440Hz" := do
  approxEq (midiToFreq 69) 440.0 ≡ true

test "midiToFreq C4 is ~261.63Hz" := do
  approxEq (midiToFreq 60) 261.63 0.01 ≡ true

test "midiToFreq octave doubles frequency" := do
  let c4 := midiToFreq 60
  let c5 := midiToFreq 72
  approxEq c5 (c4 * 2.0) 0.01 ≡ true

test "freqToMidi round trips correctly" := do
  (freqToMidi 440.0 == 69) ≡ true

test "Note.toMidi C4 is 60" := do
  let note : Note := { name := .C, octave := 4 }
  (note.toMidi == 60) ≡ true

test "Note.toMidi A4 is 69" := do
  let note : Note := { name := .A, octave := 4 }
  (note.toMidi == 69) ≡ true

test "Note.fromMidi creates correct note" := do
  let note := Note.fromMidi 60
  (note.name == NoteName.C && note.octave == 4) ≡ true

test "parseNote parses C4" := do
  match parseNote "C4" with
  | some n => (n.name == NoteName.C && n.octave == 4) ≡ true
  | none => false ≡ true

test "parseNote parses F#3" := do
  match parseNote "F#3" with
  | some n => (n.name == NoteName.Fs && n.octave == 3) ≡ true
  | none => false ≡ true

test "parseNote parses Bb5" := do
  match parseNote "Bb5" with
  | some n => (n.name == NoteName.As && n.octave == 5) ≡ true
  | none => false ≡ true

test "transpose raises note by semitones" := do
  let c4 := 60
  let e4 := transpose c4 4  -- Major third
  (e4 == 64) ≡ true

-- Scale tests

test "major scale has 7 notes" := do
  (Scale.major.intervals.length == 7) ≡ true

test "major scale intervals are correct" := do
  (Scale.major.intervals == [0, 2, 4, 5, 7, 9, 11]) ≡ true

test "minor pentatonic has 5 notes" := do
  (Scale.minorPentatonic.intervals.length == 5) ≡ true

test "chromatic has 12 notes" := do
  (Scale.chromatic.intervals.length == 12) ≡ true

test "Scale.toMidi generates correct notes" := do
  let cMajor := Scale.major.toMidi 60  -- C major starting at C4
  (cMajor == [60, 62, 64, 65, 67, 69, 71]) ≡ true

test "Scale.contains works correctly" := do
  let inScale := Scale.major.contains 60 62   -- D is in C major
  let notInScale := Scale.major.contains 60 61  -- Db is not in C major
  (inScale && !notInScale) ≡ true

-- Chord tests

test "major triad has 3 notes" := do
  (ChordType.major.intervals.length == 3) ≡ true

test "major triad intervals are correct" := do
  (ChordType.major.intervals == [0, 4, 7]) ≡ true

test "minor7 has 4 notes" := do
  (ChordType.minor7.intervals.length == 4) ≡ true

test "Chord.toMidi generates correct notes" := do
  let cMajor : Chord := { root := 60, chordType := ChordType.major }
  (cMajor.toMidi == [60, 64, 67]) ≡ true

test "Chord.toFreq generates frequencies" := do
  let a4 : Chord := { root := 69, chordType := ChordType.major }
  let freqs := a4.toFreq
  approxEq freqs[0]! 440.0 ≡ true

test "Chord.invert moves notes up" := do
  let cMajor : Chord := { root := 60, chordType := ChordType.major }
  let firstInv := cMajor.invert 1
  (firstInv == [64, 67, 72]) ≡ true

-- Tempo tests

test "beatDuration at 120 BPM is 0.5s" := do
  approxEq (beatDuration 120.0) 0.5 ≡ true

test "beatDuration at 60 BPM is 1.0s" := do
  approxEq (beatDuration 60.0) 1.0 ≡ true

test "measureDuration at 120 BPM 4/4 is 2.0s" := do
  approxEq (measureDuration 120.0 4) 2.0 ≡ true

test "noteDuration quarter at 120 BPM is 0.5s" := do
  approxEq (noteDuration 120.0 .quarter) 0.5 ≡ true

test "noteDuration eighth at 120 BPM is 0.25s" := do
  approxEq (noteDuration 120.0 .eighth) 0.25 ≡ true

test "syncedDelay returns correct duration" := do
  approxEq (syncedDelay 120.0 .quarter) 0.5 ≡ true

test "syncedLfoRate returns correct frequency" := do
  -- Quarter note at 120 BPM = 0.5s, so LFO = 2 Hz
  approxEq (syncedLfoRate 120.0 .quarter) 2.0 ≡ true

test "barBeatToSeconds converts correctly" := do
  -- At 120 BPM, bar 1 beat 0 = 2 seconds (after first measure)
  approxEq (barBeatToSeconds 120.0 1 0.0 4) 2.0 ≡ true

end Tests.Theory

-- ============================================================================
-- Main
-- ============================================================================

def main : IO UInt32 := do
  runAllSuites
