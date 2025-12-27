/-
  Fugue Demo - Sound synthesis examples

  Run with: ./build.sh demo && .lake/build/bin/demo
-/
import Fugue

open Fugue
open Fugue.Osc
open Fugue.Env
open Fugue.Combine
open Fugue.Render
open Fugue.FFI
open Fugue.Effects
open Fugue.Filter

/-- Play a single note with ADSR envelope. -/
def playNote (player : AudioPlayer) (freq : Float) (duration : Float) : IO Unit := do
  let env := ADSR.create (attack := 0.02) (decay := 0.1) (sustain := 0.7) (release := 0.2)
  let note := applyEnvelopeWithHold env duration (sine freq)
  let samples := renderDSignalClipped cdQuality note
  player.play samples

/-- Play a chord (multiple notes together). -/
def playChord (player : AudioPlayer) : IO Unit := do
  IO.println "Playing C major chord..."

  let env := ADSR.create (attack := 0.05) (decay := 0.1) (sustain := 0.6) (release := 0.4)

  -- C major chord frequencies
  let c4 := 261.63
  let e4 := 329.63
  let g4 := 392.00

  -- Create individual notes
  let noteC := applyEnvelope env (sine c4)
  let noteE := applyEnvelope env (sine e4)
  let noteG := applyEnvelope env (sine g4)

  -- Mix them together
  let chord := mixAllD [noteC, noteE, noteG]

  -- Scale down to avoid clipping and render
  let samples := renderDSignalClipped cdQuality (scaleD 0.5 chord)
  player.play samples

/-- Play an arpeggio (notes in sequence). -/
def playArpeggio (player : AudioPlayer) : IO Unit := do
  IO.println "Playing arpeggio..."

  let env := ADSR.create (attack := 0.01) (decay := 0.05) (sustain := 0.5) (release := 0.1)
  let noteDur := 0.15

  -- Create notes
  let mkNote freq := applyEnvelopeWithHold env noteDur (sine freq)

  let notes := [
    mkNote 261.63,  -- C4
    mkNote 329.63,  -- E4
    mkNote 392.00,  -- G4
    mkNote 523.25,  -- C5
    mkNote 392.00,  -- G4
    mkNote 329.63,  -- E4
  ]

  let arpeggio := sequence notes
  let samples := renderDSignalClipped cdQuality (scaleD 0.5 arpeggio)
  player.play samples

/-- Demonstrate different waveforms. -/
def playWaveforms (player : AudioPlayer) : IO Unit := do
  IO.println "Playing different waveforms..."

  let env := ADSR.create (attack := 0.01) (decay := 0.1) (sustain := 0.7) (release := 0.2)
  let freq := 220.0
  let holdTime := 0.3

  -- Sine wave
  IO.println "  Sine wave..."
  let sinNote := applyEnvelopeWithHold env holdTime (sine freq)
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 sinNote))

  -- Square wave
  IO.println "  Square wave..."
  let sqNote := applyEnvelopeWithHold env holdTime (square freq)
  player.play (renderDSignalClipped cdQuality (scaleD 0.2 sqNote))

  -- Sawtooth wave
  IO.println "  Sawtooth wave..."
  let sawNote := applyEnvelopeWithHold env holdTime (sawtooth freq)
  player.play (renderDSignalClipped cdQuality (scaleD 0.2 sawNote))

  -- Triangle wave
  IO.println "  Triangle wave..."
  let triNote := applyEnvelopeWithHold env holdTime (triangle freq)
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 triNote))

/-- Play a bass line using sawtooth wave. -/
def playBass (player : AudioPlayer) : IO Unit := do
  IO.println "Playing bass line..."

  let env := ADSR.percussive (decay := 0.3)
  let noteDur := 0.1

  let mkNote freq := applyEnvelopeWithHold env noteDur (sawtooth freq)

  -- Simple bass pattern
  let notes := [
    mkNote 82.41,   -- E2
    mkNote 82.41,
    mkNote 110.0,   -- A2
    mkNote 98.0,    -- G2
  ]

  let bass := sequence notes
  let samples := renderDSignalClipped cdQuality (scaleD 0.4 bass)
  player.play samples

/-- Demonstrate audio effects. -/
def playEffects (player : AudioPlayer) : IO Unit := do
  IO.println "Demonstrating audio effects..."

  let env := ADSR.create (attack := 0.01) (decay := 0.1) (sustain := 0.6) (release := 0.3)
  let freq := 330.0  -- E4
  let holdTime := 0.5

  -- Clean note for comparison
  IO.println "  Clean sine wave..."
  let cleanNote := applyEnvelopeWithHold env holdTime (sine freq)
  player.play (renderDSignalClipped cdQuality (scaleD 0.4 cleanNote))

  -- With overdrive
  IO.println "  With overdrive..."
  let drivenSig := overdrive 3.0 (sine freq)
  let drivenNote := applyEnvelopeWithHold env holdTime drivenSig
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 drivenNote))

  -- With tremolo
  IO.println "  With tremolo..."
  let tremSig := tremolo 6.0 0.6 (sine freq)
  let tremNote := applyEnvelopeWithHold env holdTime tremSig
  player.play (renderDSignalClipped cdQuality (scaleD 0.4 tremNote))

  -- With delay
  IO.println "  With delay/echo..."
  let delaySig := slapback 0.15 0.5 (sine freq)
  let delayNote := applyEnvelopeWithHold env holdTime delaySig
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 delayNote))

  -- With chorus
  IO.println "  With chorus..."
  let chorusSig := chorusSimple 1.2 0.003 (sine freq)
  let chorusNote := applyEnvelopeWithHold env holdTime chorusSig
  player.play (renderDSignalClipped cdQuality (scaleD 0.4 chorusNote))

  -- With reverb
  IO.println "  With reverb..."
  let reverbConfig : ReverbConfig := { roomSize := 0.6, wetDry := 0.4 }
  let reverbSig := reverb reverbConfig (sine freq)
  let reverbNote := applyEnvelopeWithHold env (holdTime + 0.3) reverbSig
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 reverbNote))

/-- Demonstrate filter effects. -/
def playFilters (player : AudioPlayer) : IO Unit := do
  IO.println "Demonstrating filters..."

  let env := ADSR.create (attack := 0.01) (decay := 0.1) (sustain := 0.6) (release := 0.3)
  let holdTime := 0.5

  -- Bright sawtooth (unfiltered)
  IO.println "  Unfiltered sawtooth..."
  let saw := applyEnvelopeWithHold env holdTime (sawtooth 220.0)
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 saw))

  -- With lowpass filter (muffled)
  IO.println "  With lowpass filter (800 Hz)..."
  let lpSaw := applyEnvelopeWithHold env holdTime (lowpass 800.0 0.707 44100.0 (sawtooth 220.0))
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 lpSaw))

  -- With resonant filter (synth-like)
  IO.println "  With resonant filter..."
  let resSaw := applyEnvelopeWithHold env holdTime (resonant 600.0 0.8 44100.0 (sawtooth 220.0))
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 resSaw))

  -- Filter sweep (wah-wah style)
  IO.println "  Filter sweep (LFO modulated)..."
  let sweepSaw := applyEnvelopeWithHold env holdTime (lfoFilter 800.0 0.7 2.0 3.0 44100.0 (sawtooth 220.0))
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 sweepSaw))

  -- Highpass filter (thin/tinny)
  IO.println "  With highpass filter (1000 Hz)..."
  let hpSaw := applyEnvelopeWithHold env holdTime (highpass 1000.0 0.707 44100.0 (sawtooth 220.0))
  player.play (renderDSignalClipped cdQuality (scaleD 0.3 hpSaw))

def main : IO Unit := do
  IO.println "Fugue Demo - Sound Synthesis Library"
  IO.println "====================================\n"

  -- Initialize audio
  AudioPlayer.init
  let player ← AudioPlayer.create 44100.0

  -- Run demos
  playChord player
  IO.println ""

  playArpeggio player
  IO.println ""

  playWaveforms player
  IO.println ""

  playBass player
  IO.println ""

  playEffects player
  IO.println ""

  playFilters player
  IO.println ""

  IO.println "Demo complete!"
