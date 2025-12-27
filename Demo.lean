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

  IO.println "Demo complete!"
