/-
  Fugue.Theory.Scale - Musical scales and modes

  Generates scales as lists of MIDI notes or frequencies.
-/
import Fugue.Theory.Note

namespace Fugue.Theory

/-- Scale type defined by its interval pattern (semitones from root). -/
structure Scale where
  /-- Name of the scale -/
  name : String
  /-- Intervals from root in semitones (including 0 for root) -/
  intervals : List Nat
  deriving Repr, Inhabited

/-- Major scale intervals: W-W-H-W-W-W-H -/
def Scale.major : Scale :=
  { name := "Major", intervals := [0, 2, 4, 5, 7, 9, 11] }

/-- Natural minor scale intervals: W-H-W-W-H-W-W -/
def Scale.minor : Scale :=
  { name := "Minor", intervals := [0, 2, 3, 5, 7, 8, 10] }

/-- Harmonic minor scale (raised 7th). -/
def Scale.harmonicMinor : Scale :=
  { name := "Harmonic Minor", intervals := [0, 2, 3, 5, 7, 8, 11] }

/-- Melodic minor scale (ascending form). -/
def Scale.melodicMinor : Scale :=
  { name := "Melodic Minor", intervals := [0, 2, 3, 5, 7, 9, 11] }

/-- Major pentatonic scale. -/
def Scale.majorPentatonic : Scale :=
  { name := "Major Pentatonic", intervals := [0, 2, 4, 7, 9] }

/-- Minor pentatonic scale. -/
def Scale.minorPentatonic : Scale :=
  { name := "Minor Pentatonic", intervals := [0, 3, 5, 7, 10] }

/-- Blues scale (minor pentatonic + blue note). -/
def Scale.blues : Scale :=
  { name := "Blues", intervals := [0, 3, 5, 6, 7, 10] }

/-- Chromatic scale (all 12 semitones). -/
def Scale.chromatic : Scale :=
  { name := "Chromatic", intervals := List.range 12 }

/-- Whole tone scale. -/
def Scale.wholeTone : Scale :=
  { name := "Whole Tone", intervals := [0, 2, 4, 6, 8, 10] }

/-- Diminished scale (half-whole pattern). -/
def Scale.diminished : Scale :=
  { name := "Diminished", intervals := [0, 1, 3, 4, 6, 7, 9, 10] }

/-- Dorian mode (minor with raised 6th). -/
def Scale.dorian : Scale :=
  { name := "Dorian", intervals := [0, 2, 3, 5, 7, 9, 10] }

/-- Phrygian mode (minor with lowered 2nd). -/
def Scale.phrygian : Scale :=
  { name := "Phrygian", intervals := [0, 1, 3, 5, 7, 8, 10] }

/-- Lydian mode (major with raised 4th). -/
def Scale.lydian : Scale :=
  { name := "Lydian", intervals := [0, 2, 4, 6, 7, 9, 11] }

/-- Mixolydian mode (major with lowered 7th). -/
def Scale.mixolydian : Scale :=
  { name := "Mixolydian", intervals := [0, 2, 4, 5, 7, 9, 10] }

/-- Locrian mode (diminished). -/
def Scale.locrian : Scale :=
  { name := "Locrian", intervals := [0, 1, 3, 5, 6, 8, 10] }

/-- Generate MIDI notes for a scale starting from a root note.
    Returns one octave of the scale. -/
def Scale.toMidi (scale : Scale) (root : Nat) : List Nat :=
  scale.intervals.map (· + root)

/-- Generate MIDI notes for a scale with multiple octaves. -/
def Scale.toMidiOctaves (scale : Scale) (root : Nat) (octaves : Nat := 1) : List Nat :=
  let oneOctave := scale.intervals
  (List.range octaves).flatMap fun oct =>
    oneOctave.map (· + root + oct * 12)

/-- Generate frequencies for a scale starting from a root note. -/
def Scale.toFreq (scale : Scale) (root : Nat) (tuning : Float := concertPitch) : List Float :=
  scale.toMidi root |>.map (midiToFreq · tuning)

/-- Generate frequencies for a scale with multiple octaves. -/
def Scale.toFreqOctaves (scale : Scale) (root : Nat) (octaves : Nat := 1)
    (tuning : Float := concertPitch) : List Float :=
  scale.toMidiOctaves root octaves |>.map (midiToFreq · tuning)

/-- Generate a scale from a Note. -/
def Scale.fromNote (scale : Scale) (root : Note) : List Note :=
  scale.toMidi root.toMidi |>.map Note.fromMidi

/-- Get the degree (1-indexed) of a note within a scale.
    Returns none if the note is not in the scale. -/
def Scale.degree (scale : Scale) (root : Nat) (note : Nat) : Option Nat :=
  let interval := (note - root) % 12
  scale.intervals.findIdx? (· == interval) |>.map (· + 1)

/-- Check if a MIDI note is in the scale. -/
def Scale.contains (scale : Scale) (root : Nat) (note : Nat) : Bool :=
  let interval := (note - root) % 12
  scale.intervals.contains interval

/-- Get the number of notes in one octave of the scale. -/
def Scale.size (scale : Scale) : Nat := scale.intervals.length

/-- Create a custom scale from interval pattern.
    Example: [2, 2, 1, 2, 2, 2, 1] for major scale. -/
def Scale.fromPattern (name : String) (pattern : List Nat) : Scale :=
  -- Manual scanl: accumulate sums starting from 0
  let intervals := Id.run do
    let mut result := [0]
    let mut acc := 0
    for step in pattern do
      acc := acc + step
      result := result ++ [acc]
    result
  { name := name, intervals := intervals }

/-- Generate all modes of a scale. -/
def Scale.modes (scale : Scale) : List Scale := Id.run do
  let n := scale.intervals.length
  let mut result := []
  for i in [:n] do
    let rotated := List.range n |>.map fun j =>
      let idx := (i + j) % n
      (scale.intervals[idx]! - scale.intervals[i]! + 12) % 12
    result := result ++ [{ name := s!"{scale.name} mode {i + 1}", intervals := rotated.eraseDups }]
  result

/-- Common scale patterns for reference. -/
def ScalePattern.major : List Nat := [2, 2, 1, 2, 2, 2, 1]
def ScalePattern.minor : List Nat := [2, 1, 2, 2, 1, 2, 2]
def ScalePattern.pentatonicMajor : List Nat := [2, 2, 3, 2, 3]
def ScalePattern.pentatonicMinor : List Nat := [3, 2, 2, 3, 2]

end Fugue.Theory
