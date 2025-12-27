/-
  Fugue.Theory.Chord - Chord types and voicings

  Generate chords as lists of MIDI notes or frequencies.
-/
import Fugue.Theory.Note

namespace Fugue.Theory

/-- Chord type defined by intervals from root (in semitones). -/
structure ChordType where
  /-- Name of the chord type -/
  name : String
  /-- Symbol used in chord notation (e.g., "m", "7", "maj7") -/
  symbol : String
  /-- Intervals from root in semitones (including 0 for root) -/
  intervals : List Nat
  deriving Repr, Inhabited

-- Triads

/-- Major triad: 1-3-5 -/
def ChordType.major : ChordType :=
  { name := "Major", symbol := "", intervals := [0, 4, 7] }

/-- Minor triad: 1-b3-5 -/
def ChordType.minor : ChordType :=
  { name := "Minor", symbol := "m", intervals := [0, 3, 7] }

/-- Diminished triad: 1-b3-b5 -/
def ChordType.diminished : ChordType :=
  { name := "Diminished", symbol := "dim", intervals := [0, 3, 6] }

/-- Augmented triad: 1-3-#5 -/
def ChordType.augmented : ChordType :=
  { name := "Augmented", symbol := "aug", intervals := [0, 4, 8] }

/-- Suspended 2nd: 1-2-5 -/
def ChordType.sus2 : ChordType :=
  { name := "Suspended 2nd", symbol := "sus2", intervals := [0, 2, 7] }

/-- Suspended 4th: 1-4-5 -/
def ChordType.sus4 : ChordType :=
  { name := "Suspended 4th", symbol := "sus4", intervals := [0, 5, 7] }

-- Seventh Chords

/-- Major 7th: 1-3-5-7 -/
def ChordType.major7 : ChordType :=
  { name := "Major 7th", symbol := "maj7", intervals := [0, 4, 7, 11] }

/-- Minor 7th: 1-b3-5-b7 -/
def ChordType.minor7 : ChordType :=
  { name := "Minor 7th", symbol := "m7", intervals := [0, 3, 7, 10] }

/-- Dominant 7th: 1-3-5-b7 -/
def ChordType.dominant7 : ChordType :=
  { name := "Dominant 7th", symbol := "7", intervals := [0, 4, 7, 10] }

/-- Diminished 7th: 1-b3-b5-bb7 -/
def ChordType.diminished7 : ChordType :=
  { name := "Diminished 7th", symbol := "dim7", intervals := [0, 3, 6, 9] }

/-- Half-diminished 7th (minor 7 flat 5): 1-b3-b5-b7 -/
def ChordType.halfDiminished7 : ChordType :=
  { name := "Half-Diminished 7th", symbol := "m7b5", intervals := [0, 3, 6, 10] }

/-- Minor-major 7th: 1-b3-5-7 -/
def ChordType.minorMajor7 : ChordType :=
  { name := "Minor-Major 7th", symbol := "mMaj7", intervals := [0, 3, 7, 11] }

/-- Augmented 7th: 1-3-#5-b7 -/
def ChordType.augmented7 : ChordType :=
  { name := "Augmented 7th", symbol := "aug7", intervals := [0, 4, 8, 10] }

-- Extended Chords

/-- Add 9 (major triad + 9): 1-3-5-9 -/
def ChordType.add9 : ChordType :=
  { name := "Add 9", symbol := "add9", intervals := [0, 4, 7, 14] }

/-- Major 9th: 1-3-5-7-9 -/
def ChordType.major9 : ChordType :=
  { name := "Major 9th", symbol := "maj9", intervals := [0, 4, 7, 11, 14] }

/-- Dominant 9th: 1-3-5-b7-9 -/
def ChordType.dominant9 : ChordType :=
  { name := "Dominant 9th", symbol := "9", intervals := [0, 4, 7, 10, 14] }

/-- Minor 9th: 1-b3-5-b7-9 -/
def ChordType.minor9 : ChordType :=
  { name := "Minor 9th", symbol := "m9", intervals := [0, 3, 7, 10, 14] }

/-- Power chord (5th): 1-5 -/
def ChordType.power : ChordType :=
  { name := "Power Chord", symbol := "5", intervals := [0, 7] }

/-- Power chord with octave: 1-5-8 -/
def ChordType.powerOctave : ChordType :=
  { name := "Power Chord w/Octave", symbol := "5", intervals := [0, 7, 12] }

/-- A chord instance with root and type. -/
structure Chord where
  /-- Root note as MIDI number -/
  root : Nat
  /-- Chord type -/
  chordType : ChordType
  deriving Repr, Inhabited

/-- Create a chord from a Note and ChordType. -/
def Chord.fromNote (note : Note) (ct : ChordType) : Chord :=
  { root := note.toMidi, chordType := ct }

/-- Get MIDI notes for a chord. -/
def Chord.toMidi (chord : Chord) : List Nat :=
  chord.chordType.intervals.map (· + chord.root)

/-- Get frequencies for a chord. -/
def Chord.toFreq (chord : Chord) (tuning : Float := concertPitch) : List Float :=
  chord.toMidi.map (midiToFreq · tuning)

/-- Get Notes for a chord. -/
def Chord.toNotes (chord : Chord) : List Note :=
  chord.toMidi.map Note.fromMidi

/-- Get chord name (e.g., "Cmaj7", "F#m"). -/
def Chord.name (chord : Chord) : String :=
  let rootNote := Note.fromMidi chord.root
  s!"{rootNote.name}{chord.chordType.symbol}"

instance : ToString Chord := ⟨Chord.name⟩

/-- Invert a chord (move lowest note up an octave). -/
def Chord.invert (chord : Chord) (inversion : Nat := 1) : List Nat :=
  let notes := chord.toMidi
  if notes.isEmpty then [] else
  let n := inversion % notes.length
  let (low, high) := notes.splitAt n
  high ++ low.map (· + 12)

/-- Get a specific inversion as MIDI notes. -/
def Chord.inversion (chord : Chord) (n : Nat) : List Nat :=
  chord.invert n

/-- Create chord with custom voicing (octave offsets for each note). -/
def Chord.voice (chord : Chord) (octaveOffsets : List Int) : List Nat :=
  let notes := chord.toMidi
  let notesInt : List Int := notes.map Int.ofNat
  List.zipWith (fun note offset => (note + offset * 12).toNat) notesInt octaveOffsets

/-- Drop 2 voicing (move 2nd highest note down an octave). -/
def Chord.drop2 (chord : Chord) : List Nat :=
  let notes := chord.toMidi
  if notes.length < 3 then notes else
  let sorted := notes
  let idx := sorted.length - 2
  sorted.mapIdx fun i n =>
    if i == idx then n - 12 else n

/-- I-IV-V-I in major key -/
def Progression.classic : List (Nat × ChordType) :=
  [(0, ChordType.major), (5, ChordType.major), (7, ChordType.major), (0, ChordType.major)]

/-- I-V-vi-IV (pop progression) -/
def Progression.pop : List (Nat × ChordType) :=
  [(0, ChordType.major), (7, ChordType.major), (9, ChordType.minor), (5, ChordType.major)]

/-- ii-V-I (jazz) -/
def Progression.jazzTwoFiveOne : List (Nat × ChordType) :=
  [(2, ChordType.minor7), (7, ChordType.dominant7), (0, ChordType.major7)]

/-- I-vi-IV-V (50s progression) -/
def Progression.fifties : List (Nat × ChordType) :=
  [(0, ChordType.major), (9, ChordType.minor), (5, ChordType.major), (7, ChordType.major)]

/-- i-bVII-bVI-V (Andalusian cadence) -/
def Progression.andalusian : List (Nat × ChordType) :=
  [(0, ChordType.minor), (10, ChordType.major), (8, ChordType.major), (7, ChordType.major)]

/-- Generate a chord progression from a root MIDI note. -/
def generateProgression (root : Nat) (prog : List (Nat × ChordType)) : List Chord :=
  prog.map fun (interval, ct) =>
    { root := root + interval, chordType := ct }

/-- Generate frequencies for a chord progression. -/
def progressionToFreq (root : Nat) (prog : List (Nat × ChordType))
    (tuning : Float := concertPitch) : List (List Float) :=
  generateProgression root prog |>.map (·.toFreq tuning)

end Fugue.Theory
