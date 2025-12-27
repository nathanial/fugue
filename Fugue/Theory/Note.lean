/-
  Fugue.Theory.Note - MIDI notes and note name parsing

  Convert between MIDI note numbers, note names, and frequencies.
  Standard tuning: A4 = 440 Hz, MIDI note 69.
-/
namespace Fugue.Theory

/-- Standard concert pitch (A4 = 440 Hz). -/
def concertPitch : Float := 440.0

/-- MIDI note number for A4. -/
def a4Midi : Nat := 69

/-- Convert MIDI note number to frequency in Hz.
    Uses equal temperament tuning with A4 = 440 Hz.

    Formula: freq = 440 * 2^((midi - 69) / 12) -/
def midiToFreq (midi : Nat) (tuning : Float := concertPitch) : Float :=
  tuning * Float.pow 2.0 ((midi.toFloat - a4Midi.toFloat) / 12.0)

/-- Convert frequency to MIDI note number (rounded to nearest).
    Returns the closest MIDI note. -/
def freqToMidi (freq : Float) (tuning : Float := concertPitch) : Nat :=
  let midiFloat := 12.0 * (Float.log (freq / tuning) / Float.log 2.0) + a4Midi.toFloat
  midiFloat.toUInt64.toNat

/-- Note name without octave. -/
inductive NoteName
  | C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving Repr, BEq, Inhabited

/-- Get the semitone offset (0-11) for a note name. -/
def NoteName.semitone : NoteName → Nat
  | .C => 0 | .Cs => 1 | .D => 2 | .Ds => 3
  | .E => 4 | .F => 5 | .Fs => 6 | .G => 7
  | .Gs => 8 | .A => 9 | .As => 10 | .B => 11

/-- Get note name from semitone offset (0-11). -/
def NoteName.fromSemitone (s : Nat) : NoteName :=
  match s % 12 with
  | 0 => .C | 1 => .Cs | 2 => .D | 3 => .Ds
  | 4 => .E | 5 => .F | 6 => .Fs | 7 => .G
  | 8 => .Gs | 9 => .A | 10 => .As | _ => .B

/-- Convert note name to string (sharp notation). -/
def NoteName.toString : NoteName → String
  | .C => "C" | .Cs => "C#" | .D => "D" | .Ds => "D#"
  | .E => "E" | .F => "F" | .Fs => "F#" | .G => "G"
  | .Gs => "G#" | .A => "A" | .As => "A#" | .B => "B"

/-- Convert note name to string (flat notation). -/
def NoteName.toStringFlat : NoteName → String
  | .C => "C" | .Cs => "Db" | .D => "D" | .Ds => "Eb"
  | .E => "E" | .F => "F" | .Fs => "Gb" | .G => "G"
  | .Gs => "Ab" | .A => "A" | .As => "Bb" | .B => "B"

instance : ToString NoteName := ⟨NoteName.toString⟩

/-- A complete note with name and octave. -/
structure Note where
  name : NoteName
  octave : Int
  deriving Repr, BEq, Inhabited

/-- Convert a Note to MIDI note number.
    C4 = MIDI 60, A4 = MIDI 69. -/
def Note.toMidi (note : Note) : Nat :=
  let semitone := note.name.semitone
  let midiBase := (note.octave + 1) * 12  -- C-1 = MIDI 0
  (midiBase + semitone).toNat

/-- Convert a Note to frequency in Hz. -/
def Note.toFreq (note : Note) (tuning : Float := concertPitch) : Float :=
  midiToFreq note.toMidi tuning

/-- Create a Note from MIDI note number. -/
def Note.fromMidi (midi : Nat) : Note :=
  let octave := (midi / 12 : Nat).toUInt64.toNat - 1
  let semitone := midi % 12
  { name := NoteName.fromSemitone semitone, octave := octave }

/-- Convert Note to string (e.g., "C4", "F#3"). -/
def Note.toString (note : Note) : String :=
  s!"{note.name}{note.octave}"

instance : ToString Note := ⟨Note.toString⟩

/-- Parse a single character to NoteName base (C-G, A-B). -/
private def parseNoteLetter (c : Char) : Option NoteName :=
  match c.toLower with
  | 'c' => some .C | 'd' => some .D | 'e' => some .E
  | 'f' => some .F | 'g' => some .G | 'a' => some .A
  | 'b' => some .B | _ => none

/-- Apply sharp or flat modifier to a note name. -/
private def applyModifier (note : NoteName) (modifier : Char) : NoteName :=
  let semitone := note.semitone
  match modifier with
  | '#' | 's' => NoteName.fromSemitone ((semitone + 1) % 12)
  | 'b' => NoteName.fromSemitone ((semitone + 11) % 12)  -- -1 mod 12
  | _ => note

/-- Parse a note name string like "C4", "F#3", "Bb5", "Ds2".
    Returns none if parsing fails.

    Supported formats:
    - "C4" - natural note
    - "C#4", "Cs4" - sharp
    - "Db4", "Db4" - flat -/
def parseNote (s : String) : Option Note := do
  let chars := s.toList
  if chars.isEmpty then none else
  -- Parse note letter
  let baseName ← parseNoteLetter chars[0]!
  -- Check for modifier and octave
  let rest := chars.drop 1
  if rest.isEmpty then none else
  let (noteName, octaveChars) :=
    if rest.length > 0 && (rest[0]! == '#' || rest[0]! == 'b' ||
       rest[0]! == 's' || rest[0]! == 'S') then
      (applyModifier baseName rest[0]!, rest.drop 1)
    else
      (baseName, rest)
  -- Parse octave (can be negative)
  let octaveStr := String.ofList octaveChars
  let octave ← octaveStr.toInt?
  some { name := noteName, octave := octave }

/-- Parse note or return default (A4). -/
def parseNoteOrDefault (s : String) (default : Note := ⟨.A, 4⟩) : Note :=
  parseNote s |>.getD default

/-- Common note frequencies (equal temperament, A4 = 440 Hz). -/
def Freq.c0 : Float := midiToFreq 12
def Freq.c1 : Float := midiToFreq 24
def Freq.c2 : Float := midiToFreq 36
def Freq.c3 : Float := midiToFreq 48
def Freq.c4 : Float := midiToFreq 60   -- Middle C
def Freq.c5 : Float := midiToFreq 72
def Freq.c6 : Float := midiToFreq 84
def Freq.c7 : Float := midiToFreq 96
def Freq.a0 : Float := midiToFreq 21
def Freq.a1 : Float := midiToFreq 33
def Freq.a2 : Float := midiToFreq 45
def Freq.a3 : Float := midiToFreq 57
def Freq.a4 : Float := 440.0           -- Concert A
def Freq.a5 : Float := midiToFreq 81
def Freq.a6 : Float := midiToFreq 93
def Freq.middleC : Float := Freq.c4
def Freq.concertA : Float := Freq.a4

/-- Interval in semitones. -/
def Interval.unison : Nat := 0
def Interval.minorSecond : Nat := 1
def Interval.majorSecond : Nat := 2
def Interval.minorThird : Nat := 3
def Interval.majorThird : Nat := 4
def Interval.perfectFourth : Nat := 5
def Interval.tritone : Nat := 6
def Interval.perfectFifth : Nat := 7
def Interval.minorSixth : Nat := 8
def Interval.majorSixth : Nat := 9
def Interval.minorSeventh : Nat := 10
def Interval.majorSeventh : Nat := 11
def Interval.octave : Nat := 12

/-- Transpose a MIDI note by an interval (in semitones). -/
def transpose (midi : Nat) (semitones : Int) : Nat :=
  ((midi : Int) + semitones).toNat

/-- Transpose a Note by an interval. -/
def Note.transpose (note : Note) (semitones : Int) : Note :=
  Note.fromMidi (Theory.transpose note.toMidi semitones)

/-- Get the interval between two MIDI notes. -/
def intervalBetween (midi1 midi2 : Nat) : Int :=
  (midi2 : Int) - (midi1 : Int)

end Fugue.Theory
