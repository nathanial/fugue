/-
  Fugue.Theory.Tempo - Tempo, timing, and rhythm utilities

  Convert between BPM, beat durations, and musical time.
-/
namespace Fugue.Theory

/-- Beats per minute. -/
abbrev BPM := Float

/-- Duration of one beat in seconds at a given BPM. -/
def beatDuration (bpm : BPM) : Float :=
  60.0 / bpm

/-- Duration of a measure in seconds (based on beats per measure). -/
def measureDuration (bpm : BPM) (beatsPerMeasure : Nat := 4) : Float :=
  beatDuration bpm * beatsPerMeasure.toFloat

/-- Note value as a fraction of a whole note. -/
inductive NoteValue
  | whole          -- 1
  | half           -- 1/2
  | quarter        -- 1/4
  | eighth         -- 1/8
  | sixteenth      -- 1/16
  | thirtySecond   -- 1/32
  | dottedWhole    -- 1.5
  | dottedHalf     -- 3/4
  | dottedQuarter  -- 3/8
  | dottedEighth   -- 3/16
  | tripletQuarter -- 1/6 (quarter note triplet)
  | tripletEighth  -- 1/12 (eighth note triplet)
  deriving Repr, BEq, Inhabited

/-- Get the duration multiplier for a note value (relative to whole note). -/
def NoteValue.multiplier : NoteValue → Float
  | .whole => 1.0
  | .half => 0.5
  | .quarter => 0.25
  | .eighth => 0.125
  | .sixteenth => 0.0625
  | .thirtySecond => 0.03125
  | .dottedWhole => 1.5
  | .dottedHalf => 0.75
  | .dottedQuarter => 0.375
  | .dottedEighth => 0.1875
  | .tripletQuarter => 1.0 / 6.0
  | .tripletEighth => 1.0 / 12.0

/-- Duration of a note value in seconds at a given BPM.
    Assumes 4/4 time (quarter note = 1 beat). -/
def noteDuration (bpm : BPM) (noteValue : NoteValue) : Float :=
  let wholeNoteDuration := beatDuration bpm * 4.0  -- 4 quarter notes
  wholeNoteDuration * noteValue.multiplier

/-- Duration of a note value in beats. -/
def noteBeats (noteValue : NoteValue) : Float :=
  noteValue.multiplier * 4.0  -- Relative to quarter note = 1 beat

/-- Time signature representation. -/
structure TimeSignature where
  /-- Beats per measure (numerator) -/
  numerator : Nat
  /-- Note value that gets one beat (denominator) -/
  denominator : Nat
  deriving Repr, BEq, Inhabited

/-- Common time signatures. -/
def TimeSig.common : TimeSignature := { numerator := 4, denominator := 4 }     -- 4/4
def TimeSig.waltz : TimeSignature := { numerator := 3, denominator := 4 }      -- 3/4
def TimeSig.cut : TimeSignature := { numerator := 2, denominator := 2 }        -- 2/2
def TimeSig.sixEight : TimeSignature := { numerator := 6, denominator := 8 }   -- 6/8
def TimeSig.fiveFour : TimeSignature := { numerator := 5, denominator := 4 }   -- 5/4
def TimeSig.sevenEight : TimeSignature := { numerator := 7, denominator := 8 } -- 7/8

/-- Duration of one beat in a time signature (relative to quarter note at BPM). -/
def TimeSignature.beatDuration (_ts : TimeSignature) (bpm : BPM) : Float :=
  -- BPM refers to the denominator note value
  60.0 / bpm

/-- Duration of one measure in a time signature. -/
def TimeSignature.measureDuration (ts : TimeSignature) (bpm : BPM) : Float :=
  ts.beatDuration bpm * ts.numerator.toFloat

/-- Convert a bar/beat position to seconds. -/
def barBeatToSeconds (bpm : BPM) (bar : Nat) (beat : Float)
    (beatsPerMeasure : Nat := 4) : Float :=
  let beatDur := beatDuration bpm
  (bar.toFloat * beatsPerMeasure.toFloat + beat) * beatDur

/-- Convert seconds to bar/beat position. -/
def secondsToBarBeat (bpm : BPM) (seconds : Float)
    (beatsPerMeasure : Nat := 4) : Nat × Float :=
  let beatDur := beatDuration bpm
  let totalBeats := seconds / beatDur
  let bar := (totalBeats / beatsPerMeasure.toFloat).toUInt64.toNat
  let beat := totalBeats - bar.toFloat * beatsPerMeasure.toFloat
  (bar, beat)

/-- Quantize a time to the nearest beat. -/
def quantizeToBeat (bpm : BPM) (seconds : Float) : Float :=
  let beatDur := beatDuration bpm
  let beats := seconds / beatDur
  Float.round beats * beatDur

/-- Quantize to a specific note value grid. -/
def quantizeToGrid (bpm : BPM) (noteValue : NoteValue) (seconds : Float) : Float :=
  let gridDur := noteDuration bpm noteValue
  let gridPos := seconds / gridDur
  Float.round gridPos * gridDur

/-- Calculate swing timing offset for a note.
    Swing ratio: 1.0 = straight, 2.0 = triplet swing (2:1), 1.5 = moderate swing. -/
def swingOffset (bpm : BPM) (swingRatio : Float) (isOffbeat : Bool) : Float :=
  if isOffbeat then
    let eighthDur := noteDuration bpm .eighth
    -- Offset is the difference from straight 8ths
    let swungDur := eighthDur * swingRatio / (1.0 + swingRatio) * 2.0
    swungDur - eighthDur
  else
    0.0

/-- Common tempo markings. -/
def Tempo.largo : BPM := 50.0
def Tempo.adagio : BPM := 70.0
def Tempo.andante : BPM := 90.0
def Tempo.moderato : BPM := 110.0
def Tempo.allegro : BPM := 130.0
def Tempo.vivace : BPM := 160.0
def Tempo.presto : BPM := 180.0

/-- Generate a list of beat times for a given duration. -/
def beatTimes (bpm : BPM) (duration : Float) : List Float := Id.run do
  let beatDur := beatDuration bpm
  let numBeats := (duration / beatDur).toUInt64.toNat
  let mut times := []
  for i in [:numBeats] do
    times := times ++ [i.toFloat * beatDur]
  times

/-- Generate subdivision times (e.g., 16th notes) for a given duration. -/
def subdivisionTimes (bpm : BPM) (noteValue : NoteValue) (duration : Float) : List Float := Id.run do
  let noteDur := noteDuration bpm noteValue
  let numNotes := (duration / noteDur).toUInt64.toNat
  let mut times := []
  for i in [:numNotes] do
    times := times ++ [i.toFloat * noteDur]
  times

/-- Calculate delay time for a tempo-synced effect (in seconds). -/
def syncedDelay (bpm : BPM) (noteValue : NoteValue) : Float :=
  noteDuration bpm noteValue

/-- Calculate LFO rate for tempo-synced modulation (in Hz). -/
def syncedLfoRate (bpm : BPM) (noteValue : NoteValue) : Float :=
  1.0 / noteDuration bpm noteValue

/-- Tap tempo: calculate BPM from list of tap times (in seconds). -/
def tapTempo (tapTimes : List Float) : Option BPM :=
  if tapTimes.length < 2 then none
  else
    let diffs := List.zipWith (fun a b => b - a) tapTimes (tapTimes.drop 1)
    let avgDiff := diffs.foldl (· + ·) 0.0 / diffs.length.toFloat
    if avgDiff > 0.0 then some (60.0 / avgDiff) else none

end Fugue.Theory
