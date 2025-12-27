/-
  Fugue.Render.Render - Signal to sample buffer rendering

  Convert continuous signals to discrete samples for playback.
-/
import Fugue.Core.Signal
import Fugue.Core.Duration
import Fugue.Render.Config
import Init.Data.FloatArray

namespace Fugue.Render

/-- Absolute value of a float. -/
@[inline]
private def absFloat (x : Float) : Float := if x < 0.0 then -x else x

/-- Maximum of two floats. -/
@[inline]
private def maxFloat (a b : Float) : Float := if a > b then a else b

/-- Render a signal to a FloatArray for a given duration.
    Samples the signal at regular intervals determined by sample rate. -/
def renderSignal (config : Config) (duration : Float) (sig : Signal Float) : FloatArray := Id.run do
  let numSamples := config.samplesFor duration
  let dt := config.samplePeriod
  let mut arr := FloatArray.empty
  for i in [:numSamples] do
    let t := i.toFloat * dt
    arr := arr.push (sig.sample t)
  arr

/-- Render a duration-aware signal. -/
def renderDSignal (config : Config) (sig : DSignal Float) : FloatArray :=
  renderSignal config sig.duration sig.signal

/-- Render with clipping to [-1, 1] range. -/
def renderClipped (config : Config) (duration : Float) (sig : Signal Float) : FloatArray := Id.run do
  let numSamples := config.samplesFor duration
  let dt := config.samplePeriod
  let mut arr := FloatArray.empty
  for i in [:numSamples] do
    let t := i.toFloat * dt
    let v := sig.sample t
    let clipped := if v > 1.0 then 1.0 else if v < -1.0 then -1.0 else v
    arr := arr.push clipped
  arr

/-- Render a DSignal with clipping. -/
def renderDSignalClipped (config : Config) (sig : DSignal Float) : FloatArray :=
  renderClipped config sig.duration sig.signal

/-- Calculate the duration of a sample buffer. -/
def bufferDuration (config : Config) (buffer : FloatArray) : Float :=
  buffer.size.toFloat / config.sampleRate

/-- Get peak amplitude from a buffer. -/
def peakAmplitude (buffer : FloatArray) : Float := Id.run do
  let mut peak := 0.0
  for i in [:buffer.size] do
    let v := absFloat (buffer.get! i)
    if v > peak then peak := v
  peak

/-- Normalize buffer to have peak amplitude of 1.0. -/
def normalize (buffer : FloatArray) : FloatArray := Id.run do
  let peak := peakAmplitude buffer
  if peak == 0.0 then return buffer
  let factor := 1.0 / peak
  let mut arr := FloatArray.empty
  for i in [:buffer.size] do
    arr := arr.push (buffer.get! i * factor)
  arr

/-- Scale buffer by a factor. -/
def scaleBuffer (factor : Float) (buffer : FloatArray) : FloatArray := Id.run do
  let mut arr := FloatArray.empty
  for i in [:buffer.size] do
    arr := arr.push (buffer.get! i * factor)
  arr

end Fugue.Render
