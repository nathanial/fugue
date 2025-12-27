/-
  Fugue.Render.Config - Audio rendering configuration

  Settings for sample rate and audio quality.
-/

namespace Fugue.Render

/-- Audio rendering configuration. -/
structure Config where
  /-- Sample rate in Hz (samples per second). -/
  sampleRate : Float := 44100.0
  deriving Repr, Inhabited

namespace Config

/-- Standard CD quality (44.1 kHz). -/
def cdQuality : Config := { sampleRate := 44100.0 }

/-- DVD/professional quality (48 kHz). -/
def dvdQuality : Config := { sampleRate := 48000.0 }

/-- High definition (96 kHz). -/
def hdQuality : Config := { sampleRate := 96000.0 }

/-- Low quality for testing (22.05 kHz). -/
def lowQuality : Config := { sampleRate := 22050.0 }

/-- Time between samples in seconds. -/
def samplePeriod (config : Config) : Float :=
  1.0 / config.sampleRate

/-- Number of samples for a given duration. -/
def samplesFor (config : Config) (duration : Float) : Nat :=
  (duration * config.sampleRate).toUInt64.toNat

end Config

/-- Default configuration (CD quality). -/
def cdQuality : Config := Config.cdQuality

/-- DVD quality configuration. -/
def dvdQuality : Config := Config.dvdQuality

end Fugue.Render
