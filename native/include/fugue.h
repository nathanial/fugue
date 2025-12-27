#ifndef FUGUE_H
#define FUGUE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque handle to audio player
typedef struct FugueAudioPlayer* FugueAudioPlayerRef;

// Result codes
typedef enum {
    FUGUE_OK = 0,
    FUGUE_ERROR_INIT_FAILED = 1,
    FUGUE_ERROR_PLAYER_FAILED = 2,
    FUGUE_ERROR_PLAYBACK_FAILED = 3,
} FugueResult;

// Initialize audio subsystem (call once)
FugueResult fugue_audio_init(void);

// Create an audio player with given sample rate
FugueResult fugue_audio_player_create(float sample_rate, FugueAudioPlayerRef* out);

// Destroy an audio player
void fugue_audio_player_destroy(FugueAudioPlayerRef player);

// Play samples (blocking - waits for completion)
FugueResult fugue_audio_player_play(FugueAudioPlayerRef player,
                                    const float* samples,
                                    size_t count);

// Play samples (non-blocking)
FugueResult fugue_audio_player_play_async(FugueAudioPlayerRef player,
                                          const float* samples,
                                          size_t count);

// Wait for async playback to complete
void fugue_audio_player_wait(FugueAudioPlayerRef player);

// Stop playback immediately
void fugue_audio_player_stop(FugueAudioPlayerRef player);

// Check if currently playing
bool fugue_audio_player_is_playing(FugueAudioPlayerRef player);

#ifdef __cplusplus
}
#endif

#endif // FUGUE_H
