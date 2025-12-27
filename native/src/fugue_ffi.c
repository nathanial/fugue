#include <lean/lean.h>
#include "fugue.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef __APPLE__
#include <AudioToolbox/AudioToolbox.h>
#include <pthread.h>

// Number of audio buffers for streaming
#define NUM_BUFFERS 3
#define BUFFER_FRAMES 4096

// AudioQueue player structure
struct FugueAudioPlayer {
    AudioQueueRef queue;
    AudioQueueBufferRef buffers[NUM_BUFFERS];
    float sample_rate;

    // Playback state
    volatile bool is_playing;
    volatile bool should_stop;
    pthread_mutex_t mutex;
    pthread_cond_t done_cond;

    // Sample buffer
    float* sample_data;
    size_t sample_count;
    size_t sample_offset;
};

// AudioQueue callback - fills buffers with audio data
static void audio_queue_callback(void* user_data,
                                 AudioQueueRef queue,
                                 AudioQueueBufferRef buffer) {
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)user_data;

    pthread_mutex_lock(&player->mutex);

    if (player->should_stop || player->sample_data == NULL) {
        // Fill with silence and don't re-enqueue
        memset(buffer->mAudioData, 0, buffer->mAudioDataBytesCapacity);
        buffer->mAudioDataByteSize = buffer->mAudioDataBytesCapacity;
        pthread_mutex_unlock(&player->mutex);
        return;
    }

    size_t frames_available = player->sample_count - player->sample_offset;
    size_t frames_to_copy = buffer->mAudioDataBytesCapacity / sizeof(float);

    if (frames_to_copy > frames_available) {
        frames_to_copy = frames_available;
    }

    if (frames_to_copy > 0) {
        memcpy(buffer->mAudioData,
               player->sample_data + player->sample_offset,
               frames_to_copy * sizeof(float));
        player->sample_offset += frames_to_copy;
    }

    // Zero-pad if we didn't have enough data
    size_t bytes_copied = frames_to_copy * sizeof(float);
    if (bytes_copied < buffer->mAudioDataBytesCapacity) {
        memset((char*)buffer->mAudioData + bytes_copied, 0,
               buffer->mAudioDataBytesCapacity - bytes_copied);
    }

    buffer->mAudioDataByteSize = buffer->mAudioDataBytesCapacity;

    // Check if we're done
    if (player->sample_offset >= player->sample_count) {
        player->is_playing = false;
        pthread_cond_signal(&player->done_cond);
        pthread_mutex_unlock(&player->mutex);

        // Stop the queue immediately
        AudioQueueStop(queue, true);  // true = stop immediately
        return;
    }

    pthread_mutex_unlock(&player->mutex);

    // Re-enqueue the buffer
    AudioQueueEnqueueBuffer(queue, buffer, 0, NULL);
}

// C API implementations
FugueResult fugue_audio_init(void) {
    return FUGUE_OK;
}

FugueResult fugue_audio_player_create(float sample_rate, FugueAudioPlayerRef* out) {
    FugueAudioPlayerRef player = calloc(1, sizeof(struct FugueAudioPlayer));
    if (!player) return FUGUE_ERROR_PLAYER_FAILED;

    player->sample_rate = sample_rate;
    player->is_playing = false;
    player->should_stop = false;
    player->sample_data = NULL;
    player->sample_count = 0;
    player->sample_offset = 0;

    pthread_mutex_init(&player->mutex, NULL);
    pthread_cond_init(&player->done_cond, NULL);

    // Create audio format description (mono float32)
    AudioStreamBasicDescription format = {0};
    format.mSampleRate = sample_rate;
    format.mFormatID = kAudioFormatLinearPCM;
    format.mFormatFlags = kLinearPCMFormatFlagIsFloat | kAudioFormatFlagIsPacked;
    format.mBitsPerChannel = 32;
    format.mChannelsPerFrame = 1;  // Mono
    format.mBytesPerFrame = sizeof(float);
    format.mFramesPerPacket = 1;
    format.mBytesPerPacket = sizeof(float);

    OSStatus status = AudioQueueNewOutput(&format,
                                          audio_queue_callback,
                                          player,
                                          NULL,  // run loop
                                          kCFRunLoopCommonModes,
                                          0,
                                          &player->queue);
    if (status != noErr) {
        pthread_mutex_destroy(&player->mutex);
        pthread_cond_destroy(&player->done_cond);
        free(player);
        return FUGUE_ERROR_PLAYER_FAILED;
    }

    // Allocate audio buffers
    for (int i = 0; i < NUM_BUFFERS; i++) {
        status = AudioQueueAllocateBuffer(player->queue,
                                          BUFFER_FRAMES * sizeof(float),
                                          &player->buffers[i]);
        if (status != noErr) {
            AudioQueueDispose(player->queue, true);
            pthread_mutex_destroy(&player->mutex);
            pthread_cond_destroy(&player->done_cond);
            free(player);
            return FUGUE_ERROR_PLAYER_FAILED;
        }
    }

    *out = player;
    return FUGUE_OK;
}

void fugue_audio_player_destroy(FugueAudioPlayerRef player) {
    if (!player) return;

    // Stop playback
    player->should_stop = true;
    AudioQueueStop(player->queue, true);
    AudioQueueDispose(player->queue, true);

    pthread_mutex_destroy(&player->mutex);
    pthread_cond_destroy(&player->done_cond);

    if (player->sample_data) {
        free(player->sample_data);
    }
    free(player);
}

FugueResult fugue_audio_player_play_async(FugueAudioPlayerRef player,
                                          const float* samples,
                                          size_t count) {
    if (!player || !samples || count == 0) {
        return FUGUE_ERROR_PLAYBACK_FAILED;
    }

    pthread_mutex_lock(&player->mutex);

    // Stop any existing playback
    if (player->is_playing) {
        player->should_stop = true;
        AudioQueueStop(player->queue, true);
    }

    // Reset the queue to clear any pending buffers from previous playback
    AudioQueueReset(player->queue);

    // Free old sample data
    if (player->sample_data) {
        free(player->sample_data);
    }

    // Copy new samples
    player->sample_data = malloc(count * sizeof(float));
    if (!player->sample_data) {
        pthread_mutex_unlock(&player->mutex);
        return FUGUE_ERROR_PLAYBACK_FAILED;
    }
    memcpy(player->sample_data, samples, count * sizeof(float));
    player->sample_count = count;
    player->sample_offset = 0;
    player->should_stop = false;
    player->is_playing = true;

    pthread_mutex_unlock(&player->mutex);

    // Prime the buffers
    for (int i = 0; i < NUM_BUFFERS; i++) {
        audio_queue_callback(player, player->queue, player->buffers[i]);
    }

    // Start playback
    OSStatus status = AudioQueueStart(player->queue, NULL);
    if (status != noErr) {
        pthread_mutex_lock(&player->mutex);
        player->is_playing = false;
        pthread_mutex_unlock(&player->mutex);
        return FUGUE_ERROR_PLAYBACK_FAILED;
    }

    return FUGUE_OK;
}

FugueResult fugue_audio_player_play(FugueAudioPlayerRef player,
                                    const float* samples,
                                    size_t count) {
    FugueResult result = fugue_audio_player_play_async(player, samples, count);
    if (result != FUGUE_OK) return result;

    fugue_audio_player_wait(player);
    return FUGUE_OK;
}

void fugue_audio_player_wait(FugueAudioPlayerRef player) {
    if (!player) return;

    pthread_mutex_lock(&player->mutex);
    while (player->is_playing && !player->should_stop) {
        pthread_cond_wait(&player->done_cond, &player->mutex);
    }
    pthread_mutex_unlock(&player->mutex);
}

void fugue_audio_player_stop(FugueAudioPlayerRef player) {
    if (!player) return;

    pthread_mutex_lock(&player->mutex);
    player->should_stop = true;
    player->is_playing = false;
    pthread_cond_signal(&player->done_cond);
    pthread_mutex_unlock(&player->mutex);

    AudioQueueStop(player->queue, true);
}

bool fugue_audio_player_is_playing(FugueAudioPlayerRef player) {
    if (!player) return false;

    pthread_mutex_lock(&player->mutex);
    bool playing = player->is_playing;
    pthread_mutex_unlock(&player->mutex);
    return playing;
}

#else
// Non-macOS stub implementations
FugueResult fugue_audio_init(void) { return FUGUE_ERROR_INIT_FAILED; }
FugueResult fugue_audio_player_create(float sr, FugueAudioPlayerRef* out) {
    (void)sr; (void)out; return FUGUE_ERROR_INIT_FAILED;
}
void fugue_audio_player_destroy(FugueAudioPlayerRef p) { (void)p; }
FugueResult fugue_audio_player_play(FugueAudioPlayerRef p, const float* s, size_t c) {
    (void)p; (void)s; (void)c; return FUGUE_ERROR_INIT_FAILED;
}
FugueResult fugue_audio_player_play_async(FugueAudioPlayerRef p, const float* s, size_t c) {
    (void)p; (void)s; (void)c; return FUGUE_ERROR_INIT_FAILED;
}
void fugue_audio_player_wait(FugueAudioPlayerRef p) { (void)p; }
void fugue_audio_player_stop(FugueAudioPlayerRef p) { (void)p; }
bool fugue_audio_player_is_playing(FugueAudioPlayerRef p) { (void)p; return false; }
#endif

// ============================================================================
// Lean FFI Bridge
// ============================================================================

static lean_external_class* g_audio_player_class = NULL;

static void audio_player_finalizer(void* ptr) {
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)ptr;
    if (player) {
        fugue_audio_player_destroy(player);
    }
}

static void noop_foreach(void* ptr, b_lean_obj_arg arg) {
    (void)ptr;
    (void)arg;
}

static void ensure_initialized(void) {
    if (g_audio_player_class == NULL) {
        g_audio_player_class = lean_register_external_class(
            audio_player_finalizer, noop_foreach);
    }
}

static lean_object* mk_io_error(const char* msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

// lean_fugue_audio_init : IO Unit
LEAN_EXPORT lean_obj_res lean_fugue_audio_init(lean_obj_arg world) {
    (void)world;
    ensure_initialized();
    FugueResult result = fugue_audio_init();
    if (result != FUGUE_OK) {
        return mk_io_error("Failed to initialize audio subsystem");
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// lean_fugue_audio_player_create : Float -> IO AudioPlayer
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_create(double sample_rate, lean_obj_arg world) {
    (void)world;
    ensure_initialized();

    FugueAudioPlayerRef player = NULL;
    FugueResult result = fugue_audio_player_create((float)sample_rate, &player);

    if (result != FUGUE_OK || player == NULL) {
        return mk_io_error("Failed to create audio player");
    }

    lean_object* obj = lean_alloc_external(g_audio_player_class, player);
    return lean_io_result_mk_ok(obj);
}

// lean_fugue_audio_player_destroy : @& AudioPlayer -> IO Unit
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_destroy(b_lean_obj_arg player_obj, lean_obj_arg world) {
    (void)player_obj;
    (void)world;
    // Destruction handled by finalizer
    return lean_io_result_mk_ok(lean_box(0));
}

// lean_fugue_audio_player_play : @& AudioPlayer -> @& FloatArray -> IO Unit
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_play(b_lean_obj_arg player_obj,
                                                       b_lean_obj_arg samples_obj,
                                                       lean_obj_arg world) {
    (void)world;
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)lean_get_external_data(player_obj);

    size_t count = (size_t)lean_unbox(lean_float_array_size(samples_obj));
    if (count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Lean's FloatArray contains doubles, we need floats
    const double* doubles = lean_float_array_cptr(samples_obj);
    float* floats = malloc(count * sizeof(float));
    if (!floats) {
        return mk_io_error("Failed to allocate sample buffer");
    }

    for (size_t i = 0; i < count; i++) {
        floats[i] = (float)doubles[i];
    }

    FugueResult result = fugue_audio_player_play(player, floats, count);
    free(floats);

    if (result != FUGUE_OK) {
        return mk_io_error("Playback failed");
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// lean_fugue_audio_player_play_async : @& AudioPlayer -> @& FloatArray -> IO Unit
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_play_async(b_lean_obj_arg player_obj,
                                                             b_lean_obj_arg samples_obj,
                                                             lean_obj_arg world) {
    (void)world;
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)lean_get_external_data(player_obj);

    size_t count = (size_t)lean_unbox(lean_float_array_size(samples_obj));
    if (count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    const double* doubles = lean_float_array_cptr(samples_obj);
    float* floats = malloc(count * sizeof(float));
    if (!floats) {
        return mk_io_error("Failed to allocate sample buffer");
    }

    for (size_t i = 0; i < count; i++) {
        floats[i] = (float)doubles[i];
    }

    FugueResult result = fugue_audio_player_play_async(player, floats, count);
    free(floats);

    if (result != FUGUE_OK) {
        return mk_io_error("Async playback failed");
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// lean_fugue_audio_player_wait : @& AudioPlayer -> IO Unit
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_wait(b_lean_obj_arg player_obj, lean_obj_arg world) {
    (void)world;
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)lean_get_external_data(player_obj);
    fugue_audio_player_wait(player);
    return lean_io_result_mk_ok(lean_box(0));
}

// lean_fugue_audio_player_stop : @& AudioPlayer -> IO Unit
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_stop(b_lean_obj_arg player_obj, lean_obj_arg world) {
    (void)world;
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)lean_get_external_data(player_obj);
    fugue_audio_player_stop(player);
    return lean_io_result_mk_ok(lean_box(0));
}

// lean_fugue_audio_player_is_playing : @& AudioPlayer -> IO Bool
LEAN_EXPORT lean_obj_res lean_fugue_audio_player_is_playing(b_lean_obj_arg player_obj, lean_obj_arg world) {
    (void)world;
    FugueAudioPlayerRef player = (FugueAudioPlayerRef)lean_get_external_data(player_obj);
    bool playing = fugue_audio_player_is_playing(player);
    return lean_io_result_mk_ok(lean_box(playing ? 1 : 0));
}
