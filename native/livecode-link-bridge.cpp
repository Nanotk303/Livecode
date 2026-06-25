#include <ableton/Link.hpp>

#include <chrono>
#include <cmath>
#include <memory>
#include <mutex>

namespace
{
std::mutex gMutex;
std::unique_ptr<ableton::Link> gLink;

ableton::Link& ensureLink(double tempo = 120.0)
{
  std::lock_guard<std::mutex> lock(gMutex);
  if (!gLink)
  {
    gLink = std::make_unique<ableton::Link>(tempo);
  }
  return *gLink;
}

std::chrono::microseconds now()
{
  return ensureLink().clock().micros();
}
} // namespace

extern "C"
{
int livecode_link_create(double tempo)
{
  try
  {
    ensureLink(tempo);
    return 1;
  }
  catch (...)
  {
    return 0;
  }
}

void livecode_link_destroy()
{
  std::lock_guard<std::mutex> lock(gMutex);
  gLink.reset();
}

void livecode_link_enable(int enabled)
{
  ensureLink().enable(enabled != 0);
}

int livecode_link_is_enabled()
{
  return ensureLink().isEnabled() ? 1 : 0;
}

void livecode_link_enable_start_stop(int enabled)
{
  ensureLink().enableStartStopSync(enabled != 0);
}

int livecode_link_is_start_stop_enabled()
{
  return ensureLink().isStartStopSyncEnabled() ? 1 : 0;
}

int livecode_link_num_peers()
{
  return static_cast<int>(ensureLink().numPeers());
}

double livecode_link_tempo()
{
  auto& link = ensureLink();
  return link.captureAppSessionState().tempo();
}

void livecode_link_set_tempo(double tempo)
{
  auto& link = ensureLink(tempo);
  auto state = link.captureAppSessionState();
  state.setTempo(tempo, link.clock().micros());
  link.commitAppSessionState(state);
}

double livecode_link_beat(double quantum)
{
  auto& link = ensureLink();
  const auto time = link.clock().micros();
  auto state = link.captureAppSessionState();
  return state.beatAtTime(time, quantum);
}

double livecode_link_phase(double quantum)
{
  auto& link = ensureLink();
  const auto time = link.clock().micros();
  auto state = link.captureAppSessionState();
  return state.phaseAtTime(time, quantum);
}

double livecode_link_seconds_until_beat(double beat, double quantum)
{
  auto& link = ensureLink();
  const auto currentTime = link.clock().micros();
  auto state = link.captureAppSessionState();
  const auto targetTime = state.timeAtBeat(beat, quantum);
  return std::chrono::duration<double>(targetTime - currentTime).count();
}

int livecode_link_is_playing()
{
  auto& link = ensureLink();
  auto state = link.captureAppSessionState();
  return state.isPlaying() ? 1 : 0;
}

void livecode_link_start_playing(double quantum)
{
  auto& link = ensureLink();
  auto state = link.captureAppSessionState();
  const auto time = link.clock().micros();
  const double currentBeat = state.beatAtTime(time, quantum);
  const double targetBeat = std::ceil(currentBeat / quantum) * quantum;
  state.setIsPlayingAndRequestBeatAtTime(true, time, targetBeat, quantum);
  link.commitAppSessionState(state);
}

void livecode_link_stop_playing()
{
  auto& link = ensureLink();
  auto state = link.captureAppSessionState();
  state.setIsPlaying(false, link.clock().micros());
  link.commitAppSessionState(state);
}
}
