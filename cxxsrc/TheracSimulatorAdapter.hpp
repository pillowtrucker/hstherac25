#pragma once
// #ifdef _WIN32
// #pragma comment(lib, "hstherac25.dll.a")
// #endif
#include <HsFFI.h>
// #include <godot_cpp/variant/string.hpp>
// #include <shared_mutex>
#include <string>

namespace TheracSimulatorAdapter {

enum ExtCallType {
  CheekyPadding,
  ExtCallSendMEOS,
  ExtCallToggleDatentComplete,
  ExtCallToggleEditingTakingPlace,
  ExtCallReset,
  ExtCallProceed
};

enum BeamType {
  BTCheekyPadding,
  BeamTypeXRay,
  BeamTypeElectron,
  BeamTypeUndefined
};
enum CollimatorPosition {
  CPCheekyPadding,
  CollimatorPositionXRay,
  CollimatorPositionElectronBeam,
  CollimatorPositionUndefined
};
enum StateInfoRequest {
  SIRCheekyPadding,
  RequestTreatmentOutcome,
  RequestActiveSubsystem,
  RequestTreatmentState,
  RequestReason,
  RequestBeamMode,
  RequestBeamEnergy
};

class TheracSimulatorAdapter {
public:
  TheracSimulatorAdapter();
  TheracSimulatorAdapter(TheracSimulatorAdapter const &) = delete;
  TheracSimulatorAdapter(TheracSimulatorAdapter &&)      = delete;
  auto operator=(TheracSimulatorAdapter const &) -> TheracSimulatorAdapter & =
                                                        delete;
  auto
  operator=(TheracSimulatorAdapter &&) -> TheracSimulatorAdapter & = delete;
  ~TheracSimulatorAdapter();
  void externalCallWrap(
      ExtCallType ext_call_type,
      BeamType beam_type                     = BeamTypeUndefined,
      CollimatorPosition collimator_position = CollimatorPositionUndefined,
      HsInt beam_energy                      = 0
  );
  auto requestStateInfo(StateInfoRequest state_info_request) -> std::string;
/*
  auto check_malfunction() -> bool;
  auto set_malfunction() -> bool;
  auto reset_malfunction() -> bool;
*/
private:
  static auto
  hs_init(std::string const & args = "-threaded +RTS -N -RTS") -> HsStablePtr;
  static void hs_exit();

  void * wrapped_comms;
  bool malfunctioning = false;
//  std::shared_mutex malfunctioning_mutex;
};
} // namespace TheracSimulatorAdapter
