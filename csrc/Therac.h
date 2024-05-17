#include <HsFFI.h>
typedef enum ExtCallType {
  CheekyPadding,
  ExtCallSendMEOS,
  ExtCallToggleDatentComplete,
  ExtCallToggleEditingTakingPlace,
  ExtCallReset,
  ExtCallProceed
} ExtCallType;

typedef enum BeamType {
  BTCheekyPadding,
  BeamTypeXRay,
  BeamTypeElectron,
  BeamTypeUndefined
} BeamType;
typedef enum CollimatorPosition {
  CPCheekyPadding,
  CollimatorPositionXRay,
  CollimatorPositionElectronBeam,
  CollimatorPositionUndefined
} CollimatorPosition;
typedef enum StateInfoRequest {
  SIRCheekyPadding,
  RequestTreatmentOutcome,
  RequestActiveSubsystem,
  RequestTreatmentState,
  RequestReason,
  RequestBeamMode,
  RequestBeamEnergy
} StateInfoRequest;
#ifdef __cplusplus
extern "C" { // only need to export C interface if
             // used by C++ source code
#endif
#if mingw32_HOST_OS || _WIN32
__declspec(dllexport) HsStablePtr start_machine();
__declspec(dllexport) void kill_machine();
__declspec(dllexport) void wrap_external_call(
    HsStablePtr wrapped_comms,
    ExtCallType ext_call_type,
    BeamType beam_type,
    CollimatorPosition collimator_position,
    HsInt beam_energy
);
#else
HsStablePtr start_machine();
void kill_machine();
void wrap_external_call(
    HsStablePtr wrapped_comms,
    ExtCallType ext_call_type,
    BeamType beam_type,
    CollimatorPosition collimator_position,
    HsInt beam_energy
);
#endif
#ifdef __cplusplus
}
#endif
