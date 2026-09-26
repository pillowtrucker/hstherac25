#pragma once
#include <HsFFI.h>

#ifdef __cplusplus
#endif

/* Values are part of the ABI: only ever append. 0 is never a valid value and is ignored. */
typedef enum ExtCallType {
  CheekyPadding,
  ExtCallSendMEOS,                 /* 1: prescription edited on screen */
  ExtCallToggleDatentComplete,     /* 2: cursor reached the command line ("Begin"). Sets the
                                          flag; only a reset clears it (that is one of the races) */
  ExtCallToggleEditingTakingPlace, /* 3: cursor moved off the command line to edit. Sets the flag;
                                          SendMEOS also sets it */
  ExtCallReset,                    /* 4: R - after Begin it only takes effect once the magnets are
                                          set (as in the original Datent) */
  ExtCallProceed,                  /* 5: P - only during a treatment pause */
  ExtCallHardReset,                /* 6: power cycle */
  ExtCallSet,                      /* 7: set button on the hand control / "set" typed at the console */
  ExtCallFieldLight,               /* 8: hand control rotates the turntable to the field-light position */
  ExtCallBeamOn,                   /* 9: "B" typed at the console - fires the beam once it says BEAM READY.
                                          Only needed after ExtCallUseBeamOnKey */
  ExtCallUseBeamOnKey,             /* 10: this UI has a "B" command: from now on Set-Up Done waits for
                                          ExtCallBeamOn instead of firing by itself (without it, Begin
                                          doubles as B). Survives resets */
  ExtCallPrescribeDose,            /* 11: prescribed monitor units, passed in the beam_energy argument */
  /* clearer aliases for 2 and 3 */
  ExtCallDataEntryComplete = ExtCallToggleDatentComplete,
  ExtCallEditingTakingPlace = ExtCallToggleEditingTakingPlace
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
  CollimatorPositionUndefined /* "nothing requested": the turntable follows the beam type */
} CollimatorPosition;
typedef enum StateInfoRequest {
  SIRCheekyPadding,
  RequestTreatmentOutcome,  /* 1: "", "TREATMENT OK", "MALFUNCTION 54", "FLATNESS", ... */
  RequestActiveSubsystem,   /* 2: "DATA ENTRY" / "TREAT" */
  RequestTreatmentState,    /* 3: "TP_Datent", "TP_PauseTreatment", ... TP_TerminateTreatment with an
                                  outcome other than "TREATMENT OK" means treatment suspend */
  RequestReason,            /* 4: "OPERATOR" or the outcome */
  RequestBeamMode,          /* 5: beam type the hardware was actually set up for */
  RequestBeamEnergy,        /* 6: energy the hardware was actually set up for */
  RequestDumpFullState,     /* 7: the whole state record */
  RequestClass3,            /* 8: the one-byte Class3 counter, "0".."255" */
  RequestTurntablePosition, /* 9: where the turntable physically is, e.g. "CollimatorPositionFieldLight" */
  RequestDisplayedDose,     /* 10: monitor units the dose monitor showed for the last attempt */
  RequestPatientDose,       /* 11: rads the patient actually received since the last reset (the real console couldn't show this) */
  RequestSetButtonPrompt    /* 12: "PRESS SET BUTTON" while the field light is on, else "" */
} StateInfoRequest;
#ifdef __cplusplus
extern "C" { // only need to export C interface if
             // used by C++ source code
#endif
#if mingw32_HOST_OS || _WIN32
__declspec(dllexport) HsStablePtr start_machine();
/* After kill_machine the Haskell runtime cannot be started again in the same process.
   Use ExtCallHardReset to restart the simulation instead. */
__declspec(dllexport) void kill_machine();
__declspec(dllexport) void wrap_external_call(
    HsStablePtr wrapped_comms,
    ExtCallType ext_call_type,
    BeamType beam_type,
    CollimatorPosition collimator_position,
    HsInt beam_energy
);
/* The returned string is malloc'd by the library; release it with free_state_info. */
__declspec(dllexport) HsPtr request_state_info(
    HsStablePtr wrapped_comms,
    StateInfoRequest state_info_request
);
__declspec(dllexport) void free_state_info(HsPtr state_info);
#else
HsStablePtr start_machine();
/* After kill_machine the Haskell runtime cannot be started again in the same process.
   Use ExtCallHardReset to restart the simulation instead. */
void kill_machine();
void wrap_external_call(
    HsStablePtr wrapped_comms,
    ExtCallType ext_call_type,
    BeamType beam_type,
    CollimatorPosition collimator_position,
    HsInt beam_energy
);
/* The returned string is malloc'd by the library; release it with free_state_info. */
HsPtr request_state_info(
    HsStablePtr wrapped_comms,
    StateInfoRequest state_info_request
);
void free_state_info(HsPtr state_info);
#endif
#ifdef __cplusplus
}
#endif
