#include "Therac.h"
#include <HsTherac25_stub.h>
HsStablePtr start_machine() {

  char * argv[] = {"-threaded", "+RTS", "-N", "-RTS", '\0'};
  int argc      = 4;
  char ** pargv = argv;
  hs_init(&argc, &pargv);
  return startMachine();
}
void kill_machine() { hs_exit(); }
void wrap_external_call(
    HsStablePtr wrapped_comms,
    ExtCallType ext_call_type,
    BeamType beam_type,
    CollimatorPosition collimator_position,
    HsInt beam_energy
) {

  externalCallWrap(
      wrapped_comms,
      ext_call_type,
      beam_type,
      collimator_position,
      beam_energy
  );
}
HsPtr request_state_info(
    HsStablePtr wrapped_comms,
    StateInfoRequest state_info_request
) {
  return requestStateInfo(wrapped_comms, state_info_request);
}
