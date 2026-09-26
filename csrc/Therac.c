#include "Therac.h"
#include <HsTherac25_stub.h>
#include <stdlib.h>
HsStablePtr start_machine() {

  char * argv[] = {"hstherac25", "+RTS", "-N", "-RTS", NULL};
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
/* newCString allocates with malloc; free it with the same C runtime that allocated it */
void free_state_info(HsPtr state_info) { free(state_info); }
