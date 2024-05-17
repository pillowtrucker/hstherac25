#include "Therac.h"
#include "HsFFI.h"
#include <HsTherac25_stub.h>
HsStablePtr start_machine() {
        
        char * argv[4] = {(char *)"-threaded",(char *)"+RTS",(char *)"-N",(char  *)"-RTS"};
        int argc = 4;
        char **pargv = argv;
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
