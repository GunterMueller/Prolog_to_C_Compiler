%%%% GSASL interface - prolog part
%
% Note:
%
% - the number of steps is user-defined, dependent on the number
%   of calls to sasl_step64/2
% - GSASL-sessions can not be run in parallel.


:- verbatim('#include "sasl.c"').


sasl_init :- foreign_call(sasl_init).
sasl_done :- foreign_call(sasl_done).

sasl_client_start(MECH, USER, PASSWD, TOKEN, REALM) :-
	foreign_call(sasl_client_start(MECH, USER, PASSWD, TOKEN, REALM)).

sasl_finish :- foreign_call(sasl_finish).

sasl_step64(IN, OUT) :-
	foreign_call(sasl_step64(IN, OUT)).
