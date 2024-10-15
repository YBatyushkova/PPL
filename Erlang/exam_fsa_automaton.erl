-module(exam_fsa_automaton).
-compile(export_all).


% q0() ->
%     receive
%         {S, [b|Xs]} -> q1 ! {S, Xs}, q2 ! {S, Xs};
%         {S, [a|Xs]} -> q0 ! {S, Xs}
%     end, q0().

% q1() ->
%     receive
%         {S, [b|Xs]} -> q0 ! {S, Xs}
%     end, q1().

% q2() ->
%     receive
%         {S, [b|Xs]} -> q3 ! {S, Xs}
%     end, q2().

% q3() ->
%     receive
%         {S, [c|Xs]} -> q4 ! {S, Xs}
%     end, q3().
       
% q4() ->
%     receive
%         {S, []} -> io:format("String ~p is accepted~n", [S])
%     end, q4().

% read_string(S) ->
%     q0 ! {S, S}, ok.

% start() ->
%  register(q0, spawn(fun() -> q0() end)), % to avoid exporting qs
%  register(q1, spawn(fun() -> q1() end)),
%  register(q2, spawn(fun() -> q2() end)),
%  register(q3, spawn(fun() -> q3() end)),
%  register(q4, spawn(fun() -> q4() end)).


q0() ->
 receive
 {S, [b|Xs]} -> q1 ! {S, Xs}, q2 ! {S, Xs};
 {S, [a|Xs]} -> q0 ! {S, Xs}
 end, q0().
q1() ->
 receive
 {S, [b|Xs]} -> q0 ! {S, Xs}
 end, q1().
q2() ->
 receive
 {S, [b|Xs]} -> q3 ! {S, Xs}
 end, q2().
q3() ->
 receive
 {S, [c|Xs]} -> q4 ! {S, Xs}
 end, q3().
q4() ->
 receive
 {S, []} -> io:format("~w accepted~n", [S])
 end, q4().
start() ->
 register(q0, spawn(fun() -> q0() end)), % to avoid exporting qs
 register(q1, spawn(fun() -> q1() end)),
 register(q2, spawn(fun() -> q2() end)),
 register(q3, spawn(fun() -> q3() end)),
 register(q4, spawn(fun() -> q4() end)).
read_string(S) ->
 q0 ! {S, S}, ok.
