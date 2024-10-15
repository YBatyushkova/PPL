-module(ex2020_07_17).
-compile(export-all).

%%% 2020-07-17
% Define a "broadcaster" process which answers to the following
% commands:
% - {spawn, L, V} creates a process for each element of L, passing its
% initial parameter in V, where L is a list of names of functions
% defined in the current module and V is their respective parameters (of
% course it must be |L| = |V|);
% - {send, V}, with V a list of values, sends to each respective process
% created with the previous spawn command a message in V; e.g. {spawn,
% [1,2,3]} will send 1 to the first process, 2 to the second, and 3 to
% the third;
% - stop is used to end the broadcaster, and to also stop every process
% spawned by it.


broadcaster(Pids) ->
    receive
        {spawn, L, V} ->
            F = lists:zip(L, V),
            broadcaster([spawn_link(?MODULE, Ls, Vs) || {Ls, Vs} <- F]);
            % Use the same name for a child process as a function name - spawn(Module, Name, Args)
        {send, V} -> 
            F = lists:zip(Pids, V),
            [Pid ! Vs || {Pid, Vs} <- F];
            % The way to send multiple messages of the same structure
        stop -> 
            exit(ok)       
end.
