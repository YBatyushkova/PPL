-module(exercising_august).
-compile(export_all).

hello_world() -> 
    "Hello".

% Rock, Paper, Scissors
rps_player() ->
    Pid = spawn(fun() -> rps_cpu() end),
    io:format("RPS Pid =~p~n", [Pid]),    
    Pid ! {self(), rock},
    Pid ! {self(), scissors},
    Pid ! {self(), scissors},
    rps_win_loop().

rps_win_loop() ->
    receive
        Result -> io:format("~p~n", [Result])
after 5000 -> exit(ok)
end.

rps_win(PlayerChoice, CPUChoice) ->
    case {PlayerChoice, CPUChoice} of
        {rock, paper} -> win;
        {rock, scissors} -> lose;
        {paper, rock} -> win;
        {paper, scissors} -> lose;
        {scissors, paper} -> win;
        {scissors, rock} -> lose;
        {_, _} -> draw
end.

rps_cpu() ->
    io:format("CPU ready~n"),
    RPS = [rock, paper, scissors],
    CPUChoice = lists:nth(rand:uniform(2), RPS),
    receive
        {PlayerPid, PlayerChoice} ->
            PlayerPid ! rps_win(PlayerChoice, CPUChoice)
end,
rps_cpu().


% 2024.01

% Define a condition-var-manager process that receives a list of 
% initial values and a list of the corresponding conditions 
% (unary predicates), both of the same length, and spawns for 
% each ordered pair of value and condition a process that updates 
% its value only when the new value satisfies the condition.

% The manager can receive the following messages:

% - {update, F} where F is a unary function that must be applied 
% to all the saved values to obtain the new values (only if the 
% corresponding condition holds);

% - print which makes each process print its current value;

% - stop which stops the manager and all its spawned processes.

condvar_manager(Vs, Conds) ->
    Pids = [spawn_link(fun() -> proc(V, C) end) || {V, C} <- lists:zip(Vs, Conds)],
    condvar_manager_loop(Pids).

condvar_manager_loop(Pids) ->
    receive
        % {update, F} -> [Pid ! {update, F} || Pid <- Pids];
        % print -> [Pid ! print || Pid <- Pids];
        stop -> exit(ok);
        Msg -> 
            [Pid ! Msg || Pid <- Pids],
            condvar_manager_loop(Pids) 
            % recursive call only in this case
end.

proc(V, C) ->
    receive
        {update, F} -> 
            V1 = F(V),
            case C(V1) of 
                true -> proc(F, C);
                false -> proc(V, C)
            end;
        print -> 
            io:format("Current value = ~p~n", [V]),
            proc(V, C)
end.


% 2024.02

% Consider a list L of tasks, where each task is encoded as a 
% function having only one parameter: a PID P. When a
% task is called, it runs some operations and then sends back 
% the results to P, in this form: {result, <Task_PID>,
% <Result_value>}; a task could also fail for some errors.

% Define a server which takes L and runs in parallel all the 
% tasks in it, returning the list of the results (the order is 
% not important). Note: in case of failure, every task should 
% be restarted only once; if it fails twice, its result should be
% represented with the atom bug.

% Utilities: you can use from the standard libraries the 
% following functions: maps:from_list(<List of {Key, Value}>),
% which takes a list of pairs and builds the corresponding 
% map, and maps:values(<Map>), which is its inverse.

server(Tasks) ->
    Self = self(),
    process_flag(trap_exit, true),
    % Create a map with elements of the form {TaskPid, {Task, status}}
    TaskPids = map:from_list([{spawn_link(fun() -> Task(Self) end), {Task, waiting}} || Task <- Tasks]),
    tasks_results_list(TaskPids, length(Tasks)).

tasks_results_list(TaskPids, 0) ->
    [Result || {completed, Result} <- map:values(TaskPids)];
tasks_results_list(TaskPids, RemainingTasks) ->
    receive
        {result, Pid, Result} -> 
            UpdatedTaskPids = TaskPids#{Pid := {completed, Result}},
            tasks_results_list(UpdatedTaskPids, RemainingTasks - 1);
        {failure, Pid} -> 
            #{Pid := {Task, Status}} = TaskPids,
            case Status of 
                restarted -> 
                    UpdatedTaskPids = TaskPids#{Pid := {completed, bug}},
                    tasks_results_list(UpdatedTaskPids, RemainingTasks - 1);
                _ -> 
                    NewPid = spawn_link(fun() -> Task(self()) end),
                    UpdatedTaskPids = TaskPids#{NewPid => {Task, restarted}},
                    tasks_results_list(UpdatedTaskPids, RemainingTasks)
            end
    end.


% 2024.06

% Pino wants to create a Erlang program which takes two lists of 
% data [x1, x2, ..], [y1, y2, ...] and a list of binary functions 
% [f1, f2, ...], and evaluates these functions in parallel, passing 
% them the respective parameters, to obtain [f1(x1, y1), f2(x2, y2), …]. 

parallel_apply(DataList1, DataList2, FunList) ->
    FuncAppList = zip3(FunList, DataList1, DataList2), 
    WorkerPids = map:from_list([{spawn_link(fun() -> worker({F, A, B}, self()) end), result} || {F, A, B} <- FuncAppList]),
    CollectorPid = spawn_link(fun() -> collector(WorkerPids, length(FuncAppList)) end),
    collector(WorkerPids, length(FuncAppList)),
    receive
        Msg -> CollectorPid ! Msg
end.

collector(WorkerPids, 0) ->
    [Result || Result <- map:values(WorkerPids)];
collector(WorkerPids, N) ->
    receive
        {WorkerPid, Result} -> 
            % WorkerPids#{WorkerPid := Result},
            collector(WorkerPids#{WorkerPid := Result}, N - 1)
    end.

worker({F, A, B}, ParentPid) ->
    ParentPid ! {self(), F(A, B)}.

zip3([A|As], [B|Bs], [F|Fs]) -> [{A, B, F} | zip3(As, Bs, Fs)];
zip3([], _, _) -> [];
zip3(_, [], _) -> [];
zip3(_, _, []) -> [].


% Official solution

% parallel_apply(List1, List2, FunList) ->
%     W = lists:map(fun(X) -> spawn(?MODULE, worker, [X, self()]) end, zip3(List1, List2, FunList)),
%     lists:map(fun (P) ->
%         receive
%             {P, V} -> V
%         end
%     end, W).

% worker({A, B, F}, CollectorPid) ->
%     CollectorPid ! {self(), F(A,B)}.


% 2024.07

% Define the main function of a broker process for centralized PID-less 
% interaction among processes. The broker must respond to these messages:
% • {new, Pid, Id} to bind the local broker identifier Id to the PID Pid;
% • {send, Id, Msg} to send Msg to the process having the local broker identifier Id;
% • {delete_id, Id} to delete the local broker identifier binding for Id;
% • {delete_pid, Pid} to delete the local data for PID Pid;
% • {broadcast, Msg} to send Msg to all the processes known by the broker;
% • stop to stop the broker.

% You can use the following OTP functions, if you need them:
% maps:remove(Key, Map), to remove Key from Map

% MAP:FILTERMAP(F/2, Map), which is a filter, where F/2 takes a pair (Key, Value) and returns a Boolean

% maps:foreach(F/2, Map), which runs F/2 on all the pairs (Key, Value) in Map.

broker(Map) ->
    receive
        {new, Pid, Id} -> 
            broker(Map#{Id => Pid});
        {send, Id, Msg} -> 
            #{Id := Pid} = Map,
            Pid ! Msg,
            broker(Map);
        {delete_id, Id} ->
            broker(maps:remove(Id, Map));
        {delete_pid, Pid} -> 
            broker(maps:filtermap(fun (_,Value) ->
                Value =/= Pid
                end, Map));
        {broadcast, Msg} -> 
            maps:foreach(fun (_, Value) ->
                Value ! Msg
                end, Map),
            broker(Map);
        stop -> exit(ok)
    end.


% 2020.07

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
            LVList = lists:zip(L, V),
            NewPids = [spawn_link(fun() -> 
                Name(Param) end) || {Name, Param} <- LVList],
            broadcaster(NewPids);
        {send, V} -> 
            PidVList = lists:zip(Pids, V),
            [Pid ! Val || {Pid, Val} <- PidVList],
            broadcaster(Pids);
        stop -> exit(ok)
    end.

% zip([A|As], [B|Bs]) -> [{A, B} | zip(As, Bs)];
% zip([], _) -> [];
% zip(_, []) -> [].

% send_message([X|Xs]) -> 
%     if X =/= [] ->
%         fun({ToPid, Value}) ->
%             ToPid ! Value
%         end,
%         send_message(Xs)
%     end.

% 2023.09

btrees(ParentPid, X) ->
    spawn(fun() -> btrees_body({leaf, X}, self()) end).

btrees_body(Tree, ParentPid) ->
    receive
        next -> 
            NewTree = {branch, {Tree}, {Tree}},
            ParentPid ! NewTree,
            btrees_body(NewTree, ParentPid);
        stop -> Tree
end.

incbtrees(ParentPid) ->
    spawn(fun() -> btrees_body({leaf, 0}, self()) end).

inccount({leaf, N}) -> {leaf, N+1};
inccount({branch, L1, L2}) -> {branch, inccount(L1), inccount(L2)}.

incbtrees_body(Tree, ParentPid) ->
    receive
        next -> 
            Tree1 = inccount(Tree),
            Tree2 = {branch, {Tree1}, {Tree1}},
            ParentPid ! Tree2,
            incbtrees_body(Tree2, ParentPid);
        stop -> Tree
end.   