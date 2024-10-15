-module(exercise).
-compile(export_all).

hello_world() ->
    "Hello world".


greet(Name) when is_list(Name) ->
    "Hello " ++ Name ++ "!";
greet(_) ->
    "AAAAAAAAAAA".


is_adult(Age) when Age >= 18 -> true;
is_adult(_) -> false.

%% We cannot check is_adult result through if, 
%% because if accepts only GUARDS,
%% so we need to use case

greet_adult(Name, Age) ->
    case is_adult(Age) of
        true -> "Hello " ++ Name ++ "!";
        _ -> "Too young"
    end.


how_long([]) -> 0;
how_long([X | Xs]) -> 1 + how_long(Xs).


%%% Rock, Paper, Scissors

rps_start() ->
    Pid = spawn(fun() -> rps_loop() end),
    io:format("RPS Pid = ~p~n", [Pid]),
    Pid ! {self(), scissors},
    Pid ! {self(), scissors},
    Pid ! {self(), scissors},
    Pid ! {self(), scissors},
    rps_win_loop().


rps_win_loop() ->
    receive
        Result -> io:format("~p~n", [Result])
    after 5000 -> exit(ok)
    end,
    rps_win_loop().


rps_win(Player, CPU) ->
    case {Player, CPU} of
        {rock, paper} -> win;
        {rock, scissors} -> lose;
        {paper, rock} -> win;
        {paper, scissors} -> lose;
        {scissors, paper} -> win;
        {scissors, rock} -> lose;
        {_, _} -> draw
    end.


rps_loop() ->
    io:format("CPU ready~n"),
    RPS = [rock, paper, scissors],
    CPUChoice = lists:nth(rand:uniform(2), RPS),
    receive
        {PlayerPid, PlayerChoice} ->
            PlayerPid ! rps_win(PlayerChoice, CPUChoice)
    end,
    rps_loop().


%%% LINK - to control crashes
%%% spawn_link

%% Crashing child
bad_poc() ->
    io:format("I'm gonna crash!~n"),
    4/0.

test_spawn() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> bad_poc() end),
    receive
        Err -> io:format("Received ~p~n", [Err])
    end.


%% Crashing parent, CHILD DON'T CRASH BY DEFAULT
%% need to use spawn_link to syncronize
dying_parent(N) ->
    spawn_link(fun() -> child(N) end),
    io:format("Parent is going to die~n"),
    receive
    after 3000 ->
        io:format("*Parent dies*~n"),
        exit(ok)
    end.

child(0) -> exit(ok);
child(N) ->
    receive
    after 1000 ->
        io:format("Child~n")
    end,
    child(N-1).

%% Parent dies and the child is notified
%% MONITORING
dying_parent_m() ->
    Pid = self(),
    spawn(fun() -> child_m(Pid) end),
    receive
    after 1000 ->
        io:format("[P] *Parent dies*~n"),
        exit(ok)
    end.

child_m(ParentPid) ->
    io:format("[C] Monitoring parent...~n"),
    monitor(process, ParentPid),
    receive
        Any -> io:format("[C] Parent is dead!~n~p~n", [Any])
    after 2000 -> waiting
    end.

%%% Exam example
%%% 1. Define a “deep reverse” function, which
%%% takes a “deep” list, i.e. a list containing
%%% possibly lists of any depths, and returns
%%% its reverse.
% deeprev([1,2,[3,[4,5]],[6]]) is [[6],[[5,4],3],2,1].
%%% 2. Define a parallel version of the previous function.

deeprev([X | Xs]) -> deeprev(Xs) ++ [deeprev(X)];
deeprev(X) -> X.

deeprevp(List) ->
    Self = self(),
    dp(Self, List),
    receive
        {Self, Res} -> Res
    end.

dp(Pid, [X | Xs]) ->
    Self = self(),
    PX = spawn(fun() -> dp(Self, X) end),
    PXs = spawn(fun() -> dp(Self, Xs) end),
    receive
        {PX, RX} ->
            receive
                {PXs, RXs} ->
                    Pid ! {Self, RXs ++ [RX]}
            end
    end;
dp(Pid, X) ->
    Pid ! {self(), X}.