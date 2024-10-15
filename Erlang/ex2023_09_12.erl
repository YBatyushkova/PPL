-module(ex2023_09_12).
-compile(export_all).

%%% 2023-09-12
% Consider the infinite list of complete binary trees (from THAT haskell exercise): instead of infinite lists,
% we want to create processes which return the current element of the "virtual infinite list" with the message next,
% and terminate with the message stop.
% 1. Define a function btrees to create a process corresponding to the infinite tree of Exercise 2.1.
% 2. Define a function incbtrees to create a process corresponding to the infinite tree of Exercise 2.2.
% Notes: for security reasons, processes must only answer to their creating process; to define trees, you can use suitable
% tuples with atoms as customary in Erlang (e.g. {branch, {leaf, 1}, {leaf, 1}}).

btrees(Val) ->
    Self = self(),
    Pid = spawn(fun() -> btrees_child(Self, Val) end),
    Pid ! {Self, next},
    Pid ! {Self, next},
    Pid ! {Self, next}.

btrees_child(Parent, Tree) ->
    % Self = self(),
    NextTree =
        case Tree of
            {leaf, Value} -> {branch, {leaf, Value}, {leaf, Value}};
            {branch, V1, V2} -> {branch, {branch, V1, V2}, {branch, V1, V2}};
            V -> {leaf, V}
        end,

    receive
        {Parent, next} -> 
            % Parent ! NextTree,
            io:format("~p~n", [NextTree])
    end,
    btrees_child(Parent, NextTree).





incbtrees() ->
    Self = self(),
    Tree = 1,
    Pid = spawn(fun() -> btrees_child_inc(Self, Tree) end),
    Pid ! {Self, next},
    Pid ! {Self, next},
    Pid ! {Self, next},
    Pid ! {Self, next}.

btrees_child_inc(Parent, Tree) ->
    % Self = self(),
    NextTree =
        case Tree of
            {leaf, Value} -> inc_func({branch, {leaf, Value}, {leaf, Value}});
            {branch, V1, V2} -> inc_func({branch, {branch, V1, V2}, {branch, V1, V2}});
            V -> {leaf, V}
        end,

    receive
        {Parent, next} -> 
            % Parent ! NextTree,
            io:format("~p~n", [NextTree])
    end,
    btrees_child_inc(Parent, NextTree).

inc_func({leaf, Value}) -> {leaf, Value + 1};
inc_func({branch, L, R}) -> {branch, inc_func(L), inc_func(R)}.
