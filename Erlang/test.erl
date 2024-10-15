-module(test).
-compile(export_all).

btrees(X) ->
    Self = self(),
    Pid = spawn(fun() -> btrees_child(Self, X) end),
    Pid ! {Self, next},
    Pid ! {Self, next},
    Pid ! {Self, next}.
    

btrees_child(Parent, Tree) ->
    NextTree =
        case Tree of
            {leaf, Value} -> {branch, {leaf, Value}, {leaf, Value}};
            {branch, V1, V2} -> {branch, {branch, V1, V2}, {branch, V1, V2}};
            V -> {leaf, V}
        end,
    receive
        {Parent, next} ->
            Parent ! NextTree,
            io:format("~p~n", [NextTree])
    after 5000 -> exit(ok)
    end,
    btrees_child(Parent, NextTree).

