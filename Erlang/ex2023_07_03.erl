-module(ex2023_07_03).
-compile(export_all).

deeprev([]) -> [];
deeprev([X | Xs]) -> deeprev(Xs) ++ [deeprev(X)];
deeprev(X) -> X.

% Recursion
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
        {PX, V} ->
            receive
                {PXs, Vs} -> Pid ! {Self, Vs ++ [V]}
            end
    end;
dp(Pid, X) ->
    Self = self(),
    Pid ! {Self, X}.
