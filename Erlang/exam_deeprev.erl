-module(exam_deeprev).
-compile(export_all).

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