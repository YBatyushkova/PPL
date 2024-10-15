-module(exam_btrees).
-compile(export_all).

%%% 2023-09-12
% Consider the infinite list of complete binary trees (from THAT haskell exercise): instead of infinite lists,
% we want to create processes which return the current element of the "virtual infinite list" with the message next,
% and terminate with the message stop.
% 1. Define a function btrees to create a process corresponding to the infinite tree of Exercise 2.1.
% 2. Define a function incbtrees to create a process corresponding to the infinite tree of Exercise 2.2.
% Notes: for security reasons, processes must only answer to their creating process; to define trees, you can use suitable
% tuples with atoms as customary in Erlang (e.g. {branch, {leaf, 1}, {leaf, 1}}).



% btrees_child(Parent, Tree) ->
%     NextTree =
%         case Tree of
%             {leaf, Value} -> {branch, {leaf, Value}, {leaf, Value}};
%             {branch, V1, V2} -> {branch, {branch, V1, V2}, {branch, V1, V2}};
%             V -> {leaf, V}
%         end,
%     receive
%         {Parent, next} ->
%             Parent ! NextTree,
%             io:format("~p~n", [NextTree]);
%         {Parent, stop} -> exit(ok)
%     end,
%     btrees_child(Parent, NextTree).

% btrees(X) ->
%     Self = self(),
%     Pid = spawn(fun() -> btrees_child(Self, X) end),
%     Pid ! {Self, next},
%     Pid ! {Self, next},
%     Pid ! {Self, next},
%     Pid ! {Self, stop}.




btrees_child_t(Parent, Tree) ->
    io:format("we are inside child builder~n"),
    receive
        stop -> exit(ok);
        next ->
            Parent ! Tree, 
            io:format("~p~n", [Tree]),
            btrees_child_t(Parent, {branch, Tree, Tree})
    end.

btrees_t(X) ->
    Self = self(),
    Pid = spawn(fun() -> btrees_child_t(Self, {leaf, X}) end),
    Pid ! next,
    Pid ! next,
    Pid ! next,
    Pid ! stop.



inchelper({leaf, X}) -> {leaf, X + 1};
inchelper({branch, T1, T2}) -> {branch, inchelper(T1), inchelper(T2)}.

inc_btrees_child_t(Parent, Tree) ->
    io:format("we are inside child builder~n"),
    receive
        stop -> exit(ok);
        next ->
            Parent ! Tree, 
            io:format("~p~n", [Tree]),
            inc_btrees_child_t(Parent, {branch, inchelper(Tree), inchelper(Tree)})
    end.

inc_btrees_t() ->
    Self = self(),
    Pid = spawn(fun() -> inc_btrees_child_t(Self, {leaf, 1}) end),
    Pid ! next,
    Pid ! next,
    Pid ! next,
    Pid ! next,
    Pid ! stop.




% next_tree({leaf, Value}) -> {leaf, Value + 1};
% next_tree({branch, V1, V2}) -> {branch, next_tree(V1), next_tree(V2)}.

% incbtrees_child(Parent, Tree) ->
%     receive
%         {Parent, next} ->
%             Tree1 = next_tree(Tree), %% increase value of the leaf
%             Tree2 = {branch, Tree1, Tree1}, %% extend the tree with the new branches
%             Parent ! Tree2,
%             io:format("~p~n", [Tree2]),
%             incbtrees_child(Parent, Tree2);
%         {Parent, stop} -> exit(ok)
%     end.

% incbtrees() ->
%     Self = self(),
%     Pid = spawn(fun() -> incbtrees_child(Self, {leaf, 0}) end),
%     Pid ! {Self, next},
%     Pid ! {Self, next},
%     Pid ! {Self, next},
%     Pid ! {Self, stop}.