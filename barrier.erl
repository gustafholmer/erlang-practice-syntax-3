%%%-------------------------------------------------------------------
%%% gustafholmer guho0000
%%%-------------------------------------------------------------------

-module(barrier).
-export([start/1, wait/2]).

start(Refs) ->
  spawn_link(fun () -> loop(Refs, []) end).

loop(Exptd, Pids) when length(Exptd) =:= length(Pids) ->
  [Pid ! {continue, Ref} || {Pid, Ref} <- Pids],
  loop(Exptd, []);

loop(Exptd, Pids) ->
  receive
    {arrive, {Pid, Ref}} -> case lists:member(Ref, Exptd) of
                              true -> loop(Exptd, lists:reverse([{Pid, Ref}|Pids]));
                              false -> Pid ! {continue, Ref}, loop(Exptd, Pids)
                            end
  end.

wait(Barrier, Ref) ->
  Barrier ! {arrive, {self(), Ref}},
  receive
    {continue, Ref} ->
      ok
  end.









