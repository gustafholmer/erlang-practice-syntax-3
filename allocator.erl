-module(allocator).

-export([start/1, request/2, release/2]).

start(Resources) ->
  spawn_link(fun () ->
    allocator(Resources)
             end).

request(Pid, List_items_wanted) ->
  Ref = make_ref(),
  Pid ! {request, {self(), Ref, List_items_wanted}},
  receive
    {granted, Ref, Granted} ->
      Granted
  end.

release(Pid, Resources) ->
  Ref = make_ref(),
  Pid ! {release, {self(), Ref, Resources}},
  receive
    {released, Ref} ->
      ok
  end.

allocator(Resources) ->
  Resources_list_of_keys = maps:keys(Resources),

  receive
    {request, {Pid, Ref, List_items_wanted}} when length(Resources_list_of_keys) == length(List_items_wanted), length(List_items_wanted) == 3 -> Pid ! {granted, Ref, Resources}, allocator(#{});

    {request, {Pid, Ref, List_items_wanted}} when length(Resources_list_of_keys) >= length(List_items_wanted), length(List_items_wanted) == 2,
      map_get(hd(List_items_wanted), Resources) /= {badkey, hd(List_items_wanted)}, map_get(hd(tl(List_items_wanted)), Resources) /= {badkey, hd(tl(List_items_wanted))}
      -> Pid ! {granted, Ref, #{hd(List_items_wanted) => maps:get(hd(List_items_wanted), Resources), hd(tl(List_items_wanted)) => maps:get(hd(tl(List_items_wanted)),
        Resources)}}, allocator(maps:remove(hd(tl(List_items_wanted)), (maps:remove(hd(List_items_wanted), Resources))));

    {request, {Pid, Ref, List_items_wanted}} when length(Resources_list_of_keys) >= length(List_items_wanted), length(List_items_wanted) == 1,
      hd(List_items_wanted) == hd(Resources_list_of_keys);
      hd(List_items_wanted) == hd(tl(Resources_list_of_keys))
      -> {Result, Return_res} = maps:take(hd(List_items_wanted), Resources),
      Pid ! {granted, Ref, #{hd(List_items_wanted) => Result}}, allocator(Return_res);

    {release, {Pid, Ref, Released}} ->
      Pid ! {released, Ref},
      allocator(maps:merge(Released, Resources))
  end.

