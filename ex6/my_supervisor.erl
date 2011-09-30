-module(my_supervisor). 
-export([start_link/2, stop/1]). 
-export([init/1]).

start_link(Name, ChildSpecList) ->
	register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.
	
init(ChildSpecList) -> 
	process_flag(trap_exit, true), 
	loop(start_children(ChildSpecList)).
	
start_children([]) -> [];
start_children([{M, F, A} | ChildSpecList]) ->
	io:format("[start_children] M:~w F:~w A:~w ChildSpecList:~w~n",[M,F,A, ChildSpecList]),
	case apply(M,F,A) of 
		{ok, Pid} ->
			io:format("[start_children] Pid:~w~n", [Pid]),
			[{Pid, {M,F,A}}|start_children(ChildSpecList)]; 
		_ ->
			io:format("[start_children] ChildSpecList:~w~n", [ChildSpecList]),
			start_children(ChildSpecList) 
	end.
	
restart_child(Pid, ChildList) ->
	io:format("[restart_child] Pid:~w  ChildList:~w~n",[Pid, ChildList]),
	io:format("[restart_child] keysearch:~w~n",[lists:keysearch(Pid, 1, ChildList)]),
	{value, {Pid, {M,F,A}}} = lists:keysearch(Pid, 1, ChildList),
	io:format("[restart_child] M:~w F:~w A:~w~n",[M,F,A]),
	{ok, NewPid} = apply(M,F,A),
	io:format("[restart_child] NewPid:~w~n",[NewPid]),
	[{NewPid, {M,F,A}}|lists:keydelete(Pid,1,ChildList)].
	
loop(ChildList) -> 
	io:format("[loop] ChildList:~w~n", [ChildList]),
	receive
		{'EXIT', Pid, _Reason} ->
			NewChildList = restart_child(Pid, ChildList), 
			io:format("[loop] NewChildList:~w~n", [NewChildList]),
			loop(NewChildList);
		{stop, From} ->
			From ! {reply, terminate(ChildList)} 
	end.
	
stop(Name) ->
	Name ! {stop, self()},
	receive 
		{reply, Reply} -> Reply 
	end.

terminate([{Pid, _} | ChildList]) -> 
	exit(Pid, kill), 
	terminate(ChildList);
terminate(_ChildList) -> ok.