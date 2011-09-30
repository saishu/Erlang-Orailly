-module(mutex). 
-export([start/0, stop/0]). 
-export([wait/0, signal/0]). 
-export([init/1]).

start() -> register(mutex, spawn(?MODULE, init, [self()])).
stop()  -> mutex ! stop.

wait()  -> 
	mutex ! {wait, self()}, 
	receive 
		ok -> ok 
	end.

signal() ->
	mutex ! {signal, self()}, 
	ok.
	
init(Pid) ->
	io:format("init   Pid:~w  Self:~w~n", [Pid, self()]),
	erlang:monitor(process, Pid),
	process_flag(trap_exit, true),
	free().
	
free() -> 
	receive
		{wait, Pid} -> 
			Pid ! ok, 
			busy(Pid);
		{'DOWN', _Reference, process, Pid, _Reason} ->
			io:format("[DOWN] Pid:~w~n", [Pid]),
			signal();
		stop ->
			terminate() 
	end.
	
busy(Pid) -> 
	receive
		{signal, Pid} -> free()
	end.

terminate() -> 
	receive
		{wait, Pid} -> 
			exit(Pid, kill), 
			terminate()
		after 0 -> ok
	end.