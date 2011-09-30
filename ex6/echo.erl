-module(echo).
-export([start/0,print/1,stop/0,loop/0,init/1]).

start()    	-> 
	register(echo, spawn(echo, init, [self()])),
	ok.

print(Msg) 	-> 
	echo ! {self(),Msg},
	ok.
	
stop()		-> 
	exit(self(), kill).

init(Pid) ->
	process_flag(trap_exit, true),
	link(Pid),
	loop().
	
loop()		-> 
	receive
		{_From, Msg} ->
			io:format("~w~n", [Msg]),
			loop();
		{'EXIT', Pid, Reason} ->
			io:format("EXIT  Pid:~w  Reason:~w~n", [Pid, Reason])
	end.