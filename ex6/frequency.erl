-module(frequency).
-export([start/0,init/1,stop/0,allocate/0,deallocate/1]).
-export([loop/1]).

%% Client
%%
start() 			-> register(server, spawn(?MODULE, init,[{[10,11,12,13,14,15],[]}])).
init(Frequencies)	-> process_flag(trap_exit, true), loop(Frequencies).
stop() 				-> call(stop).
allocate() 			-> call(allocate).
deallocate(Freq)	-> call({deallocate, Freq}).

%% Helper Functions - Client
%%
call(Message) ->
	server ! {request, self(), Message},
	receive
		{reply, Reply} -> Reply
	end.

%% Helper Functions - Server
%%	
reply(Pid, Code) ->	Pid ! {reply, Code}.

% count a number of allocated frequencies per user
capacity_check([],                       _, Num) -> Num;
capacity_check([{_, Pid} | Allocated], Pid, Num) -> capacity_check(Allocated, Pid, Num + 1);
capacity_check([{_,   _} | Allocated], Pid, Num) -> capacity_check(Allocated, Pid, Num).

% check over allocation
alloc_helper(Num, Freq, Free, Pid, Allocated) when Num < 3 -> link(Pid), {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}};
alloc_helper(  _, Freq, Free,   _, Allocated)              -> 			 {{[Freq | Free], Allocated},        {error, over_allocation}}.

% allocate frequency to a user
allocate({[],            Allocated}, _)   -> {{[], Allocated}, {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) -> alloc_helper(capacity_check(Allocated, Pid, 0), Freq, Free, Pid, Allocated).

% check illegal deallocation - no used frequency / other user
dealloc_helper(      false,    _,   _, Free, Allocated) ->	{error, no_used_frequency, {Free, Allocated}};
dealloc_helper({Freq, Pid}, Freq, Pid, Free, Allocated) ->	{ok,    ok,                check_unlink(Pid, [Freq | Free], lists:keydelete(Freq, 1, Allocated))};
dealloc_helper(          _,    _,   _, Free, Allocated) ->	{error, user_error,        {Free, Allocated}}.

% if client use several frequencies, delete one frequency only. if it is last frequency, delete frequency and unlink
check_unlink(Pid, Free, Allocated) -> do_unlink(Pid, Free, Allocated, lists:keysearch(Pid, 2, Allocated)).

do_unlink(Pid, Free, Allocated, false) -> unlink(Pid), {Free, Allocated};
do_unlink(  _, Free, Allocated,     _) ->              {Free, Allocated}.

% deallocate frequency
deallocate({Free, []},           _,   _) ->	{error, {Free, []}};
deallocate({Free, Allocated}, Freq, Pid) ->	dealloc_helper(lists:keyfind(Freq, 1, Allocated), Freq, Pid, Free, Allocated).

% exited handler
exited({Free, []},          _) -> {Free, []};
exited({Free, Allocated}, Pid) -> 
	case lists:keysearch(Pid, 2, Allocated) of
		{value, {Freq, Pid}} -> exited({[Freq | Free], lists:keydelete(Pid, 2, Allocated)}, Pid);		
		false 				 -> {Free, Allocated}
	end.
		
%% Server
%%
loop(Frequencies) ->
	receive
		{request, Pid, allocate} ->
			{Allocated, Ret} = allocate(Frequencies, Pid),
			io:format("Frequencies: ~w~n", [Allocated]),
			reply(Pid, Ret),
			loop(Allocated);
		{request, Pid, {deallocate, Freq}} ->
			{Ret, Reason, Allocated} = deallocate(Frequencies, Freq, Pid),
			io:format("Frequencies: ~w~n", [Allocated]),
			reply(Pid, {Ret, Reason}),
			loop(Allocated);
		{request, Pid, stop} ->
			reply(Pid, ok);
		{'EXIT', Pid, _Reason} ->
			io:format("[EXIT] Pid:~w~n", [Pid]),
			NewFrequencies = exited(Frequencies, Pid),
			io:format("New Frequencies: ~w~n", [NewFrequencies]),
			loop(NewFrequencies)
	end.
	