-record(one_for_one, 		{max_restarts, max_seconds_between_restarts}).
-record(one_for_all, 		{max_restarts, max_seconds_between_restarts}).
-record(rest_for_one, 		{max_restarts, max_seconds_between_restarts}).
-record(simple_one_for_one, {max_restarts, max_seconds_between_restarts}).

child(Type, Module, ArgumentArray) ->
  Module 		= Module,
  StartFunc = {Module, start_link, ArgumentArray},
  Restart 	= permanent,
  Shutdown 	= 5000,
  Type 		  = Type,
  Modules 	= [Module],
  {Module, StartFunc, Restart, Shutdown, Type, Modules}.
