-module(release_config).

-export([run/0]).

run()->
	io:format("Start Release Schema Config Files.~n"),
    ReleaseVersionStrings = [V||"config_release/release_"++V<-filelib:wildcard("config_release/release_*")],
    io:format("ReleaseVersions=~p~n", [ReleaseVersionStrings]),
    ReleaseVersions = [list_to_integer(V)||V<-ReleaseVersionStrings],
    LastVersion = lists:max(ReleaseVersions),
    io:format("LastVersion=~p~n", [LastVersion]),
    NewVersion = LastVersion + 1,
    ConfigFileNames = [FileName||"config/"++FileName<-filelib:wildcard("config/*.config")],
    case file:make_dir("config_release/release_"++integer_to_list(NewVersion)) of
		ok ->
			lists:foreach(
		                fun(FileName)->
		                        file:copy("config/"++FileName, "config_release/release_"++integer_to_list(NewVersion)++"/"++FileName)
		                end,
		                ConfigFileNames
		        ),
			io:format("Complete Release Schema Config Files.~n");
		{error, Error} ->
			io:format("Copy files error : ~p~n", [Error])
	end,
	init:stop().