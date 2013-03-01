-module(erlexiftool_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    %% Suppress so we can have some peace.
    error_logger:tty(false),
    application:start(erlexiftool).

cleanup(_Arg) ->
    application:stop(erlexiftool),
    error_logger:tty(true).

basic_test_() ->
    {
      setup,
      fun setup/0,
      fun cleanup/1,
      fun(_Arg) ->
              FilePath = code:lib_dir(erlexiftool)++"/"++?FILE,
              Proplist = erlexiftool:parse(FilePath),
              {filename, BinDir} = proplists:lookup(filename, Proplist),
              ?_assertMatch("erlexiftool_tests.erl", binary_to_list(BinDir))
      end
    }.
