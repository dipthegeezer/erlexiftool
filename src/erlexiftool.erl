-module(erlexiftool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% Server state.
%% ------------------------------------------------------------------
-record(state, {port}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    %% Work out directory of execution
    Priv = code:priv_dir(erlexiftool),
    io:fwrite("PRIV ~p~n",[Priv]),
    Port = open_port({spawn, Priv++"/exiftool/bin/exiftool-server"}, [stream,exit_status]),
    {ok, #state{port = Port}}.

handle_call({parse, Args}, _From, #state{port = Port} = State) ->
    port_command(Port, Args),
    case collect_response(Port) of
        {response, Response} ->
            {reply, Response, State};
        timeout ->
            {stop, port_timeout, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
collect_response(Port) ->
    collect_response(Port, [], []).

collect_response(Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            {response, lists:reverse(RespAcc)};

        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);

        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc])
    after 5000 ->
            timeout
    end.
