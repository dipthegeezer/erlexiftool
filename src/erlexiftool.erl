-module(erlexiftool).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, parse/1]).

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

parse(File) ->
    gen_server:call(?MODULE, {parse, File}).

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
    Port = open_port({spawn, Priv++"/exiftool/bin/exiftool-server"}, [{packet,2}, binary]),
    {ok, #state{port = Port}}.

handle_call({parse, Args}, _From, #state{port = Port} = State) ->
    port_command(Port, term_to_binary(Args)),
    receive
        {Port, {data, Any}} ->
            Response = binary_to_term(Any),
            {reply, Response, State}
    after 5000 ->
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
