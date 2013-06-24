-module(id_server).

-behaviour(gen_server).

%% API
-export([start_cluster/0, next_value/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {id = 0}).

start_cluster() ->
    gen_server_cluster:start(?MODULE, ?MODULE, [], []).

next_value() ->
    gen_server:call({global, ?MODULE}, nextValue).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

init([]) ->
    net_kernel:monitor_nodes(true),
    lager:info("[~p] Monitoring nodes...", [?MODULE]),
    {ok, #state{}}.

handle_call(nextValue, _From, State) ->
    Value = State#state.id,
    {reply, Value, State#state{id = Value + 1}};

handle_call(stop, _From, State) ->
    {stop, normalStop, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    lager:info("[~p] Node ~p added to the cluster.", [?MODULE,Node]),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    lager:info("[~p] Node ~p exited from the cluster.", [?MODULE, Node]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.