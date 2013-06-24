-module(chat).

-behaviour(gen_server).

%% API
-export([start/1,
    stop/1,
    send/2,
    get_all_msg/1,
    get_users/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% 消息列表
-record(state, {msgList = []}).

start(Name) ->
    gen_server_cluster:start(Name, ?MODULE, [], []).

send(Name, Text) ->
    gen_server:call({global, Name}, {send, {Name, node(), Text}}).

get_all_msg(Name) ->
    io:format("All messages of ~p:~n", [Name]),
    MsgList = gen_server:call({global, Name}, get_all_msg),
    F = fun({Node, Text}) ->
        io:format("[~p]: ~p~n", [Node, Text])
    end,
    lists:foreach(F, MsgList),
    ok.

get_users(Name) ->
    {GlobalNode, LocalNodeList} = gen_server_cluster:get_all_server_nodes(Name),
    [GlobalNode | LocalNodeList].

stop(Name) ->
    gen_server:call({global, Name}, stop).

init([]) ->
    {ok, #state{}}.

handle_call({send, {Name, Node, Text}}, _From, _State) ->
    F = fun(State) ->
        io:format("[~p,~p]: ~p~n", [Name, Node, Text]),
        NewMsgList = [{Node, Text} | State#state.msgList],
        State#state{msgList = NewMsgList}
    end,
    {reply, sent, F};

handle_call(get_all_msg, _From, State) ->
    List = lists:reverse(State#state.msgList),
    {reply, List, State};

handle_call(stop, _From, State) ->
    {stop, normalStop, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.