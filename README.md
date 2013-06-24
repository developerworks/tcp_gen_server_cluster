概述
======

本文描述了在集群中运行服务器的简单框架.集群中的服务器必须是`gen_server`.

首先解释一下服务器集群的含义, 以及为什么在集群中运行运行服务器有意义. 了解了关键的思路后,
通过一个简短的例子,我们将会详细的说明如何使用此框架来构建服务器集群.该框架仅由一个模块`gen_server_cluster`构成.在展示完整的代码前,
我们先要搞清楚如何使用它的API函数. 最后会给出一个 `gen_server_cluster`的使用实例: 分布式聊天系统

动机
===

通常,一个服务器集群由多个同时运行的服务器实例组成,对外提供一个单一的服务点, 从客户端的视角来看就像只有一个服务器一样.
集群的一个主要原因是增强服务的可靠性: 如果集群中的一个服务器由于网络问题或者其他原因不再可用, 集群中的其他服务器任然能够
提供同样的服务.因此要提供高可靠性,集群中的服务器最好能在不同的机器上运行.

由于Erlang内置的错误检测机制,使`gen_server`的实例达到一个特定级别的可靠性是容易的:让服务器进程被其他进程监控,
例如OTP supervisor, 当进程中止时,可以对其进行重启.

问题是,当服务器持有某些状态时,为了给客户端提供持续不断的服务, 被重启服务器进程的状态应该恢复到中止服务器状态的最近一个版本.
一个解决办法是,中止服务器时可以把这些状态写入数据库或者文件系统,重启服务器时,从其中读取状态数据,然后恢复运行, 但是这个解决办法带来了一个问题:
服务的可靠性完全依赖于数据库的可靠性.

与重启服务器,然后使之再次可用相比, 该框架允许服务器运行在一个集群中. 集群由一个动态可扩展的服务器群组成,其中每个服务器运行在不同的Erlang节点上.
这里面一个核心思想是,使用一个单独的进程负责分发客户端的请求,并且更新集群中所有服务器的状态, 保持它们同步.
如果一个活动的进程死亡, 所有其他的后台进程通过竞争,称为一个新的互动进程.但在他们之中仅有一个获胜.成为活动进程.
因此,任何时候,集群中只要有一个存活的服务器可用, 那么服务即是可用的.可以通过向集群中添加后台进程增强可靠性.

gen_server_cluster 框架上手
=================

集群框架由基于`gen_server`行为的单个模块`gen_server_cluster`构成. 精确地说, `gen_server_cluster`是`gen_server`的回调模块,导出handle_call, handle_cast等被`gen_server`调用的回调函数.
从另一方面讲,`gen_server_cluster` 本身是一个是一个要求目标模块的行为,比如, 被集群的服务器模块, 作为 `gen_server`的回调模块, 这允许 `gen_server_cluster` 把从`gen_server`接收到的
客户端请求委派给目标.下一节将详细地说明`gen_server_cluster`如何工作


如何工作
=====

当一个目标服务器使用 `gen_server:start`或`gen_server:start_link`作为一个正常的服务器启动时,
一个本地的或者全局的注册的进程等待从客户端进程使用`gen_server:call`发送的请求,要返回结果,
`gen_server`调用一个由目标模块提供的`handle_call`回调函数的一个匹配子句.

现在想一下当目标服务器使用`gen_server_cluster:start`函数启动并运行在一个集群中时会发生什么.
用`gen_server_cluster`作为回调模块调用`gen_server:start`注册一个任意的全局名称.
当客户端调用`gen_server:call`发起一个请求时,该进程接收到一个请求并调用`gen_server_cluster:handle_call`来处理客户的请求,并向客户端返回一个结果. 因为目标模块被包含在
服务器的状态(`state`)中, `gen_server_cluster:handle_call`通过调用目标模块的`handle_call`函数转发请求,最终为客户端获取返回值. 要注意的是特殊的情况,服务器的状态必须持有目标状态,
比如, 如果目标服务器作为一个正常的服务器启动持有的状态,目标状态作为一个参数传递给目标模块的任何回调函数.

到现在为止,集群仅有一个运行在某个节点上的服务器进程组成,这个节点成为`node1`.因为它是全局注册的进程,我们称之为全局服务器.为了用一个运行在不同节点上的一个后台进程来
扩展这个集群,这个不同的节点称之为`node2`,在`node2`上调用 `gen_server_cluster:start`(作为选择, 也可以用`node2`作为参数在任意连接的节点上调用`gen_server_cluster:start_local_server`)
这个启动函数检测全局服务器已经运行, 因此它启动一个新的进程作为后台服务器.因为它是在 `node2`上本地注册的, 我们称这个进程为本地服务器.
只要全局服务器还存活, 本地服务器的唯一用途就是保持它的状态和全局服务器的状态相同.一旦全局服务器或它的节点死亡, 这允许本地服务器替换全局服务器.
因此,在启动时,本地服务器请求当前的全局服务器的状态.同时全局服务器仍然处理任何客户端请求,另外它还必须发送每个状态更新给所有的本地服务器.

现在假设, 在有多个本地服务器的情况下,全局服务器由于某些错误, 比如, 节点故障而终止.
这个时候, 因为所有的本地服务器链接到全局服务器进程,并且trapping退出信号, 他们能够执行下面的动作.
每个本地服务器试图全局地注册自己为一个新的全局服务器.全局注册表确保仅有一个服务器注册成功.
所有其他的本地服务器则保持在后台.特别要注意的是,由于这个注册策略,在集群中新的全局服务器的选择是不确定的.


示例 1, 在集群中运行一个简单的服务器
======================

这个例子将帮助实际的理解 `gen_server_cluster`.
模块`id_server`作为一个最小的`gen_server`实例管理一个用作ID的单个整数. 调用`id_server:next_value`返回当前值,同时增加1

```erlang
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
    {ok, #state{}}.

handle_call(nextValue, _From, State) ->
    Value = State#state.id,
    {reply, Value, State#state{id = Value + 1}};

handle_call(stop, _From, State) ->
    {stop, normalStop, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

要在一个两个链接的Erlang节点组成的集群中运行`id_server`,首先使用`erl -sname node1`和`erl -sname node2`启动两个shell,
然后使用`net_adm:ping`建立这两个节点之间的连接.

在`node1`上调用`id_server:start_cluster`启动集群:

```erlang
Erlang (BEAM) emulator version 5.5.3 [async-threads:0]

Eshell V5.5.3  (abort with ^G)
(node1@hamburg)1> id_server:start_cluster().
id_server started as global server.
{ok,<0.47.0>}
(node1@hamburg)2> id_server:next_value().
0
```

接下来,在`node2`上启动第二个服务器成为本地服务器来扩展集群.为了演示通过集群提供的服务器可靠性, 我们使用`gen_server_cluster`模块的函数
`gen_server_cluster:stop(id_server,global)`停止当前的全局服务器(作为选择,我们可以简单地关闭`node1`) ,这个时候`node2`的输出显示, 之前在`node2`上的本地服务器现在
成为了新的全局服务器. 调用`id_server:next_value`的返回值证明`id_server`的状态正确地同步了.

```erlang
Erlang (BEAM) emulator version 5.5.3 [async-threads:0]

Eshell V5.5.3  (abort with ^G)
(node2@hamburg)1> id_server:start_cluster().
id_server started as local server.
{ok,<0.47.0>}
(node2@hamburg)2> id_server:next_value().
1
(node2@hamburg)3> gen_server_cluster:stop(id_server,global).
{stopByGenServerCluster,stopGlobalServer}
Global server on node1@hamburg terminated: stopGlobalServer.
New global server on node2@hamburg.
(node2@hamburg)4> id_server:next_value().
2
```

在`node1`上显示了一些停止信息.

```erlang
Server id_server stopped.
(node1@hamburg)3>
=ERROR REPORT==== 14-Apr-2007::16:36:34 ===
** Generic server id_server terminating
** Last message in was {stopByGenServerCluster,stopGlobalServer}
** When Server state == {state,id_server,
                               <0.47.0>,
                               [<3997.47.0>],
                               [],
                               id_server,
                               {state,2}}
** Reason for termination ==
** stopGlobalServer
```

gen_server_cluster 模块
=============


API 函数
-----------

在展示`gen_server_cluster`的视线代码之前,我们简要的说明一下它的API函数. 如上面的实例所示, 集群通过`start(Name,TargetModule,TargetArgs,Options)`启动或扩展,四个参数分别是:
集群的名称, 它的模块, 启动参数 以及`gen_server`选项.取决于集群是否存在, 一个全局服务器和本地服务器在执行吃函数的节点上启动.
要添加一个本地服务器到一个已有的集群, 简单地使用`start_local_server(Name)`, 同时省略不相干的参数.

函数`is_running(Name)`检查是否有一个注册为Name的集群正在运行. 关于集群中服务器的信息可通过`get_all_server_pids(Name)`和`get_all_server_nodes(Name)`获得.
前面一个返回全局服务器pid和所有本地服务器的pid列表, 后一个返回全局服务器节点和所有本地服务器节点的列表.
若需要, 目标服务器的状态可用`get_target_state(Name)`获得.

如果目标服务器的响应为一个stop, 集群终止. 你也可以停止集群中某个特定的服务器:

*  `stop(Name,global)` 停止当前全局服务器,
*  `stop(Name,all)` 停止整个集群,
*  `stop(Name,Node)` 停止运行在指定节点上的全局或本地服务器

最后两个API函数创建到目标进程的连接,设置trapping退出信号,来监控其他的进程.
如果被监控的进程终止, 目标服务器通过它的handle_info函数获得通知, 并使之执行一些合适的动作.
假设目标服务器使用`erlang:link`函数创建到其他经常的连接. 这意味着当前全局服务器进程被链接到那个进程,如果全局服务器死亡,该链接会丢失.
新的全局服务器没有设置到它的连接,因此不再能够继续监控其他的进程
该问题可以通过使用`link(Name,Pid)`来避免,而不是使用通常的link函数. 该函数发送一个异步请求创建一个到指定进程的链接.
该链接存储在全局服务器的状态中,并复制到所有本地服务器,当本地服务器成为一个新的全局服务器时,其中每一个都能连接自己到那个进程,
对应的删除连接的函数为`unlink(Name,Pid)`.



增强状态更新的效率
------------------------------------------------

多数由目标服务器回调函数返回的值包含新的目标状态. 因为状态会发送给所有服务器, 如果大量的状态在网络上传输, 状态更新可能是一个非常昂贵的操作.
更加高效的方式是,发送一个函数而非返回新的目标状态, 这将减少状态传输的网络开销.

`gen_server_cluster`服务器能处理下面两种返回类型:

* 如果目标服务器返回一个函数,该函数被解释为状态更新函数, 更新函数被全局服务器和任何本地服务器执行以获得新的目标状态.
* 否则, 返回值作为一个新的状态值处理.



实现
--------------


完整的 `gen_server_cluster`如下. 代码包含帮助理解的注释
The code contains some detailed comments that hopefully helps in understanding of what the functions actually do.
It is particularly helpful to see which kind of server (global or local) the various callback functions are meant for.



```erlang
粘贴注解完成的代码
```

实例2,一个高可用性分布式聊天系统
===================


Finally, we show that gen_server_cluster can be used for building applications beyond its primary purpose of increasing availability of a given server.
As an example, we present a pretty small chat application which has nonetheless some powerful features.
Writing chat servers in Erlang is very popular, most of them are designed as a classical client-server system relying on the availability of the central server.

In contrast, our chat application works without such a central server. The chat system is a cluster of chat instances started from different Erlang shells.
Each user has to start his own shell (on a different machine, to make sense) for communication with the chat system.
The system is started by starting a first instance and terminates when there is no more instance alive. During this time,
instances can dynamically be added to or removed from the cluster.
Any chat cluster is registered under an arbitrary name such that a user can participate in different chat sessions while using the same shell.

The chat module API is quite simple. Besides start and stop functions it provides a function for sending some text message
to the chat that is displayed on every users's shell combined with the chat name and the name of the node it is sent from.
The other two API functions lists all messages that are sent so far and the nodes (representing the users) currently involved.

As can be seen in the callback function for the send request, a state update function term is returned to add the new message to the message list.
However, this function has additionally the side-effect of writing the new message on the standard output.
Since all chat instances evaluate this update function, the message is displayed on the shell of every chat user.
Thus, the local servers which normally are only background processes are now playing an active role within this application.

```erlang
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
```

下面是一个示例会话,当James加入到该会话中.


```erlang
Erlang (BEAM) emulator version 5.5.3 [async-threads:0]

Eshell V5.5.3  (abort with ^G)
(james@hamburg)1> chat:start(chatroom).
chatroom started as local server.
{ok,<0.50.0>}
(james@hamburg)3> chat:get_users(chatroom).
[fred@hamburg,james@hamburg,peter@hamburg]
(james@hamburg)4> chat:get_all_msg(chatroom).
All messages of chatroom:
[fred@hamburg]: "Is there anybody out there?"
[peter@hamburg]: "Hi Fred."
[fred@hamburg]: "Hi Peter."
ok
(james@hamburg)5> chat:send(chatroom,"Hi all.").
[chatroom,james@hamburg]: "Hi all."
sent
[chatroom,peter@hamburg]: "Hi James."
[chatroom,fred@hamburg]: "Hi James."
```

结语
====

我们演示了一个在集群中运行服务器的框架来增强服务器的可靠性. 该框架使用具有状态复制功能的后天服务器.
他能够很好的应用在不依赖与位置的服务器,例如,文件服务器可能不能很好的使用gen_server_cluster进行集群.



