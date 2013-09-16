-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
               count =1          :: integer(),
               cmd   = undefined :: any(),
               nodes =[]         :: [node()],
               data  = []        :: any()
              }).                        

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    register(percept2_online, self()),
    {ok, Req, #state{}}.

websocket_handle({text, <<"stop">>}, Req, State) ->
    Cmd = State#state.cmd, 
    Nodes = State#state.nodes,
    stop_profiling(Cmd, Nodes),
    {shutdown, Req, State};
websocket_handle({text, <<"start">>}, Req, State) ->
    io:format("Profiling started ...\n"),
    {ok, Req, State};
websocket_handle({text, Msg}, Req, State) ->
    MsgStr=binary_to_list(Msg),
    case string:tokens(MsgStr, ":") of 
        ["start_profile",Feature, NodeStr] ->
            Nodes= string:tokens(NodeStr, ";"),
            Ns = [list_to_atom(N)||N<-Nodes],
            Cmd = list_to_atom(Feature),
            start_profiling(Cmd, Ns),
            NewState=State#state{cmd=Cmd, nodes=Ns},
            {reply, {text, << "Start profiling ...">>}, Req, NewState};
        _ ->
            Msg = "Unexpected message from client:"++ binary_to_list(Msg),
            {reply, {text,list_to_binary(Msg)}, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.


websocket_info({timeout, _Ref, stop_profile}, Req, State) ->
    {shutdown, Req, State};
websocket_info({timeout, _Ref, _Msg}, Req, State) ->
    Cnt = State#state.count,
    StateStr=case State#state.cmd of 
                 migration ->
                     Data = [{From, To, Times}||
                                {{From, To}, Times}<-State#state.data],
                     lists:flatten(
                       io_lib:format("~p.", 
                                     [{Cnt*200/1000,Data}]));
                 _ ->
                     lists:flatten(
                       io_lib:format("~p.", 
                                     [{Cnt*200/1000,
                                       State#state.data}]))
             end,
    erlang:start_timer(200, self(), <<"timeout">>),
    {reply, {text, list_to_binary(StateStr)}, Req, 
     State#state{count=Cnt+1, data=[]}};
websocket_info({trace_inter_node, From, To,MsgSize}, Req, State=#state{data=Data}) ->
    Key = {From, To},
    NewData=case lists:keyfind(Key, 1, Data) of
                false ->
                    [{{From, To},1, MsgSize}|Data];
                {_, Count, SumSize} ->
                    lists:keyreplace({From, To}, 1, 
                                     Data, {Key, Count+1, MsgSize+SumSize})
            end,
    {ok, Req, State#state{data=NewData}};
websocket_info({trace_rq, FromRq, ToRq}, Req, State=#state{data=Data}) ->
    Key = {FromRq, ToRq},
    NewData=case lists:keyfind(Key, 1, Data) of
                 false ->
                     [{{FromRq, ToRq},1}|Data];
                 {_, Count} ->
                     lists:keyreplace({FromRq, ToRq}, 1, 
                                      Data, {Key, Count+1})
             end,
    {ok, Req, State#state{data=NewData}};
websocket_info(_Info={run_queues_info, Ts, Rqs}, Req, State) ->
    Str=lists:flatten([" "++integer_to_list(Len)++" "
                       ||Len<-tuple_to_list(Rqs)]),
    InfoStr=lists:flatten(io_lib:format("~p ~s", [Ts, Str])),
    {reply, {text, list_to_binary(InfoStr)}, Req, State};
websocket_info(Info={s_group, _Node, _Fun, _Args}, Req, State) ->
    InfoStr=lists:flatten(io_lib:format("~p.", [Info])),
    {reply, {text, list_to_binary(InfoStr)}, Req, State};
websocket_info(start_profile, Req, State) ->
    erlang:start_timer(1, self(), <<"Online profiling started...!">>),
    {ok, Req, State};
websocket_info(stop_profile, Req, State) ->
    erlang:start_timer(1, self(), stop_profile),
    {ok, Req, State};
websocket_info(Info, Req, State) ->
    io:format("percept2_online_ws_handler:unexpected trace:~p\n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.


start_profiling(rq, [Node|_]) ->
    Res=rpc:call(Node, percept2_online_sampling, start,
                 [[run_queues], infinity, none, {percept2_online, node()}]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Percept2_online failed to start profiling for reason:\n~p\n",
                      [Reason]);
        _ -> Res
    end;
start_profiling(migration, [Node|_]) ->
    erlang:start_timer(1, self(), <<"Online profiling started...!">>),
    percept2_online_trace:start_trace(migration, Node, {percept2_online, node()});
start_profiling(rq_migration, [Node|_]) ->
    erlang:start_timer(1, self(), <<"Online profiling started...!">>),
    Res=rpc:call(Node, percept2_online_sampling, start,
                 [[run_queues], infinity, none,{percept2_online, node()}]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Percept2_online failed to start profiling for reason:\n~p\n",
                      [Reason]);
        _ -> 
            percept2_online_trace:start_trace(migration, Node,{percept2_online, node()})
    end;
start_profiling(inter_node, Nodes) ->
    erlang:start_timer(1, self(), <<"Online profiling started...!">>),
    percept2_online_trace:start_trace(inter_node, Nodes, {percept2_online, node()});
start_profiling(s_group, Nodes) ->
    percept2_online_trace:start_trace(s_group, Nodes, {percept2_online, node()});
start_profiling(Cmd, _Nodes) ->
    io:format("start_profiling: unnexpected command:~p\n", [Cmd]),
    ok.


stop_profiling(rq, [Node|_]) ->
    Res=rpc:call(Node, percept2_online_sampling, stop,[]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Percept2_online failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        _ -> 
           Res
    end;
stop_profiling(migration, [_Node|_]) ->
    percept2_online_trace:stop_trace();
stop_profiling(rq_migration, [Node|_]) ->
    Res=rpc:call(Node, percept2_online_sampling, stop,[]),
    case Res of 
        {badrpc, Reason} ->
            io:format("Percept2_online failed to stop profiling for reason:\n~p\n",
                      [Reason]);
        _ -> 
            Res
    end,
    erlang:start_timer(1, self(), stop_profile),
    percept2_online_trace:stop_trace();
stop_profiling(inter_node, _Nodes) ->
    erlang:start_timer(1, self(), stop_profile),
    percept2_online_trace:stop_trace();
stop_profiling(s_group, _Nodes) ->
    percept2_online_trace:stop_trace();
stop_profiling(undefined,_) ->
    ok;
stop_profiling(Cmd, _Nodes) ->
    io:format("stop_profiling: unnexpected command:~p\n", [Cmd]),
    ok. 

