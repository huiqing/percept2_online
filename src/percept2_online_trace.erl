%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(percept2_online_trace).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_trace(migration, Node, Receiver) ->
    {ok, _} = do_tracer([Node], Receiver),
    {ok, _} = dbg:p(all, [procs, running, scheduler_id]);
start_trace(inter_node, Nodes,Receiver) ->
    {ok, _} = do_tracer(Nodes,Receiver),
    {ok, _} = dbg:p(all, [send]).
                  
stop_trace() ->
    dbg:stop_clear().

do_tracer(Clients, Receiver) ->
    Res=lists:foldl(
          fun(N, {Cs, S}) ->
                  Host = case N of
                             nonode@nohost ->
                                 {ok, H} = inet:gethostname(),
				 H;
                             _ ->
				 [_,H] = string:tokens(atom_to_list(N),"@"),
                                 H
                         end,
                  case catch dbg:tracer(N,port,dbg:trace_port(ip,0)) of
                      {ok,N} ->
                          {ok,Port} = dbg:trace_port_control(N,get_listen_port),
                          {ok,_T} = dbg:get_tracer(N),
                          dbg:trace_client(ip,{Host,Port}, mk_trace_parser(Receiver)),
                      {[N|Cs], [N|S]};
                      Other ->
                          display_warning(N,{cannot_open_ip_trace_port,
                                             Host,
                                             Other}),
                          {Cs, S}
                  end             
          end, 
          {[],[]}, 
          Clients),
    {ok, Res}.
 
display_warning(Item,Warning) ->
    io:format("Warning: {~w,~w}~n",[Warning,Item]).

mk_trace_parser(Receiver) -> 
    {fun trace_parser/2, Receiver}.

trace_parser({trace, From, send, _Msg, To}, Receiver) ->
    FromNode = get_node_name(From),
    ToNode = get_node_name(To),
    case FromNode/=node() andalso ToNode/=node() 
        andalso FromNode/=ToNode of 
        true ->
            Receiver!{trace_inter_node, FromNode,ToNode},
            Receiver;        
        false ->
            Receiver
    end;
trace_parser(_Trace={trace, Pid, in, Rq, _MFA}, Receiver) ->
    case erlang:get({run_queue, Pid}) of 
        Rq ->
            Receiver;
        undefined ->
            erlang:put({run_queue, Pid}, Rq),
            Receiver;
        OldRq ->
            erlang:put({run_queue, Pid}, Rq),
            Receiver!{trace_rq, OldRq, Rq},
            Receiver
    end;
trace_parser(_Trace, Receiver) ->
    Receiver.


get_node_name({_RegName, Node}) ->    
    Node;
get_node_name(Arg) -> 
    try node(Arg) 
    catch _E1:_E2 ->
            nonode
    end.


run_orbit_with_trace() ->
    Nodes = ['node1@127.0.0.1', 'node2@127.0.0.1', 'node3@127.0.0.1'],
    rpc:call('node1@127.0.0.1', bench, dist_seq, 
             [fun bench:g124/1, 10000, 8,Nodes]).
    
%% percept2:profile("sim_code.dat", {sim_code,sim_code_detection, [["c:/cygwin/home/hl/git_repos/case_studies/par_sim_code/test"], 3, 40, 2, 4, 0.8, [], 8]},  [all]).
