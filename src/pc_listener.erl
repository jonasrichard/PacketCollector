-module(pc_listener).
-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3]).

-record(state, {
         socket
         }).

start_link(Port, Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Dir], []).

init([Port, _Dir]) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once}]),
    {ok, #state{socket = Socket}}.

handle_info({udp, _Client, Host, _Port, Bin}, #state{socket = Socket} = State) ->
    IP = format_ip(Host),
    parse(IP, Bin),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse(IP, <<_Version:8, SourceId:32, StreamId:32, PartId:32,
            Last:8, _Size:16, Payload/binary >>) ->
    Last2 = Last =/= 0,
    pc_collector:collect(IP, SourceId, StreamId, PartId, Last2, Payload).

format_ip({H1, H2, H3, H4}) ->
    io_lib:format("~p.~p.~p.~p", [H1, H2, H3, H4]).

