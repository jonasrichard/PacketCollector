-module(pc_collector).
-behaviour(gen_server).

-export([start_link/0,
         collect/6]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-record(state, {
          tab
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

collect(IP, SourceId, StreamId, PartId, Last, PayLoad) ->
    gen_server:call(?MODULE, {recv, IP, SourceId, StreamId, PartId, Last, PayLoad}).

init(_) ->
    Tab = ets:new(packets, [named_table, public]),
    {ok, #state{tab = Tab}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({recv, IP, SourceId, StreamId, PartId, Last, PayLoad}, _From, State) ->
    handle_packet(State#state.tab, IP, SourceId, StreamId, PartId, Last, PayLoad),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_packet(Tab, IP, SourceId, StreamId, PartId, Last, PayLoad) ->
    Key = {SourceId, StreamId},
    Part = {PartId, Last, PayLoad},
    Stream = case ets:lookup(Tab, Key) of
                 [] ->
                     TS = os:timestamp(),
                     S = {Key, TS, IP, [Part]},
                     ets:insert(Tab, S),
                     S;
                 [{Key, TS, Parts}] ->
                     S =  {Key, TS, IP, [Part | Parts]},
                     ets:insert(Tab, S),
                     S
             end,
    case is_full_stream(Stream) of
        true ->
            Message = append_parts(Stream),
            pc_logger:log(Message),
            ets:delete(Tab, Key),
            ok;
        false ->
            ok
    end.

is_full_stream({_, _, _, Parts}) ->
    case lists:keyfind(true, 2, Parts) of
        false ->
            %% We don't have the last part yet
            false;
        _ ->
            is_all_packets(Parts, lists:seq(1, length(Parts)))
    end.

is_all_packets([], []) ->
    true;
is_all_packets([], _) ->
    false;
is_all_packets([{PartId, _, _} | Rest], Seq) ->
    Seq2 = lists:delete(PartId, Seq),
    is_all_packets(Rest, Seq2).

append_parts({Key, TS, IP, Parts}) ->
    Sorted = lists:sort(fun({Id1, _, _}, {Id2, _, _}) -> Id1 =< Id2 end, Parts),
    IoList = [PayLoad || {_, _, PayLoad} <- Sorted],
    Msg = iolist_to_binary(IoList),
    {Key, TS, IP, Msg}.

