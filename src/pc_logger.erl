-module(pc_logger).
-behaviour(gen_server).

-export([start_link/1,
         log/1]).

-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-record(state, {
          file
         }).

start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

log(Message) ->
    gen_server:call(?MODULE, {message, Message}).

init([Dir]) ->
    {ok, File} = file:open(Dir ++ "/packets.log", [append]),
    {ok, #state{file = File}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({message, Message}, _From, State) ->
    do_log(State#state.file, Message),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(File, {Key, TS, _IP, Msg}) ->
    file:write(File, [format_date(TS), " ", Msg, "\n"]).

format_date(Now) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_local_time(Now),
    {_, _, Nano} = Now,
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0B",
                  [Y, Mo, D, H, Mi, S, Nano]).
