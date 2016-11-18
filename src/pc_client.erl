-module(pc_client).

-export([send/6]).

-define(CHUNK, 1024).

%% UDPSocket must be opened in binary mode
send(UDPSocket, Host, Port, SourceId, StreamId, Binary) ->
    Parts = split(Binary),
    [send(UDPSocket, Host, Port, SourceId, StreamId, PartId, Last, Payload)
     || {PartId, Last, Payload} <- Parts].

send(UDPSocket, Host, Port, SourceId, StreamId, PartId, Last, Payload) ->
    Last2 = case Last of
                true ->
                    255;
                false ->
                    0
            end,
    Size = size(Payload),
    Packet = <<"1", SourceId:32, StreamId:32, PartId:32,
               Last2:8, Size:16, Payload/binary >>,
    gen_udp:send(UDPSocket, Host, Port, Packet).

split(Bin) ->
    lists:reverse(split(Bin, 1, [])).

split(Bin, Counter, Parts) ->
    case Bin of
        << Part:?CHUNK/binary, Rest/binary >> ->
            NewParts = [{Counter, false, Part} | Parts],
            split(Rest, Counter + 1, NewParts);
        _ ->
            %% The binary is the last, we cannot split it
            [{Counter, true, Bin} | Parts]
    end.
