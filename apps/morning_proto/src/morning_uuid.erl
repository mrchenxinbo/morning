-module(morning_uuid).

% -export([generate_muc_id/0, generate_msg_id/0, next_id/1]).

generate_muc_id() ->
    % generate_id().
    ok.

% generate_msg_id() ->
%     generate_id().

% generate_id() ->
%     {ok, MIDBin} = ticktick_id:id(),
%     _MIDInt = binary:decode_unsigned(MIDBin).    %%list_to_binary(integer_to_list(MIDInt)).

% next_id(ID) when is_integer(ID) ->
%     Next = next_id(binary:encode_unsigned(ID)),
%     binary:decode_unsigned(Next);
% next_id(ID) when is_binary(ID) ->
%     {ok, Next} = ticktick_id:sibling(ID),
%     Next.
