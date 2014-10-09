%%%----------------------------------------------------------------------

%%% File    : mod_offline_post.erl
%%% Author  : Adam Duke <adam.v.duke@gmail.com>
%%% Purpose : Forward offline messages to an arbitrary url
%%% Created : 12 Feb 2012 by Adam Duke <adam.v.duke@gmail.com>
%%%
%%%
%%% Copyright (C) 2012   Adam Duke
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline_post).
-author('rgeorge@midnightweb.net').

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 send_notice/3,
	 send_notice_group/3,
	 dispatch_post_by_type/6,
	 dispatch_confession_post/4,
	 dispatch_new_friend_post/4,
	 get_active_group_participants/1]).

-import(custom_odbc_queries, [
		get_device_info/2
	]).

-define(PROCNAME, ?MODULE).
-define(SERVER, <<"versapp.co">>).
-define(PARTICIPANT_ACTIVE_STATUS, <<"active">>).

%% These are arguments sent to the push notification service to indicate what kind of
%% push notification is being sent (message, confession,...)
-define(MESSAGE_PUSH_NOTIFICATION_TYPE, <<"message">>).
-define(CONFESSION_PUSH_NOTIFICATION_TYPE, <<"confession">>).
-define(NEW_FRIEND_PUSH_NOTIFICATION_TYPE, <<"new_friend">>).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_post with Opts: ~p", [Opts]),
    AuthToken = gen_mod:get_module_opt(Host, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("default")),

    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     proc_lib:spawn(?MODULE, init, [Host, Opts])),
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ?INFO_MSG("Starting mod_offline_post. Host: ~p", [Host]),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_notice_group, 11),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_offline_post.", [] ),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, send_notice, 10),
    ok.

send_notice(From, To, Packet) ->
    ?INFO_MSG("\n\n\n\nsend_notice(): \n", []),
    
    Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
    Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
    Thread = xml:get_path_s(Packet, [{elem, list_to_binary("thread")}, cdata]),

    ConnectionToken = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),

    ?INFO_MSG("DISPATCH_POST_BY_TYPE:\nType: ~p - Body: ~p\n", [Type, Body]),
    dispatch_post_by_type(Type, From, To, Body, ConnectionToken, [{"thread", Thread}, {"type", ?MESSAGE_PUSH_NOTIFICATION_TYPE}]).


%% 'groupchat' messages do not activate offline_message_hook so I had to create this method hooked with 'user_send_packet'. This is a little hacky but needed to do.
send_notice_group(From, To, #xmlel{name = <<"message">>, attrs = Attrs, children = Children} = Packet)->

    Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
    Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
    Thread = xml:get_path_s(Packet, [{elem, list_to_binary("thread")}, cdata]),
    ConnectionToken = gen_mod:get_module_opt(From#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),

    ?INFO_MSG("\n\n\nSEND_NOTICE_GROUP", []),

    %% This check is a little redundant but needs to be here otherwise the hook will activate twice when 'chat' messages are sent.
    case Type of
        <<"groupchat">> ->
                dispatch_post_by_type(Type, From, To, Body, ConnectionToken, [{"thread", Thread}, {"type", ?MESSAGE_PUSH_NOTIFICATION_TYPE}]);
        _->
                ok
    end,
ok;
send_notice_group(_, _, _)-> ok.


%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

dispatch_post_by_type(<<"chat">>, From, To, Body, ConnectionToken, ExtraParams)->

	?INFO_MSG("Dispatching post chat. Type: ~p", [ExtraParams]),

	send_custom_post(From#jid.lserver, From#jid.luser, To#jid.luser, Body, ConnectionToken, ExtraParams);

dispatch_post_by_type(<<"groupchat">>, From, To, Body, ConnectionToken, ExtraParams)->

	Participants = get_active_group_participants(To#jid.luser),

	FilteredParticipants = lists:delete(From#jid.luser, Participants),

	?INFO_MSG("\n\nGROUPCHAT DISPATCH: From: ~p, To: ~p\nParticipants: ~p", [From, To, FilteredParticipants]),

	lists:foreach( fun(Participant)->
		?INFO_MSG("\nSending to participant in group (offline): ~p", [Participant]),
		send_custom_post(From#jid.lserver, From#jid.luser, Participant, Body, ConnectionToken, ExtraParams)
	end, FilteredParticipants),	
	

        ok;
dispatch_post_by_type( Type, From, To, Body, ConnectionToken, ExtraParams)->
	?INFO_MSG("I don't know how to dispatch this type of message: ~p", [Type]),
        ok.

dispatch_confession_post(Server, ToUsername, Body, ConfessionId )->
        
        ?INFO_MSG("\nSending THOUGHT notification to ~p. Body: ~p", [ToUsername, Body]),

        ConnectionToken = gen_mod:get_module_opt(Server, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
 
        send_custom_post(Server, <<"Versapp.Thoughts">>, ToUsername, Body, ConnectionToken, [{"confession_id", ConfessionId}, {"type", ?CONFESSION_PUSH_NOTIFICATION_TYPE}]),
        ?INFO_MSG("\nThought Notification Sent", [ ]).


dispatch_new_friend_post(Server, ToUsername, Body, NewFriendUsername )->
        ?INFO_MSG("\nDispatching new_friend_post. NewFriendUsername: ~p. To: ~p", [NewFriendUsername, ToUsername]),

        ConnectionToken = gen_mod:get_module_opt(Server, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),

        send_custom_post(Server, <<"Versapp.NewFriend">>, ToUsername, Body, ConnectionToken, [{"username", NewFriendUsername}, {"type", ?NEW_FRIEND_PUSH_NOTIFICATION_TYPE}]),
        ?INFO_MSG("\nNew Friend Notification Sent", [ ]).

% ExtraParamList follows: [{key,val},{key,val},...]


send_custom_post(Server, FromString, ToString, Body, ConnectionToken, ExtraParamList) ->

	{DeviceToken, DeviceType} = custom_odbc_queries:get_device_info(Server, ToString),

	%% Based on device type, proxy to correct url.
	PostUrl = case DeviceType of
		<<"android">> ->
			gen_mod:get_module_opt(Server, ?MODULE, android_post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary(""));
		<<"ios">> ->
			gen_mod:get_module_opt(Server, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))
	end,

	?INFO_MSG("Dispatching post for user ~p to URL: ~p", [ToString, PostUrl]),

	%% Takes a list of tuples representing key,val pairs and transforms it into a post param string
	ExtraPostParamString = lists:flatten(string:join(lists:map(fun(El)-> {Key, Val} = El,  [Key, "=", Val]  end, ExtraParamList), "&")),

	Sep = "&",
        Post = [
          "token=", ConnectionToken, Sep,
          "to=", ToString, Sep,
          "from=", FromString, Sep,
          "body=", url_encode(binary_to_list(Body)), Sep,
          "access_token=", DeviceToken] ++ Sep ++ ExtraPostParamString,

	?INFO_MSG( "\nPost associated with notification. ~p", [ list_to_binary(Post) ] ),

        httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)}, [], []),
	?INFO_MSG("\n\nSent123", []),
ok.


get_active_group_participants(ChatId)->
	{_,_, Result} = ejabberd_odbc:sql_query(?SERVER,
                                [<<"SELECT p.username FROM participants p WHERE chat_id='">>,ChatId,<<"' AND status='active' AND username NOT IN (SELECT username FROM session)">>]),

	lists:flatten(Result).
