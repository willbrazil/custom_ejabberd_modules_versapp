-module(mod_http_notify).
-author("Guilherme Guedes").

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").
-include("custom_config.hrl").

-import(custom_odbc_queries, [get_friends_jid/2]).
-import(mod_offline_post, [send_new_friend_post/4]).

-export([start/2, stop/1, process/2]).

-define(NEW_FRIEND_NOTIFICATION_TYPE, <<"new_friend">>).

start(_Host, _Opts) -> 
	ok.
stop(_Host)-> ok.

process([<<"blm">>], #request{
			method = 'GET',
			path = _Path,
			q = Params,
			us = _Us,
			auth = _Auth,
			lang = _Lang,
			data = _Data,
			ip = _Ip,
			host = Host
			} = Request)-> 

	?INFO_MSG("Received Request: ~p", [?MAIN_HOSTNAME]),

	Username = lists:filtermap(fun(El)-> case El of {<<"username">>, Username} -> {true, Username}; _-> false end end, Params),
	
	notify_friends(?MAIN_HOSTNAME, Username);
process(_,_) -> 
	{501, [], <<"Not Implemented">>}.

notify_friends(_, [])-> {500, [], <<"No username provided.">>};
notify_friends(Server, [Username]) ->

	?INFO_MSG("\n\n~p resources: ~p", [Username, ejabberd_sm:get_user_resources(Username, Server)]),

	?INFO_MSG("get friends jid: ~p", [custom_odbc_queries:get_friends_jid(Server, Username)]),

	case custom_odbc_queries:get_friends_jid(Server, Username) of
                error ->
                        {500, [], <<"Internal Server Error">>};
                FriendsJid ->
                        lists:foreach(fun(Jid)->

				LoginStatus = case length(ejabberd_sm:get_user_resources(Jid#jid.luser, Server)) of
                                	0 -> offline;
                                	_ -> online
                        	end,

                                send_new_friend_notification(LoginStatus, Username, Jid)
                        end, FriendsJid),
                        {200, [], <<"OK">>}
        end.

send_new_friend_notification(online, NewFriendUsername, Jid)->

	Name = NewFriendUsername,

	Message = list_to_binary(string:join([ binary_to_list(Name), " has just joined Versapp."], "")),

	BroadcastInfo = [
				#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, ?NEW_FRIEND_NOTIFICATION_TYPE}]},
                                #xmlel{ name = <<"username">>, attrs = [], children = [{xmlcdata, NewFriendUsername}]}
			],

	ejabberd_router:route(jlib:string_to_jid(<<"notification@versapp.co">>), Jid, build_notification_packet(Message, BroadcastInfo ));
send_new_friend_notification(offline, NewFriendUsername, Jid)->

	Name = NewFriendUsername,
        Message = list_to_binary(string:join([ binary_to_list(Name), " has just joined Versapp."], "")),

	mod_offline_post:dispatch_new_friend_post(Jid#jid.lserver, Jid#jid.luser, Message, NewFriendUsername).

build_notification_packet(Body, BroadcastInfo)->
	{xmlel, <<"message">>,
     		[{<<"type">>, <<"headline">>}, {<<"id">>, randoms:get_string()}],
     		[
			{xmlel, <<"body">>, [], [{xmlcdata, Body}]},
                	#xmlel{
				name = <<"broadcast">>,
				attrs = [],
				children = BroadcastInfo
			}
		]
	}.
