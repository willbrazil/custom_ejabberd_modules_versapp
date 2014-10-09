%%%----------------------------------------------------------------------
%%% File    : 
%%% Author  : Will Brazil
%%% Purpose : Sample module that extends embedded ejabberd HTTP server
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_http_contacts_manager).
-author('willbrazil.usa@gmail.com').
-vsn('').
-define(ejabberd_debug, true).

-define(SERVER_IP, <<"versapp.co">>).
-define(JID_EXT, <<"@versapp.co">>).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
    ]).

-export([send_packet_all_resources/3, build_packet/2, build_completed_packet/2]).

-export([contact_exists/3, is_valid_http_request/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process([<<"store">>], Request) ->

	?INFO_MSG("\n\n\nREQUEST ~p", [Request]),

	%%Extract parameters we want from Request:
	{request,'POST',_PATH,_Q , _Something, _Undef  , _Lang  , Data , _A, _Host, _P , _T , Header} = Request,

	Username = is_valid_http_request(Request),

	?INFO_MSG("\n\nUsername: ~p", [Username]),

	case Username of
		false -> [];
		_->

	
		ContactList = string:tokens(binary_to_list(Data), ","),

		%% Formats user information in this format: ('current_username','contact_identifier'). Eg: ('w','34938439849385')
		FormattedList = lists:map(fun(El)-> lists:concat(["('",Username,"','",El,"')"]) end, ContactList),
	
		%% Formats SQL query inputs the way it will be used in the SELECT statement below.
		SQLFormattedContactList = lists:map(fun(El)-> string:join(["'",El,"'"], "") end, ContactList),

	{QueryResOne,QueryResTwo, Reg} = ejabberd_odbc:sql_query(?SERVER_IP,
                              [<<"SELECT upe.username FROM username_phone_email upe WHERE (  (  CONCAT(upe.ccode, upe.phone) IN (">>,string:join(SQLFormattedContactList, ","),<<") OR email IN (">>,string:join(SQLFormattedContactList, ","),<<") ) AND upe.username NOT IN (SELECT ru.username FROM rosterusers ru WHERE ru.username=upe.username AND ru.jid=CONCAT('">>,Username,<<"', '">>,binary_to_list(?JID_EXT),<<"')))">>]),


	?INFO_MSG("\n\n\n\n\nREG: ~p. \n\nQueryResOne: ~p. \n\nQUeryResTwo: ~p", [Reg, QueryResOne, QueryResTwo]),

		%%List containing information to add friends to user roster
		FriendList1 = lists:map(fun([El])-> string:join(["('",Username,"', '",string:concat(binary_to_list(El), binary_to_list(?JID_EXT)),"', 'temp_name', 'B', 'N', 'N', 'item')"], "") end, Reg),

		FriendList2 = lists:map(fun([El])-> string:join(["('",binary_to_list(El),"', '",Username, binary_to_list(?JID_EXT),"', 'temp_name', 'B', 'N', 'N', 'item')"], "") end, Reg),

		%%Combined list.
		TempFriendsSQL = lists:append(FriendList1, FriendList2),

		%% Remove any duplicates from list.
		Set = sets:from_list(TempFriendsSQL),
        	FriendsSQL = sets:to_list(Set),	



		%%TODO: Check if that user is blocked by user he's trying to add. If so, don't add. Also, if user has already tried to be friends, don't add them to roster as a friend.

		?INFO_MSG("\n\nFriendsSQL: ~p", [FriendsSQL]),

		case length(FriendsSQL) > 0 of
			true ->
		
				?INFO_MSG("\n\nSQL: ~p", [ejabberd_odbc:sql_query(?SERVER_IP,
                        	                [<<"INSERT IGNORE INTO rosterusers (username, jid, nick, subscription, ask, server, type) VALUES ">>,string:join(FriendsSQL, ",") ])]);

			false ->

				[]
		end,

		lists:foreach(fun(El)-> 
		
			%% SENDS BROADCAST TO USERS NOTIFYING THEM SOMEONE ADDED THEM TO THEIR ROSTER
			send_packet_all_resources(list_to_binary(string:concat(Username, binary_to_list(?JID_EXT))), list_to_binary(string:concat(El,binary_to_list(?JID_EXT))), build_packet(message_chat, [<<"You have a new friend in Versapp!">>]))

		end, Reg),


			send_packet_all_resources(list_to_binary(string:concat(Username, binary_to_list(?JID_EXT))), list_to_binary(string:concat(Username,binary_to_list(?JID_EXT))), build_completed_packet(message_chat, [<<"DONE">>])),

		
			#xmlel{name = <<"html">>,
           	attrs =
               		[{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
               	 	{<<"xml:lang">>, <<"en">>}, {<<"lang">>, <<"en">>}],
           	children =
              	 	[#xmlel{name = <<"head">>, attrs = [],
                      	 children =
                         	  [#xmlel{name = <<"meta">>,
                               		   attrs =
                                      		 [{<<"http-equiv">>, <<"Content-Type">>},
                                       		 {<<"content">>,
                                       		  <<"application/xml">>}],
                                   	children = []}
                            	]},
                	#xmlel{name = <<"body">>, attrs = [], children = []}]}
	end;
process([<<"produce_error">>], Request) ->
	"ERROR";
process(LocalPath, _Request) ->
	 #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>},
		{<<"xml:lang">>, <<"en">>}, {<<"lang">>, <<"en">>}],
	   children =
	       [#xmlel{name = <<"head">>, attrs = [],
		       children =
			   [#xmlel{name = <<"meta">>,
				   attrs =
				       [{<<"http-equiv">>, <<"Content-Type">>},
					{<<"content">>,
					 <<"application/xml">>}],
				   children = []}
			    ]},
		#xmlel{name = <<"body">>, attrs = [], children = []}]}.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok. 

%%%----------------------------------------------------------------------
%%% SUPOPRT METHODS
%%%----------------------------------------------------------------------

contact_exists(Server, Username, ContactId) ->

	%%See if contact is registered.
                {_,_, Reg} = ejabberd_odbc:sql_query(?SERVER_IP,
                                [<<"SELECT username FROM username_phone_email WHERE username='">>,Username,<<"' AND (CONCAT(ccode, phone)='">>,ContactId,<<"' OR email='">>,ContactId,<<"')">>]),

                ?INFO_MSG("Done w query", []),

                length(Reg) > 0.



send_packet_all_resources(FromJIDString, ToJIDString, Packet) ->
    FromJID = jlib:string_to_jid(FromJIDString),
    ToJID = jlib:string_to_jid(ToJIDString),
    ToUser = ToJID#jid.user,
    ToServer = ToJID#jid.server,
    case ToJID#jid.resource of
	<<>> ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, Packet);
	Res ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, Res, Packet)
    end.

send_packet_all_resources(FromJID, ToUser, ToServer, Packet) ->
    case ejabberd_sm:get_user_resources(ToUser, ToServer) of
	[] ->
	    send_packet_all_resources(FromJID, ToUser, ToServer, <<>>, Packet);
	ToResources ->
	    lists:foreach(
	      fun(ToResource) ->
		      send_packet_all_resources(FromJID, ToUser, ToServer,
						ToResource, Packet)
	      end,
	      ToResources)
    end.

send_packet_all_resources(FromJID, ToU, ToS, ToR, Packet) ->
    ToJID = jlib:make_jid(ToU, ToS, ToR),
    ejabberd_router:route(FromJID, ToJID, Packet).


build_packet(message_chat, [Body]) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"chat">>}, {<<"id">>, randoms:get_string()}],
     [{xmlel, <<"body">>, [], [{xmlcdata, Body}]},

%%		#xmlel{name = <<"broadcast">>, attrs = [], children = [#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"new_user">>}]}, #xmlel{ name = <<"username">>, attrs = [], children = [{xmlcdata, <<"username_goes_here">>}]},  #xmlel{ name = <<"full_name">>, attrs = [], children = [{xmlcdata, <<"Name goes here">>}]}   ]}
	
		#xmlel{name = <<"broadcast">>, attrs = [], children = [#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"new_user">>}]}   ]}

	
		]};
build_packet(message_headline, [Subject, Body]) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"headline">>}, {<<"id">>, randoms:get_string()}],
     [{xmlel, <<"subject">>, [], [{xmlcdata, Subject}]},
      {xmlel, <<"body">>, [], [{xmlcdata, Body}]}
     ]
    }.


build_completed_packet(message_chat, [Body]) ->
    {xmlel, <<"message">>, 
     [{<<"type">>, <<"chat">>}, {<<"id">>, randoms:get_string()}],
     [{xmlel, <<"body">>, [], [{xmlcdata, Body}]},
     
        
                #xmlel{name = <<"broadcast">>, attrs = [], children = [#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"new_user_request_completed">>}]}   ]}
                
                
                ]}.

is_valid_http_request({request,'POST',_PATH,_Q , _Something, _Undef  , _Lang  , Data , _A, _Host, _P , _T , Header} = Request)->

	[{Key, UserAndKey}] = lists:filter(fun({Key, Val})-> Key=='Authorization' end,Header),

        [_, Auth] = string:tokens(binary_to_list(UserAndKey), " "),

        [Username, AccessKey] = string:tokens(binary_to_list(base64:decode(Auth)), ":"),

	{_,_, Result} = ejabberd_odbc:sql_query(?SERVER_IP,
                                [<<"SELECT * FROM session WHERE username='">>,Username,<<"' AND session_key='">>,AccessKey,<<"'">>]),

	case length(Result) > 0 of
		true->
			Username;
		_->
			false
	end.
