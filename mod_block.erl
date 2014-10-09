-module(mod_block).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(NS_BLOCK, <<"who:iq:block">>).

-define(BLOCK_TYPE_EXPLICIT_USER, <<"explicit_user">>).
-define(BLOCK_TYPE_IMPLICIT_USER, <<"implicit_user">>).
-define(BLOCK_TYPE_EXPLICIT_GROUP, <<"explicit_group">>).
-define(BLOCK_TYPE_USER_IN_GROUP, <<"user_in_group">>).

-export([start/2, stop/1]).
-export([handle_block_iq/3]).
-export([block/3, unblock/3]).
-export([insert_into_blocked_list/4, insert_into_blocked_list/3]).
-export([is_user_blocked_in_group/4, is_user_blocked/3]).
-export([get_implicit_blocked_users/1, get_explicit_blocked_users/1, get_blocked_users/2]).
-export([cancel_blocked_packets/1]).

-export([get_blocked_explicit_users/2, get_blocked_explicit_groups/2, filter_who_packet/1, is_one2one_blocked/3, filter_message/5,
		block_message_body/1, is_user_blocked_within_group/4, filter_who_packet_on_user_receive/4, on_user_send_packet/3, is_user_blocked_implicit/3,
		get_group_sender_id/1, add_time_property/1, get_timestamp/0]).

start(Host, Opts) ->
	?INFO_MSG("mod_block has started.", []),
		
	ejabberd_hooks:add(filter_packet, global, ?MODULE, cancel_blocked_packets, 10),

	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 0),

	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_BLOCK, ?MODULE, handle_block_iq, IQDisc),
	ok.

stop(Host) ->
	?INFO_MSG("mod_block has stoped.", []),

	%%ejabberd_hooks:delete(filter_packet, global, ?MODULE, cancel_blocked_packets, 10),
	
	%%ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, filter_who_packet_on_user_receive, 0),
	ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 0),
	
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_BLOCK),
	ok.


handle_block_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = get, sub_el = SubEl} = IQ) ->

	BlockedUsers = get_explicit_blocked_users(From),

	Filtered = lists:map(fun(BlockedUser)->

                        lists:map(fun(Term)->
                                case Term of
                                        null ->
                                                "";
                                        _ ->
                                                binary_to_list(Term)
                                end
                         end, BlockedUser)
        end,BlockedUsers),


        Filtered2 = lists:map(fun(BlockedUser)->

                lists:flatten(io_lib:format("~p", [BlockedUser]))

        end, Filtered),


IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, list_to_binary(Filtered2)}]}]};
handle_block_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = set, sub_el = SubEl} = IQ) ->

	#xmlel{children = SubEls} = SubEl,
        IQResponse = case xml:remove_cdata(SubEls) of
                [#xmlel{name = Name, attrs = Attrs, children = Els} = Tag| Rest] ->
                        case Name of
                                <<"block">> ->
                                	block(From, Tag, IQ);
				<<"unblock">> ->
					unblock(From, Tag, IQ)
			end;
                _ ->
                        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Action Does Not Exist">>}]}]}
        end,

IQResponse.


block(#jid{user = User, server = Server,
                      resource = Resource} = From, Tag, IQ) ->

	Type = xml:get_subtag_cdata(Tag, <<"type">>),
	BlockedUsername = xml:get_subtag_cdata(Tag, <<"username">>),
	
	case Type of
		?BLOCK_TYPE_EXPLICIT_USER ->
	
			case is_user_blocked(From, BlockedUsername, Type) of
				true ->
					[];
				false ->
					insert_into_blocked_list(From, BlockedUsername, Type)
			end;
		?BLOCK_TYPE_IMPLICIT_USER ->
			case is_user_blocked(From, BlockedUsername, Type) of
                                true ->
                                        [];
                                false ->
                                        insert_into_blocked_list(From, BlockedUsername, Type)
                        end;
		?BLOCK_TYPE_USER_IN_GROUP ->
			ChatId  = xml:get_subtag_cdata(Tag, <<"chat_id">>),
                        
			case is_user_blocked_in_group(From, BlockedUsername, ChatId, Type) of
				true ->
					[];
				false ->
					insert_into_blocked_list(From, BlockedUsername, ChatId, Type)
			end;	
		?BLOCK_TYPE_USER_IN_GROUP ->
			ChatId  = xml:get_subtag_cdata(Tag, <<"chat_id">>),

                        case is_user_blocked_in_group(From, BlockedUsername, ChatId, Type) of
                                true ->
                                        [];
                                false ->
                                        insert_into_blocked_list(From, BlockedUsername, ChatId, Type)
                        end
	end,

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"User Blocked">>}]}]}.

unblock(#jid{user = User, server = Server,
                      resource = Resource} = From, Tag, IQ) ->

	BlockedUsername  = xml:get_subtag_cdata(Tag, <<"username">>),
	Type  = xml:get_subtag_cdata(Tag, <<"type">>),
	ChatId  = xml:get_subtag_cdata(Tag, <<"chat_id">>),

	case Type of
		MyType when MyType == ?BLOCK_TYPE_EXPLICIT_USER; MyType == ?BLOCK_TYPE_IMPLICIT_USER ->
			ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM blocked_list WHERE username='">>,User,<<"' AND blocked_username='">>,BlockedUsername,<<"' AND type='">>,Type,<<"'">>]);
		MyType when MyType == ?BLOCK_TYPE_EXPLICIT_GROUP; MyType == ?BLOCK_TYPE_USER_IN_GROUP ->
			ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM blocked_list WHERE username='">>,User,<<"' AND blocked_username='">>,BlockedUsername,<<"' AND type='">>,Type,<<"' AND group_id='">>,ChatId,<<"'">>]);
		_ ->
			[]
	end, 

	?INFO_MSG("Blocking chatId: ~p", [ChatId]),

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"User Unblocked">>}]}]}.



insert_into_blocked_list(#jid{user = User, server = Server,
                      resource = Resource} = From, BlockedUsername, Type) ->

	ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO blocked_list (username, blocked_username, type) VALUES ('">>,User,<<"','">>,BlockedUsername,<<"', '">>,Type,<<"')">>]),

ok.

insert_into_blocked_list(#jid{user = User, server = Server,
                      resource = Resource} = From, BlockedUsername, ChatId, Type) ->

	ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO blocked_list (username, blocked_username, type, group_id) VALUES ('">>,User,<<"','">>,BlockedUsername,<<"', '">>,Type,<<"', '">>,ChatId,<<"')">>]),

ok.

is_user_blocked_in_group(#jid{user = User, server = Server,
                      resource = Resource} = From, BlockedUsername, ChatId, Type) ->

	{_,_, BlockedUser} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,User,<<"' AND type='">>,Type,<<"' AND blocked_username='">>,BlockedUsername,<<"' AND group_id='">>,ChatId,<<"'">>]),

length(BlockedUser) > 0.

is_user_blocked(#jid{user = User, server = Server,
                      resource = Resource} = From, BlockedUsername, Type) ->

	{_,_, BlockedUser} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,User,<<"' AND type='">>,Type,<<"' AND blocked_username='">>,BlockedUsername,<<"' AND group_id IS NULL">>]),

length(BlockedUser) > 0.


get_explicit_blocked_users(#jid{user = User, server = Server,
                      resource = Resource} = From) ->
	BlockedUsers = get_blocked_users(From, ?BLOCK_TYPE_EXPLICIT_USER),
BlockedUsers.


get_implicit_blocked_users(#jid{user = User, server = Server,
                      resource = Resource} = From) ->
	BlockedUsers = get_blocked_users(From, ?BLOCK_TYPE_IMPLICIT_USER),
BlockedUsers.

get_blocked_users(#jid{user = User, server = Server,
                      resource = Resource} = From, Type) ->

	{_,_, BlockedUsers} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,User,<<"' AND type='">>,Type,<<"'">>]),

BlockedUsers.


on_user_send_packet(#jid{user = User, server = Server,
                      resource = Resource} = From, #jid{user = ToUser, server = ToServer, resource = _R2} = To, #xmlel{name = <<"message">>, attrs = Attrs, children = Children} = Packet) ->


	%%If Sender has implicit blocked person he is sending the message to, implicit unblock that person.
	case is_user_blocked(From, ToUser, ?BLOCK_TYPE_IMPLICIT_USER) of
		true ->
			ejabberd_odbc:sql_query(Server,
                                [<<"DELETE FROM blocked_list WHERE username='">>,User,<<"' AND blocked_username='">>,ToUser,<<"' AND type='">>,?BLOCK_TYPE_IMPLICIT_USER,<<"'">>]);
		false ->
			[]
	end,

	?INFO_MSG("\n\nIntercepting Packet on_user_send..", []),
	?INFO_MSG("\nFrom: ~p", [User]),
	?INFO_MSG("\nTo: ~p", [ToUser]),
	?INFO_MSG("\nPacket: ~p" , [Packet]),
	?INFO_MSG("Is ~p implicit blocked by ~p? ~p" , [ToUser, User, is_user_blocked(From, ToUser, ?BLOCK_TYPE_IMPLICIT_USER)]),
	?INFO_MSG("Is ~p explicit blocked by ~p? ~p" , [ToUser, User, is_user_blocked(From, ToUser, ?BLOCK_TYPE_EXPLICIT_USER)]),
	

%%        case is_user_blocked_implicit(Server, MyUsername, Username) of
  %%              true ->
    %%                    unblock(Server, MyUsername, Username);
      %%          false ->
%%                        []
      %%  end,

Packet;
on_user_send_packet(From, To, Packet)-> Packet.



cancel_blocked_packets({#jid{user = FromUser, server = FromServer, resource = Res} = From, #jid{user = ToUser, server = ToServer, resource = _R2} = To, #xmlel{name = <<"message">>, attrs = Attrs, children = SubEl} = Xml} = Packet) ->


			NewPacket = case is_user_blocked(To, FromUser, ?BLOCK_TYPE_IMPLICIT_USER) of
				true ->
					drop;
				false ->

					case is_user_blocked_in_group(To, Res, FromUser, ?BLOCK_TYPE_USER_IN_GROUP) of			
						true ->
							?INFO_MSG("\n\n\n\nBlocked Packet: ~p", [Packet]),
                	                        	BodyEl = xml:get_subtag(Xml, <<"body">>),
    		        	                            ThreadEl = xml:get_subtag(Xml, <<"thread">>),
                               			         Properties = xml:get_subtag(Xml, <<"properties">>),

                		                        #xmlel{name = NameBody, attrs = AttrBody, children = ChildBody} = BodyEl,

                                		        NewBodyEl = #xmlel{name = NameBody, attrs = AttrBody, children = [{xmlcdata, <<"Message from blocked user">>}]},

                                      			  {From, To, #xmlel{name = <<"message">>, attrs = Attrs, children = [NewBodyEl, ThreadEl, Properties]}};
						false ->
							Packet
					end
				end,

NewPacket;
cancel_blocked_packets(Packet)-> 

	?INFO_MSG("Inside MOD_BLOCK. Not message packet. ~p", [Packet]),	

Packet.





%% DEPRICATED________________________________________________________________

get_blocked_explicit_users(Server, MyJid) ->
	{_,_, BlockedUsers} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,MyJid,<<"' && type='">>,?BLOCK_TYPE_EXPLICIT_USER,<<"'">>]),
        BlockedUsers.

get_blocked_explicit_groups(Server, MyJid) ->
	{_,_, BlockedGroups} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,MyJid,<<"' && type='">>,?BLOCK_TYPE_EXPLICIT_GROUP,<<"'">>]),
        BlockedGroups.

is_user_blocked_implicit(Server, MyJid, Username) ->
        {_,_, BlockedUsers} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,MyJid,<<"' && type='">>,?BLOCK_TYPE_IMPLICIT_USER,<<"' AND blocked_username='">>,Username,<<"'">>]),
        (length(BlockedUsers) > 0).





%%%=======================
%%%% Message Filtering

filter_who_packet({{jid, User, FromServer, Resouce, _, _, _} = From, {jid, MyUsername, Server, _, _, _, _} = To, {xmlelement, PacketType, Attrs, SubEl}} = Packet)->


	filter_message(Server, User, MyUsername, {PacketType, get_attr("type", Attrs)}, Packet).



filter_who_packet_on_user_receive(Jid, From, To, Packet) ->
	?INFO_MSG("USER RECEIVED PACKET. Jid: ~p. - From: ~p. - To: ~p. - Packet: ~p ", [Jid, From, To, Packet]),
Packet.

filter_message(Server, From, To, {"message", "chat"}, Packet) ->
	case {is_one2one_blocked(Server, To, From)} of
		{false} ->
			Packet;
		{true} ->
			?INFO_MSG("Received and BLOCKED Chat msg.", []),
			drop
		end;
filter_message(Server, From, To, {"message", "groupchat"}, Packet) ->
	%%get sender id
	?INFO_MSG("filter_message()", []),
	GroupId = From,
	SenderId = get_group_sender_id(Packet),
	?INFO_MSG("Is ~p blockedin group ~p??? ~p", [SenderId, From, is_user_blocked_within_group(Server, To, SenderId, GroupId)]),
	?INFO_MSG("Packet is: ~p", [Packet]),
	case {is_user_blocked_within_group(Server, To, SenderId, GroupId)} of
		{false} ->
			Packet;
		{true} ->
			block_message_body(Packet)
		end;
filter_message(Server, From, To, {"message", "headline"}, Packet) ->
        Packet;
filter_message(Server, From, To, {PacketType, AnyType}, Packet) when PacketType /= "message" ->
	?INFO_MSG("Received no msg packet.", []),
	Packet.


%%%=======================
%%%% XML processing

get_attr(Attr, XData) ->
    case lists:keysearch(Attr, 1, XData) of
        {Key, {_, Value}} -> Value;
        false -> false
    end.


add_time_property({From, To, {xmlelement, PacketType, Attrs, SubEl}} = Packet) ->
	?INFO_MSG("add_time_property()", []),
	EditedPacket = case {PacketType} of
                {"message"} ->
                        ?INFO_MSG("Adding time to packet: ~p ", [Packet]),

			[Body,
                                     Thread,
                                     PropertiesXml|_] = SubEl,

			{xmlelement,"properties",
                                      Xmlns,
                                      Properties} = PropertiesXml,
	
			MyTimestamp = io_lib:format("~p", [get_timestamp()]),

		        NewProperties = lists:append(Properties, [{xmlelement,"property",[],
                                               [{xmlelement,"name",[],
                                                 [{xmlcdata,<<"time">>}]},
                                                {xmlelement,"value",
                                                 [{"type","string"}],
                                                 [{xmlcdata,
                                                  MyTimestamp}]}]}]),

			?INFO_MSG("Added time to packet. New properties are: ~p", [NewProperties]),

			

		        NewSubEl = [Body, Thread, {xmlelement, "properties", Xmlns, NewProperties}],
			NewPacket = {From, To, {xmlelement, PacketType, Attrs, NewSubEl}},
			NewPacket;
               	_ ->
		Packet
	end,
EditedPacket.



get_timestamp() ->
	{Mega, Secs, _} = now(),
	Timestamp = Mega*1000000 + Secs,
Timestamp.


is_user_blocked_within_group(Server, MyUsername, User, GroupId) ->
	
	?INFO_MSG("Server: ~p - My JID: ~p - Possible blocked: ~p - Group Id: ~p", [Server, MyUsername, User, GroupId]),
	{_,_, BlockedUsers} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,
						MyUsername,<<"' AND blocked_username='">>,
						User,<<"' AND (((type='">>,
						?BLOCK_TYPE_EXPLICIT_USER,<<"') OR (type='">>,
						?BLOCK_TYPE_USER_IN_GROUP,<<"' AND group_id='">>,GroupId,<<"')))">>]),
	?INFO_MSG("Executed query to check for blocked users within group.", []),
        ?INFO_MSG("Blocked Users: ~p", [BlockedUsers]),
	(length(BlockedUsers) > 0).


is_one2one_blocked(Server, MyUsername, User) ->

	{_,_, BlockedUsers} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT blocked_username FROM blocked_list WHERE username='">>,MyUsername,<<"'
											AND blocked_username='">>,User,<<"' 
												AND (type='">>,?BLOCK_TYPE_EXPLICIT_USER,<<"' 
													OR type='">>,?BLOCK_TYPE_IMPLICIT_USER,<<"' )">>]),	
	(length(BlockedUsers) > 0).


block_message_body({From, To, {xmlelement, PacketType, Attrs, SubEl}} = Packet) ->
	[Body, T, P|E] = SubEl,

	NewBody = {xmlelement,"body", [], [{xmlcdata, <<"Message from blocked sender..">>}]},	
	
	NewPacket = {From, To, {xmlelement, PacketType, Attrs, [NewBody, T,P|E]}},

	?INFO_MSG("Body of msg is:::::::::::: ~p", [Body]),
NewPacket.



get_group_sender_id({From, To, {xmlelement, PacketType, Attrs, SubEl}} = Packet) ->
	?INFO_MSG("GETTING SENDER ID FROM PACKER: ~p", [Packet]),
	?INFO_MSG("SubEl: ~p", [SubEl]),

	[Body,
                                     Thread,
                                     PropertiesXml|_] = SubEl,

	?INFO_MSG("PropertiesXml: ~p", [PropertiesXml]),

                        {xmlelement,"properties",
                                      Xmlns,
                                      Properties} = PropertiesXml,

	?INFO_MSG("Properties: ~p", [Properties]),

	[{xmlelement, "property", [], [{xmlelement, "name",[], [{xmlcdata, <<"sender_id">>}]}, {xmlelement, "value", [{"type", "string"}], SenderIdCData}]}|Rest] = Properties, 


	
	
	SenderId =  xml:get_cdata(SenderIdCData),
	?INFO_MSG("SenderId: ~p", [SenderId]),

SenderId.

