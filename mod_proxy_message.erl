-module(mod_proxy_message).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-import(custom_odbc_queries, [get_active_participants/3]).

-export([start/2, stop/1]).
-export([proxy_message/1, is_chat_packet/1]).

start(Host, Opts) ->
        ?INFO_MSG("START MOD_PROXY_MESSAGE. ~p ~p",[Host, Opts]),
        ejabberd_hooks:add(filter_packet, global, ?MODULE, proxy_message, 3),
	ok.

stop(Host) ->
        ?INFO_MSG("STOP MOD_PROXY_MESSAGE.", []),
        ejabberd_hooks:delete(filter_packet, global, ?MODULE, proxy_message, 3),
	ok.

proxy_message({From, To, #xmlel{name = <<"message">>, attrs = Attrs, children = Children } = Packet}) ->

	?INFO_MSG("PROXY MESSAGE: ~p", [Packet]),

	case {is_chat_packet(Packet), is_proxyed(Packet)} of
		{true, true} -> {From, To, Packet};
		{true, false} ->

			%% For each active participant in the chat, send message.
			ParticipantsUsername =  custom_odbc_queries:get_active_participants(From#jid.lserver, From#jid.luser,To#jid.luser ),

			?INFO_MSG("Participants of ~p: ~p", [To#jid.luser, ParticipantsUsername]),

			JIDs = lists:map(fun(El)-> jlib:string_to_jid(list_to_binary(string:join([binary_to_list(El), "@", binary_to_list(To#jid.lserver)], ""))) end, ParticipantsUsername),

			lists:foreach(fun(JID)-> 
			
				case JID#jid.luser == From#jid.luser of
                                true ->
					?INFO_MSG("Sending message to self. Cancel.", []),
                                        []; %{From, To, Packet};
                                false ->
					?INFO_MSG("Sending message from: ~p to: ~p. Packet: ~p", [To, JID, Packet]),

					NewPacket = #xmlel{name = <<"message">>, attrs = lists:concat([Attrs, [{<<"proxyed">>, <<"true">>}]]), children = Children },

					ejabberd_router:route(To, JID, NewPacket)
                        	end
	
			end, JIDs),
			drop; %% drops original packet sent.
		_->
			?INFO_MSG("Attrs: ~p", [Attrs]),
			{From, To, Packet}
		end;

proxy_message(Packet)-> 
	?INFO_MSG("Not message packet: ~p", [Packet]),
Packet.

is_chat_packet(#xmlel{name = <<"message">>, attrs = Attrs, children = _ } = Packet)->
	case xml:get_attr_s(<<"type">>, Attrs) of
                <<"chat">> -> true;
		_-> false
	end;
is_chat_packet(_Packet)-> false.

is_proxyed(#xmlel{name = <<"message">>, attrs = Attrs, children = _ } = Packet)->
        case xml:get_attr_s(<<"proxyed">>, Attrs) of
                <<"true">> -> true;
                _-> false
        end;
is_proxyed(_Packet)-> false.
















