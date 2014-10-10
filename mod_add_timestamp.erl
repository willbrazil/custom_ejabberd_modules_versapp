-module(mod_add_timestamp).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([start/2, stop/1]).
-export([add_time_property/1]).


start(Host, Opts) ->
    
	?INFO_MSG("START MOD_ADD_TIMESTAMP. ~p ~p",[Host, Opts]),

	ejabberd_hooks:add(filter_packet, global, ?MODULE, add_time_property, 10).

stop(Host) ->

	?INFO_MSG("STOP MOD_ADD_TIMESTAMP.", []),

	ejabberd_hooks:delete(filter_packet, global, ?MODULE, add_time_property, 10).

add_time_property({ _, _, #xmlel{name = <<"message">>, attrs = [{<<"type">>,<<"normal">>}], children = _} } = Packet) -> Packet;
add_time_property({ #jid{user = FromUser, server = Server, resource = _R} = From, #jid{user = ToUser, server = _S, resource = _Res} = To, #xmlel{name = <<"message">>, attrs = Attrs, children = SubEl} = Xml} = Packet) ->

                        ?INFO_MSG("\n\nPACKET IS:: ~p", [Packet]),

                        ?INFO_MSG("get_sub_tag properties:\n ~p", [xml:get_subtag(Xml, <<"properties">>)]),

                        BodyEl = xml:get_subtag(Xml, <<"body">>),
                        ThreadEl = xml:get_subtag(Xml, <<"thread">>),
                        #xmlel{name = PName, attrs = PAttrs, children = PList} = xml:get_subtag(Xml, <<"properties">>),

			{xmlel,<<"body">>,[],[{xmlcdata,Body}]}	= BodyEl,			

			case ToUser == FromUser of

                                false ->
			
					?INFO_MSG("insert message log: ~p(~p), ~p" , [ Body, byte_size(Body),
				
					ejabberd_odbc:sql_query(Server,
                                		[<<"INSERT INTO messages_log (to_user, from_user, body_length) VALUES ('">>,ToUser,<<"','">>,FromUser,<<"','">>,integer_to_list(byte_size(Body)),<<"')">>]) 

					]);

                                true ->
                                        []
                        end,

                        [Timestamp] = get_timestamp(),

                        TSTerm = list_to_binary(Timestamp),

                        ?INFO_MSG("\\n Timestamp is: ~p", [list_to_binary(Timestamp)]),

                        NewProp = #xmlel{name = PName, attrs = PAttrs, children = lists:append(PList, [#xmlel{name = <<"property">>, attrs = [], children = [#xmlel{ name = <<"name">>, attrs = [], children = [{xmlcdata, <<"time">>}]},  #xmlel{ name = <<"value">>, attrs = [{<<"type">>, <<"string">>}], children = [{xmlcdata, TSTerm}]}]}])},



                        NewXml = #xmlel{name = <<"message">>, attrs = Attrs, children = [BodyEl, ThreadEl, NewProp]},

        NewPacket = {From, To, NewXml},
NewPacket;
add_time_property(FullPacket)-> FullPacket.



add_time_property2({From, To, {xmlelement, PacketType, Attrs, SubEl}} = Packet) ->
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





get_timestamp()->
{Mega, Secs, _} = now(),
        CurrentTimestamp = io_lib:format("~p", [Mega*1000000 + Secs]),
CurrentTimestamp.
