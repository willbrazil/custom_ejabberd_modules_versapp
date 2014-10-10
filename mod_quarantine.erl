-module(mod_quarantine).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-import(custom_odbc_queries, [get_device_version/2]).

-export([start/2, stop/1]).
-export([intercept_message/1]).

start(Host, Opts) ->
        ?INFO_MSG("START MOD_QUARANTINE. ~p ~p",[Host, Opts]),
        ejabberd_hooks:add(filter_packet, global, ?MODULE, intercept_message, 1),
        ok.

stop(Host) ->
        ?INFO_MSG("STOP MOD_QUARANTINE.", []),
        ejabberd_hooks:delete(filter_packet, global, ?MODULE, intercept_message, 1),
        ok.

intercept_message({From, To, #xmlel{name = <<"message">>, attrs = Attrs, children = Children } = Packet}) ->

	TargetVersion = gen_mod:get_module_opt(From#jid.server, ?MODULE, target_version, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),

        ?INFO_MSG("QUARANTINE: (Target Version: ~p) ~p", [TargetVersion, Packet]),

	case xml:get_attr_s(<<"type">>, Attrs) of
                <<"chat">> ->

			NewChildren = lists:map(fun({xmlel, Tag, ChildrenAttrs,CData} = Standard)-> case Tag of <<"body">> -> #xmlel{name = <<"body">>, attrs = ChildrenAttrs, children = [{xmlcdata, <<"Automated Message From Versapp: Please, download the latest version of Versapp.">>}]} ; _-> Standard end end, Children),				
			
			NewPacket = #xmlel{name = <<"message">>, attrs = Attrs, children = NewChildren },
			
			DeviceVersion = custom_odbc_queries:get_device_version(From#jid.lserver, From#jid.luser),

			case DeviceVersion of
				Version when Version >= TargetVersion ->
                                        {From, To, Packet};
				_ ->
					case From#jid.user == To#jid.user of
						true ->
							drop;
						_->
						 	{From, From, NewPacket}
					end
			end;
                _->
                        ?INFO_MSG("Attrs: ~p", [Attrs]),
                        {From, To, Packet}
                end;

intercept_message(Packet)->
        ?INFO_MSG("Not message packet: ~p", [Packet]),
Packet.





