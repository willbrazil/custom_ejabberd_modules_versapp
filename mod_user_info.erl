-module(mod_user_info).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(NS_USER_INFO, <<"who:iq:info">>).

-export([start/2, stop/1]).
-export([handle_local_iq/3]).
-export([set_session/4, unset_session/4, get_session_id/3]).

start(Host, Opts) ->
	?INFO_MSG("mod_device_token has started.", []),
	
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),

	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_USER_INFO, ?MODULE, handle_local_iq, IQDisc),
	ok.

stop(Host) ->
	?INFO_MSG("mod_integrated_session has stoped.", []),
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_USER_INFO),
	ok.

handle_local_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = get, sub_el = SubEl} = IQ) ->

	
	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                                [<<"select ccode, phone, email from username_phone_email  where username='">>,User,<<"'">>]),


	Filtered = lists:map(fun(ConfessionTerms)->

                        lists:map(fun(Term)->
                                case Term of
                                        null ->
                                                "";
                                        _ ->
                                                binary_to_list(Term)
                                end
                         end, ConfessionTerms)
        end,Result),

        ?INFO_MSG("Result FILTERED: ~p", [Filtered]),



        Filtered2 = lists:map(fun(Confession)->

                lists:flatten(io_lib:format("~p", [Confession]))

        end, Filtered),


        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Filtered2)}]}]};


handle_local_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = set, sub_el = SubEl} = IQ) ->

	?INFO_MSG("\n\nSubEl: ~p", [SubEl]),

	CCode = xml:get_subtag_cdata(SubEl, <<"ccode">>),
	Phone = xml:get_subtag_cdata(SubEl, <<"phone">>),
	Email = xml:get_subtag_cdata(SubEl, <<"email">>),
	
	DeviceVersion = xml:get_subtag_cdata(SubEl, <<"version">>),

	case DeviceVersion of 
		
		[] ->
			custom_odbc_queries:set_device_version(Server, User, <<"1.0.0">>);
		_ ->
			custom_odbc_queries:set_device_version(Server, User, DeviceVersion)
	end,

	{_, _, Result} = ejabberd_odbc:sql_query(Server,
                                [<<"select username from username_phone_email  where username='">>,User,<<"'">>]),

	case (length(Result) > 0) of
		true ->
			ejabberd_odbc:sql_query(Server,
                                [<<"UPDATE username_phone_email SET ccode='">>,CCode,<<"', phone='">>,Phone,<<"', email='">>,Email,<<"' WHERE username='">>,User,<<"'">>]);
		false ->
			ejabberd_odbc:sql_query(Server,
                                [<<"insert into username_phone_email VALUES ('">>,User,<<"','">>, CCode, <<"', '">>,Phone,<<"', '">>,Email,<<"')">>])
	end,


        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, CCode}]}]}.


set_session(User, Server, _Resource, _Packet) ->
	?INFO_MSG("Setting Session for ~p on ~p", [User, Server]),
	Random = base64:encode(crypto:strong_rand_bytes(10)),
	ejabberd_odbc:sql_query(Server,
   	                        [<<"insert into session VALUES ('">>,User,<<"','">>, Random, <<"')">>]),
	none.

unset_session(User, Server, _Resource, _Packet) ->
	?INFO_MSG("DELETING Session for ~p on ~p", [User, Server]),
        ejabberd_odbc:sql_query(Server,
                                [<<"delete from session where username='">>,User,<<"'">>]),
	none.

get_session_id({Item, User, Server, _, _, _, _} = From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	?INFO_MSG("Getting IQ Packet for ~p", [From]),
	{_, _, [[SessionKey]]} = ejabberd_odbc:sql_query(Server,
                                [<<"select session_key from session  where username='">>,User,<<"'">>]),
	?INFO_MSG("SESSION ID IS: ~p", SessionKey),
	IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, SessionKey}]}]}.
