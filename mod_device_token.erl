-module(mod_device_token).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(NS_DEVICE_TOKEN, <<"who:iq:token">>).

-export([start/2, stop/1]).
-export([handle_local_iq/3]).
-export([set_device_token/2, get_token/1, update_device_token/2, token_exists/1]).
-export([set_session/4, unset_session/4, get_session_id/3]).

start(Host, Opts) ->
	?INFO_MSG("mod_device_token has started.", []),
	
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),

	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DEVICE_TOKEN, ?MODULE, handle_local_iq, IQDisc),
	ok.

stop(Host) ->
	?INFO_MSG("mod_integrated_session has stoped.", []),
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DEVICE_TOKEN),
	ok.

handle_local_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = get, sub_el = SubEl} = IQ) ->

        ?INFO_MSG("\n\nSubEl: ~p", [SubEl]),

        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, get_token(From)}]}]};

handle_local_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = set, sub_el = SubEl} = IQ) ->

	?INFO_MSG("\n\nSubEl: ~p", [SubEl]),
	?INFO_MSG("\n\nResult of get: ~p", [xml:get_subtag_cdata(SubEl, <<"token">>)]),

	%% Devices with old app may be sending old query. Check for that.
	{DeviceToken, DeviceType} = case xml:get_subtag_cdata(SubEl, <<"token">>) of
		<<>> ->
			%% Old query was sent
			{xml:get_tag_cdata(SubEl), <<"ios">>};
		_->
			Token = xml:get_subtag_cdata(SubEl, <<"token">>),
			Type = xml:get_subtag_cdata(SubEl, <<"type">>),
			{Token, Type}
	end,

	?INFO_MSG("\n\nDevice Token: ~p. Device Type: ~p", [DeviceToken, DeviceType]),
	
	case token_exists(From) of 
		false ->
			set_device_token(From, {DeviceToken, DeviceType}),
			?INFO_MSG("\n\nAdding Token to ~p. \n", [User]);
		true ->
			update_device_token(From, {DeviceToken, DeviceType}),
			?INFO_MSG("\n\nUpdating Token to ~p. \n", [User])
	end,

	?INFO_MSG("\n\nToken: ~p", [DeviceToken]),

        IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, DeviceToken}]}]}.

set_device_token(#jid{user = User, server = Server,
                      resource = Resource} = From, {DeviceToken, DeviceType}) ->
	ejabberd_odbc:sql_query(Server,
                                [<<"INSERT INTO device_tokens VALUES ('">>,User,<<"','">>,DeviceToken,<<"', '">>,DeviceType,<<"')">>]),

ok.

update_device_token(#jid{user = User, server = Server,
                      resource = Resource} = From, {DeviceToken, DeviceType}) ->
	ejabberd_odbc:sql_query(Server,
                                [<<"UPDATE device_tokens SET token='">>,DeviceToken,<<"', type='">>, DeviceType ,<<"' WHERE username='">>,User,<<"'">>]),

ok.

token_exists(#jid{user = User, server = Server,
                      resource = Resource} = From) ->

        {_,_, TokenResult} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT * FROM device_tokens WHERE username='">>,User,<<"' AND token IS NOT NULL">>]),

length(TokenResult) > 0.

get_token(#jid{user = User, server = Server,
                      resource = Resource} = From)->

	{_, _, Res} = {_,_, TokenResult} = ejabberd_odbc:sql_query(Server,
                                [<<"SELECT token FROM device_tokens WHERE username='">>,User,<<"' AND token IS NOT NULL LIMIT 1">>]),
	
	[[Token]] = case length(Res) > 0 of
		true ->
			Res;
		_ ->
			[[<<"">>]]
	end,
	
	?INFO_MSG("\n\nToken is: ~p", [Token]),

Token.

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
