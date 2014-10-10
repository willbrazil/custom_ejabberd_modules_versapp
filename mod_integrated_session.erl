-module(mod_integrated_session).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(NS_WHO_SESSION, <<"who:iq:session">>).

-export([start/2, stop/1, set_session/4, unset_session/4, get_session_id/3]).

start(Host, Opts) ->
	?INFO_MSG("mod_integrated_session has started.", []),
	ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, set_session, 50),
	ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, unset_session, 50),
	
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),

	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_WHO_SESSION, ?MODULE, get_session_id, IQDisc),
	ok.

stop(Host) ->
	?INFO_MSG("mod_integrated_session has stoped.", []),
	ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, set_session, 50),
	ejabberd_hooks:remove(unset_presence_hook, Host, ?MODULE, unset_session, 50),
	ok.


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
