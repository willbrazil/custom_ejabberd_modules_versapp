-module(mod_confession).
-author('willbrazil.usa@gmail.com').
-version('1.0').

%% Every ejabberd module must implement gen_mod.
%% gen_mod requires two functions: start and stop.
-behavior(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("custom_records.hrl").

-export([start/2, stop/1]).
-export([handle_confession_iq/3]).

%%Methods to interact with database
-export([destroy_confession/3]).
-export([create_confession/3, destroy_confession/3]).
-export([toggle_favorite/3]).

-import(mod_offline_post, [dispatch_confession_post/4]).

-import(mod_http_contacts_manager, [send_packet_all_resources/3, build_packet/2]).

-import(custom_odbc_queries, [
			insert_confession/2,
			get_confession/2,
			remove_confession/3,
			insert_confession_favorite/3,
			remove_confession_favorite/3,
			is_confession_favorited/3,
			get_seconds_since_last_push_notification/2,
			insert_last_push_notification_timestamp/2,
			update_last_push_notification_timestamp/2
			]).

start(Host, Opts) ->

	?INFO_MSG("START MOD_CONFESSION. ~p ~p",[Host, Opts]),
	
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_CONFESSION, ?MODULE, handle_confession_iq, IQDisc),

	?INFO_MSG("STARTED MOD_CONFESSION",[]).

stop(Host) ->
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_CONFESSION).


%%Respond to SET requests.
handle_confession_iq(#jid{user = User, server = Server,
                      resource = Resource} = From,
                 _To, #iq{type = set, sub_el = SubEl} = IQ) ->

	JIDString = jlib:jid_to_string(jlib:make_jid(User,Server, <<"">>)),

	#xmlel{children = SubEls} = SubEl,

	IQResponse = case xml:remove_cdata(SubEls) of
		[#xmlel{name = Name, attrs = Attrs, children = Els} = Tag| Rest] ->
			?INFO_MSG("\n\nName of tag is: ~p", [Name]),
			case Name of
				<<"create">> ->
					create_confession(From, Tag, IQ);
				<<"destroy">> ->
					destroy_confession(From, Tag, IQ);
				<<"toggle_favorite">> ->
					toggle_favorite(From, Tag, IQ)
			end;
		_ ->
			IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Action Does Not Exist">>}]}]}
	end,

IQResponse.


create_confession(#jid{user = User, server = Server,
                      resource = Resource} = JID, TagEl, IQ) ->

	Body = xml:get_subtag_cdata(TagEl, <<"body">>),
        ImageUrl = xml:get_subtag_cdata(TagEl, <<"image_url">>),
	
	Confession = custom_odbc_queries:insert_confession(Server, #confession{username = User, body = Body, image_url = ImageUrl}),

	?INFO_MSG("\n\nConfession: ~p", [Confession]),

	Result = string:join([binary_to_list(Confession#confession.id), binary_to_list(Confession#confession.created_timestamp)], ","),

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, iolist_to_binary(Result)}]}]}.


destroy_confession(#jid{user = User, server = Server,
                      resource = Resource}, TagEl,IQ) ->

	ConfessionId = xml:get_tag_attr_s(<<"id">>, TagEl),

	custom_odbc_queries:remove_confession(Server, User, ConfessionId),

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Confession Destroyed">>}]}]}.


toggle_favorite(#jid{user = User, server = Server,
                      resource = Resource} = JID, TagEl, IQ)->

	ConfessionId = xml:get_tag_attr_s(<<"id">>, TagEl),

	case custom_odbc_queries:is_confession_favorited(Server, User, ConfessionId) of
		true ->
			custom_odbc_queries:remove_confession_favorite(Server, User, ConfessionId);
		false ->
			custom_odbc_queries:insert_confession_favorite(Server, User, ConfessionId),

			send_confession_favorited_push_notification(Server, User, ConfessionId)
	end,

IQ#iq{type = result, sub_el = [{xmlel, "value", [], [{xmlcdata, <<"Confession Favortie Toggled">>}]}]}.


build_notification_packet(Body) ->
    {xmlel, <<"message">>,
     [{<<"type">>, <<"headline">>}, {<<"id">>, randoms:get_string()}],
     [


                #xmlel{name = <<"broadcast">>, attrs = [], 
			children = [
				#xmlel{ name = <<"type">>, attrs = [], children = [{xmlcdata, <<"confession_favorited">>}]},
				#xmlel{ name = <<"confession">>, attrs = [], children = [#xmlel{ name = <<"id">>, attrs = [], children = [{xmlcdata, list_to_binary(Body)}]}]}
			]}


                ]}.

send_confession_favorited_push_notification(Server, User, ConfessionId)->

	Confession = custom_odbc_queries:get_confession(Server, ConfessionId),

	SecondsSinceLast = custom_odbc_queries:get_seconds_since_last_push_notification(Server, Confession#confession.username),

	ShouldSend = case {SecondsSinceLast, not (User == Confession#confession.username)} of
		{none, true} ->
			custom_odbc_queries:insert_last_push_notification_timestamp(Server, Confession#confession.username),
 			true;
		{Seconds, true} when Seconds > 300 ->
			custom_odbc_queries:update_last_push_notification_timestamp(Server, Confession#confession.username),
			true;
		_->
			%% Do nothing
			false
	end,

	case ShouldSend of
		true ->
			dispatch_confession_post(Server, Confession#confession.username, <<"Someone favorited your thought">>, ConfessionId);
		false  -> []
	end,




	
		%%	FavoriteAlertJSON = lists:flatten(io_lib:format("~s", [ConfessionId])),
%%			%%send_packet_all_resources(Server, ToUsername, build_notification_packet(FavoriteAlertJSON)),

ok.



