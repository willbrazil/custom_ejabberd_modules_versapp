-module(custom_odbc_queries).

-author("Guilherme Guedes").

-include("ejabberd.hrl").
-include("logger.hrl").
-include("custom_records.hrl").

-export([insert_confession/2,
	 get_confession/2,
	 remove_confession/3]).

-export([insert_confession_favorite/3, 
	remove_confession_favorite/3, 
	is_confession_favorited/3]).

-export([get_device_info/2]).

-export([get_seconds_since_last_push_notification/2,
	insert_last_push_notification_timestamp/2,
	update_last_push_notification_timestamp/2]).

-export([get_active_participants/3]).

-export([get_device_version/2, set_device_version/3]).

-export([set_session_key/2, delete_session_key/2, get_session_key/2]).

-export([insert_message/5]).

%%%------------------------
%%% TABLES
%%%------------------------

%% Table: confessions
-define(CONFESSIONS_TABLE, <<"confessions">>).
-define(CONFESSIONS_TABLE_COLUMN_CONFESSION_ID, <<"confession_id">>).
-define(CONFESSIONS_TABLE_COLUMN_USERNAME, <<"jid">>).
-define(CONFESSIONS_TABLE_COLUMN_BODY, <<"body">>).
-define(CONFESSIONS_TABLE_COLUMN_IMAGE_URL, <<"image_url">>).
-define(CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<"created_timestamp">>).

%% Table: confession_favorites
-define(CONFESSION_FAVORITES_TABLE, <<"confession_favorites">>).
-define(CONFESSION_FAVORITES_TABLE_COLUMN_USERNAME, <<"jid">>).
-define(CONFESSION_FAVORITES_TABLE_COLUMN_CONFESSION_ID, <<"confession_id">>).

%% Table: last_push_notifications
-define(LAST_PUSH_NOTIFS_TABLE, <<"last_push_notifications">>).
-define(LAST_PUSH_NOTIFS_TABLE_COLUMN_USERNAME, <<"username">>).
-define(LAST_PUSH_NOTIFS_TABLE_COLUMN_TIMESTAMP, <<"last_push_timestamp">>).

%% Table: device_tokens
-define(DEVICE_TOKENS_TABLE, <<"device_tokens">>).
-define(DEVICE_TOKENS_TABLE_COLUMN_USERNAME, <<"username">>).
-define(DEVICE_TOKENS_TABLE_COLUMN_TOKEN, <<"token">>).
-define(DEVICE_TOKENS_TABLE_COLUMN_TYPE, <<"type">>).

%% Table: participants
-define(CHAT_TABLE, <<"chat">>).
-define(CHAT_TABLE_COLUMN_ID, <<"id">>).
-define(CHAT_TABLE_COLUMN_UUID, <<"uuid">>).

%% Table: participants
-define(PARTICIPANTS_TABLE, <<"participants">>).
-define(PARTICIPANTS_TABLE_COLUMN_CHAT_ID, <<"chat_id">>).
-define(PARTICIPANTS_TABLE_COLUMN_USERNAME, <<"username">>).
-define(PARTICIPANTS_TABLE_COLUMN_INVITED_TIMESTAMP, <<"invited_timestamp">>).
-define(PARTICIPANTS_TABLE_COLUMN_JOINED_TIMESTAMP, <<"joined_timestamp">>).
-define(PARTICIPANTS_TABLE_COLUMN_INVITED_BY, <<"invited_by">>).
-define(PARTICIPANTS_TABLE_COLUMN_STATUS, <<"status">>).

%% Table: device_version
-define(DEVICE_VERSION_TABLE, <<"device_version">>).
-define(DEVICE_VERSION_TABLE_COLUMN_USERNAME, <<"username">>).
-define(DEVICE_VERSION_TABLE_COLUMN_VERSION, <<"version">>).

%% Table: session
-define(SESSION_TABLE, <<"session">>).
-define(SESSION_TABLE_COLUMN_USERNAME, <<"username">>).
-define(SESSION_TABLE_COLUMN_SESSION_KEY, <<"session_key">>).

%% Table: messages
-define(MESSAGES_TABLE, <<"messages">>).
-define(MESSAGES_TABLE_COLUMN_ID, <<"id">>).
-define(MESSAGES_TABLE_COLUMN_CHAT_UUID, <<"chat_uuid">>).
-define(MESSAGES_TABLE_COLUMN_TIMESTAMP, <<"timestamp">>).
-define(MESSAGES_TABLE_COLUMN_BODY, <<"body">>).
-define(MESSAGES_TABLE_COLUMN_IMAGE_URL, <<"image_url">>).
-define(MESSAGES_TABLE_COLUMN_SENDER_USERNAME, <<"sender_username">>).

%%%------------------------
%%% Confession Queries
%%%------------------------

%% Create Confession
%% Return: confession_id.
-type insert_confession_result() :: #confession{} | {error, _}.

-spec insert_confession(binary(), #confession{}) -> insert_confession_result().

insert_confession(Server, #confession{username=Username, body=Body, image_url=ImageUrl} = Confession)->

	Res = ejabberd_odbc:sql_query(Server,
                                [
					<<"INSERT INTO ">>,?CONFESSIONS_TABLE,<<" (
					">>,?CONFESSIONS_TABLE_COLUMN_USERNAME,<<",
					">>,?CONFESSIONS_TABLE_COLUMN_BODY,<<",
					">>,?CONFESSIONS_TABLE_COLUMN_IMAGE_URL,<<"
					) VALUES (
					'">>,Username,<<"',
					'">>,Body,<<"',
					 '">>,ImageUrl,<<"'
					)">>
				]),

	case Res of
		{updated, 1} ->
			IdAndTimeRes = ejabberd_odbc:sql_query(Server, 
				[
					<<"SELECT ">>
					,?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<", ">>
					,<<"UNIX_TIMESTAMP(">>,?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP,<<") AS ">>,?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<" ">>
					,<<"FROM ">>
					,?CONFESSIONS_TABLE,<<" ">>
					,<<"WHERE ">>
					,?CONFESSIONS_TABLE_COLUMN_USERNAME,<<"='">>,Username,<<"' ">>
					,<<"ORDER BY ">>
					,?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP,<<" DESC LIMIT 1">>
				]),

			case IdAndTimeRes of
				{
					selected, 
					[
						?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID, 
						?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP
					],
					Result
				} ->
					[[IdBinary,CreatedTimeBinary]] = Result,
					Confession#confession{id = IdBinary, created_timestamp = CreatedTimeBinary};
				
				_->
					?INFO_MSG("Failed to retrieve confession after insert success: ~p", [IdAndTimeRes]),
					error				
			end;
		_-> 
			?INFO_MSG("An error occurred inserting a confession on db: ~p", [Res]),
			error
	end.


remove_confession(Server, Username, ConfessionId) ->

	%% I want to pass the Username here even though it's not necessary so
	%% we can ensure the user requesting the deletion is the user who owns the confession.

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"DELETE FROM ">>,
                                        ?CONFESSIONS_TABLE,<<" ">>,
                                        <<"WHERE ">>,
                                        ?CONFESSIONS_TABLE_COLUMN_USERNAME, <<"='">>,Username,<<"' ">>,
                                        <<"AND ">>,
                                        ?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"'">>
                                ] ),

        case Res of
                {updated, 1} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to delete confession: ~p", Res),
                        error
        end.



-spec get_confession(binary(), binary()) -> 'undefined'| #confession{} | error.

get_confession(Server, ConfessionId) ->

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"SELECT ">>,
					?CONFESSIONS_TABLE_COLUMN_USERNAME, <<", ">>,
					?CONFESSIONS_TABLE_COLUMN_BODY, <<", ">>,
					?CONFESSIONS_TABLE_COLUMN_IMAGE_URL, <<", ">>,
					<<"UNIX_TIMESTAMP(">>, ?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<") AS ">>, ?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP, <<" ">>,

					<<"FROM ">>,
                                        ?CONFESSIONS_TABLE,<<" ">>,

                                        <<"WHERE ">>,
					?CONFESSIONS_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"'">>
                                ]),

        case Res of
                {selected, _, []} ->
                        'undefined';

                {selected, 
			[
				?CONFESSIONS_TABLE_COLUMN_USERNAME,
                                ?CONFESSIONS_TABLE_COLUMN_BODY,
                                ?CONFESSIONS_TABLE_COLUMN_IMAGE_URL,
                                ?CONFESSIONS_TABLE_COLUMN_CREATED_TIMESTAMP
			], Result} ->
				[[Username, Body, ImageUrl, CreatedTimestamp]] = Result,
                        	#confession{ 
					id = ConfessionId,
					username = Username,
					body = Body,
					image_url = ImageUrl,
					created_timestamp = CreatedTimestamp
				};
		_->
                        ?INFO_MSG("Unable to get confession: ~p", [Res]),
                        error
        end.


%%%------------------------
%%% Confession Favorite Queries
%%%------------------------

insert_confession_favorite(Server, Username, ConfessionId)->
	
	Res = ejabberd_odbc:sql_query(Server, 
				[
					<<"INSERT INTO ">>,
					?CONFESSION_FAVORITES_TABLE,<<" ">>,
					<<"(">>,?CONFESSION_FAVORITES_TABLE_COLUMN_USERNAME,<<",">>,
						?CONFESSION_FAVORITES_TABLE_COLUMN_CONFESSION_ID,<<") ">>,
					<<"VALUES ('">>,Username,<<"','">>,ConfessionId,<<"')">>
				] ),
	
	case Res of
		{updated, 1} ->
			ok;
		_->
			?INFO_MSG("Unable to favorite confession: ~p", Res),
			error
	end.

remove_confession_favorite(Server, Username, ConfessionId)->
	
	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"DELETE FROM ">>,
                                        ?CONFESSION_FAVORITES_TABLE,<<" ">>,
                                	<<"WHERE ">>,
                                        ?CONFESSION_FAVORITES_TABLE_COLUMN_USERNAME, <<"='">>,Username,<<"' ">>,
                                        <<"AND ">>,
                                        ?CONFESSION_FAVORITES_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"'">>
				] ),

        case Res of
                {updated, 1} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to delete favorite: ~p", Res),
                        error
        end.

is_confession_favorited(Server, Username, ConfessionId)->
	
	Res = ejabberd_odbc:sql_query(Server,
				[
					<<"SELECT ">>,?CONFESSION_FAVORITES_TABLE_COLUMN_USERNAME,<<" FROM ">>,
					?CONFESSION_FAVORITES_TABLE,<<" ">>,
					<<"WHERE ">>,
					?CONFESSION_FAVORITES_TABLE_COLUMN_USERNAME, <<"='">>,Username,<<"' ">>,
					<<"AND ">>,
					?CONFESSION_FAVORITES_TABLE_COLUMN_CONFESSION_ID,<<"='">>,ConfessionId,<<"'">>
				]),

	case Res of
		{selected, [?CONFESSION_FAVORITES_TABLE_COLUMN_USERNAME], Result} ->
			(length(Result) > 0);
		_->
			?INFO_MSG("Unable to check if confession favorite exists: ~p", [Res]),
			error
	end.

%%%------------------------
%%% Push Notification Queries
%%%------------------------

-spec get_seconds_since_last_push_notification(binary(), binary()) -> non_neg_integer() | none.

get_seconds_since_last_push_notification(Server, Username) ->

	Res = ejabberd_odbc:sql_query(Server, 
				[
					<<"SELECT ">>,
					<<"TIME_TO_SEC(TIMEDIFF(NOW(), ">>,?LAST_PUSH_NOTIFS_TABLE_COLUMN_TIMESTAMP,<<")) AS time_diff ">>,
			
					<<"FROM ">>,
					?LAST_PUSH_NOTIFS_TABLE, <<" ">>,
			
					<<"WHERE ">>,
					?LAST_PUSH_NOTIFS_TABLE_COLUMN_USERNAME,<<"='">>,Username,<<"'">>
				]),

	case Res of
		{selected, [<<"time_diff">>], [[TimeDiff]]} -> binary_to_integer(TimeDiff);
		{selected, [<<"time_diff">>], []} -> none;
		_-> error
	end.

insert_last_push_notification_timestamp(Server, Username) ->

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"INSERT INTO ">>,
                                        ?LAST_PUSH_NOTIFS_TABLE,<<" ">>,
                                        <<"(">>,?LAST_PUSH_NOTIFS_TABLE_COLUMN_USERNAME,<<") ">>,
                                        <<"VALUES ('">>,Username,<<"')">>
                                ] ),

        case Res of
                {updated, 1} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to insert username into last_push_notifs table: ~p", Res),
                        error
        end.

update_last_push_notification_timestamp(Server, Username) ->

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"UPDATE ">>,
                                        ?LAST_PUSH_NOTIFS_TABLE,<<" ">>,
					
					<<"SET ">>,
					?LAST_PUSH_NOTIFS_TABLE_COLUMN_TIMESTAMP, <<"=NOW() ">>,
                                	
					<<"WHERE ">>,
					?LAST_PUSH_NOTIFS_TABLE_COLUMN_USERNAME,<<"='">>,Username,<<"'">>
				] ),

        case Res of
                {updated, 1} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to update username in last_push_notifs table: ~p", Res),
                        error
        end.

%%%------------------------
%%% Device Token Queries
%%%------------------------

-type device_token() :: binary().
-type device_type() :: binary().
-type device_info() :: {device_token(), device_type()}.

-spec get_device_info(binary(), binary()) -> device_info() | 'undefined' | error.

get_device_info(Server, Username) ->

	Res = ejabberd_odbc:sql_query(Server,
                                [
					<<"SELECT ">>,
					?DEVICE_TOKENS_TABLE_COLUMN_TOKEN, <<", ">>,
					?DEVICE_TOKENS_TABLE_COLUMN_TYPE, <<" ">>,
					
					<<"FROM ">>,
					?DEVICE_TOKENS_TABLE, <<" ">>,
		
					<<"WHERE ">>,
					?DEVICE_TOKENS_TABLE_COLUMN_USERNAME,<<"='">>,Username,<<"'">>
                                ]),

        case Res of
		{selected, _, []} ->
                        'undefined';
                {selected,
                        [
				?DEVICE_TOKENS_TABLE_COLUMN_TOKEN,
				?DEVICE_TOKENS_TABLE_COLUMN_TYPE
                        ], Result} ->
                                [[DeviceToken, DeviceType]] = Result,
                		{DeviceToken, DeviceType};
                _->
                        ?INFO_MSG("Unable to get device information: ~p", [Res]),
                        error
        end.


%%%------------------------
%%% Participants Queries
%%%------------------------

get_active_participants(Server, MyUsername, ChatId) ->

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"SELECT ">>,
					?PARTICIPANTS_TABLE_COLUMN_USERNAME, <<" ">>,

					<<"FROM ">>,
                                	?PARTICIPANTS_TABLE, <<" ">>,

					<<"WHERE ">>,
					?PARTICIPANTS_TABLE_COLUMN_CHAT_ID, 
						<<"=(SELECT ">>,?CHAT_TABLE_COLUMN_ID,<<" FROM ">>,?CHAT_TABLE,<<" WHERE ">>,?CHAT_TABLE_COLUMN_UUID,<<"='">>,ChatId,<<"') ">>,

					<<"AND ">>,
					?PARTICIPANTS_TABLE_COLUMN_STATUS, <<"='active' ">>,

					<<"AND ">>,
					?PARTICIPANTS_TABLE_COLUMN_USERNAME, <<"!='">>,MyUsername,<<"'">>
				]),

        case Res of
                {selected,
                        [
                        	?PARTICIPANTS_TABLE_COLUMN_USERNAME
			], Result} ->
				%% [[<<"a">>],[<<"b">>]] --> [<<"a">>, <<"b">>]
                                lists:map(fun(El) -> [Username] = El, Username end, Result);
                {selected, _, []} ->
                        'undefined';
                _->
                        ?INFO_MSG("Unable to get device information: ~p", [Res]),
                        error
        end.	


%%%------------------------
%%% Device Version Queries
%%%------------------------

-spec get_device_version(binary(), binary()) -> binary().

get_device_version(Server, Username)->

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"SELECT ">>,
					?DEVICE_VERSION_TABLE_COLUMN_VERSION, <<" ">>,

					<<"FROM ">>,
                                	?DEVICE_VERSION_TABLE, <<" ">>,

					<<"WHERE ">>,
					?DEVICE_VERSION_TABLE_COLUMN_USERNAME, <<"='">>,Username,<<"' ">>
				]),

        case Res of
		{selected, _, []} ->
                        'undefined';
                {selected,
                        [
                        	?DEVICE_VERSION_TABLE_COLUMN_VERSION
			], Result} ->
				[[Version]] = Result,
				Version;
                _->
                        ?INFO_MSG("Unable to get device version: ~p", [Res]),
                        error
        end.


-spec insert_device_version(binary(), binary(), binary()) -> ok | error.

insert_device_version(Server, Username, Version)->

        Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"INSERT INTO ">>,
					?DEVICE_VERSION_TABLE, <<" ">>,

					<<"(">>,
					?DEVICE_VERSION_TABLE_COLUMN_USERNAME,<<", ">>,
					?DEVICE_VERSION_TABLE_COLUMN_VERSION,
					<<") ">>,

					<<"VALUES ">>,
					<<"(">>,
                                        <<"'">>,Username,<<"', ">>,
                                        <<"'">>,Version,<<"'">>,
                                        <<") ">>
                                ]),

        case Res of
                {updated, _Id} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to insert device version: ~p", [Res]),
                        error
        end.


-spec update_device_version(binary(), binary(), binary()) -> ok | error.

update_device_version(Server, Username, Version)->

        Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"UPDATE ">>,
                                        ?DEVICE_VERSION_TABLE, <<" ">>,

					<<"SET ">>,
                                        ?DEVICE_VERSION_TABLE_COLUMN_VERSION, 
					<<"='">>,Version,<<"' ">>,

					<<"WHERE ">>,
					?DEVICE_VERSION_TABLE_COLUMN_USERNAME,
					<<"='">>,Username,<<"'">>

                                ]),

        case Res of
                {updated, _Id} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to update device version: ~p", [Res]),
                        error
        end.


-spec set_device_version(binary(), binary(), binary()) -> ok | error.

set_device_version(Server, Username, Version)->

	Res = get_device_version(Server, Username),

	case Res of
		error ->
			error;
		undefined ->
			insert_device_version(Server, Username, Version);
		_ ->
			update_device_version(Server, Username, Version)	
	end.


%%%------------------------
%%% Device Version Queries
%%%------------------------

-spec get_session_key(binary(), binary()) -> binary().

get_session_key(Server, Username)->

        Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"SELECT ">>,
                                        ?SESSION_TABLE_COLUMN_SESSION_KEY, <<" ">>,

                                        <<"FROM ">>,
                                        ?SESSION_TABLE, <<" ">>,

                                        <<"WHERE ">>,
                                        ?SESSION_TABLE_COLUMN_USERNAME, <<"='">>,Username,<<"' ">>
                                ]),

        case Res of
                {selected, _, []} ->
                        'undefined';
                {selected,
                        [
                                ?SESSION_TABLE_COLUMN_SESSION_KEY
                        ], Result} ->
                                [[Key]] = Result,
                                Key;
                _->
                        ?INFO_MSG("Unable to get session: ~p", [Res]),
                        error
        end.


-spec insert_session_key(binary(), binary()) -> ok | error.

insert_session_key(Server, Username)->

	SessionKey = base64:encode(crypto:strong_rand_bytes(10)),

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"INSERT INTO ">>,
                                        ?SESSION_TABLE, <<" ">>,

                                        <<"(">>,
                                        ?SESSION_TABLE_COLUMN_USERNAME,<<", ">>,
                                        ?SESSION_TABLE_COLUMN_SESSION_KEY,
                                        <<") ">>,

                                        <<"VALUES ">>,
                                        <<"(">>,
                                        <<"'">>,Username,<<"', ">>,
                                        <<"'">>,SessionKey,<<"'">>,
                                        <<") ">>
                                ]),

        case Res of
                {updated, _Id} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to insert session key: ~p", [Res]),
                        error
	        

end.


update_session_key(Server, Username)->

        Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"UPDATE ">>,
                                        ?SESSION_TABLE, <<" ">>,

                                        <<"SET ">>,
                                        ?SESSION_TABLE_COLUMN_SESSION_KEY,
                                        <<"='">>,base64:encode(crypto:strong_rand_bytes(10)),<<"' ">>,

                                        <<"WHERE ">>,
                                        ?SESSION_TABLE_COLUMN_USERNAME,
                                        <<"='">>,Username,<<"'">>

                                ]),

        case Res of
                {updated, _Id} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to session key: ~p", [Res]),
                        error
        end.


set_session_key(Server, Username)->

        Res = get_session_key(Server, Username),

        case Res of
                error ->
                        error;
                undefined ->
                        insert_session_key(Server, Username);
                _ ->
                        update_session_key(Server, Username)
        end.



-spec delete_session_key(binary(), binary()) -> ok.

delete_session_key(Server, Username)->

	Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"DELETE FROM ">>,
					?SESSION_TABLE, <<" ">>,

					<<"WHERE ">>,
					?SESSION_TABLE_COLUMN_USERNAME, <<"='">>,Username,<<"'">>                              
                                ]),

ok.



%%%------------------------
%%% Messages Queries
%%%------------------------

insert_message(Server, SenderUsername, ChatUUID, Body, ImageUrl)->

        Res = ejabberd_odbc:sql_query(Server,
                                [
                                        <<"INSERT INTO ">>,
                                        ?MESSAGES_TABLE, <<" ">>,

                                        <<"(">>,
                                        ?MESSAGES_TABLE_COLUMN_CHAT_UUID,<<", ">>,
                                        ?MESSAGES_TABLE_COLUMN_BODY, <<", ">>,
					?MESSAGES_TABLE_COLUMN_IMAGE_URL, <<", ">>,
					?MESSAGES_TABLE_COLUMN_SENDER_USERNAME,
                                        <<") ">>,

                                        <<"VALUES ">>,
                                        <<"(">>,
                                        <<"'">>,ChatUUID,<<"', ">>,
                                        <<"'">>,Body,<<"', ">>,
                                        <<"'">>,ImageUrl,<<"', ">>,
                                        <<"'">>,SenderUsername,<<"'">>,
                                        <<") ">>
                                ]),

        case Res of
                {updated, _Id} ->
                        ok;
                _->
                        ?INFO_MSG("Unable to insert message: ~p", [Res]),
                        error


end.
