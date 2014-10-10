-module(odbc_queries_wrapper).

-export([query/2, testQuery/0]).

testQuery()->
	io:fwrite(query("?='?'", [{"name", "Will"}])).

query(BinarySqlQuery, SelectionArgs) ->

	SqlQuery = binary_to_list(BinarySqlQuery),

	

	FlattenSelectionArgs = case length(SelectionArgs) of
		0 -> [];
		_ -> 
			[Flattened] = lists:map(fun({Key, Val})-> [binary_to_list(Key), binary_to_list(Val)] end, SelectionArgs),
			Flattened
	end,

	Query = case [{SqlQuery, count(SqlQuery, "?", 0)},{FlattenSelectionArgs, length(FlattenSelectionArgs)}] of
		[{_,MarkerCount},{_, ListCount}]  when not (MarkerCount == ListCount) -> invalid_marker_count;
		[{"",_}, _] -> "";
		_->
			buildQuery(SqlQuery, FlattenSelectionArgs)
	end,

	list_to_binary(Query).

buildQuery(Query, SelectionArgs) ->
        case SelectionArgs of
		[] -> "";
                [H] -> string:sub_string(Query, 1, string:str(Query, "?")-1) ++ H ++ string:sub_string(Query, string:str(Query, "?")+1, length(Query));
                [H|Tail]->
                        string:sub_string(Query, 1, string:str(Query, "?")-1) ++  H ++ buildQuery(string:sub_string(Query, string:str(Query, "?")+1, length(Query)), Tail)
        end.

count(Str, Char, Count)->
        case {length(Str), string:str(Str, Char)} of
                {0,_} -> Count;
                {_, 0} -> Count;
                {Length,Pos} when Length > 0 ->
                        count(string:sub_string(Str, Pos+1, length(Str)), Char, Count) + 1
        end.
