% <p>this module will be called in case of badmatch exception (bmr = bad record match).
% parse_transform will prepare all information about record in left side of
% match expr (compile time) and provide it to this module with right side (runtime).
% this module will which exact fields were not match between left and right sides
% and format this information in human readable format.</p>
-module(bmr).

-author("Pavel Baturko (pabftk@gmail.com)").

-compile(export_all).

-define(DBG(Str, Args), ?DBGN(Str ++ "~n", Args)).
-define(DBG(Str), ?DBG(Str, [])).

-define(DBGN(Str, Args), io:format(Str, Args)).
-define(DBGN(Str), ?DBGN(Str, [])).

bmr(RecType, Values, RecNewVars, RSide, Line_num) ->
  ?DBG(">>>>>>>>>> bmr called (~p) <<<<<<<<<<~n", [Line_num]),

  case check_records_same_type(RecType, RSide) of
    true ->
      Proc_new_vars = proc_new_vars(RecNewVars, RSide),
      Proc_values = proc_values(Values, RSide),

      print_proc_new_vars(RecType, Proc_new_vars),
      print_proc_values(RecType, Proc_values);
    false ->
      ?DBG("Left side record (~w) is not the same type as Right side (~w)",
        [record_type(RecType), record_type(RSide)])
  end,

  ?DBG("~n<<<<<<<<<<  bmr done  >>>>>>>>>>"),

  erlang:error({bmr, [{location, Line_num}]}),

  ok.

proc_new_vars({_Rec, NewVars}, RSide) ->
  [{V,proc_new_vars2(RSide, L)} || {V,L}  <- NewVars].

proc_new_vars2(RSide, L) ->
  proc_new_vars2(RSide, [], L).

proc_new_vars2(_RSide, L2, []) ->
  lists:reverse(L2);
proc_new_vars2(RSide, L2, [{N,I} | T]) ->
  proc_new_vars2(RSide, [{[N], element(I,RSide)} | L2], T);
proc_new_vars2(RSide, L2, [{N,I,NR,NL} | T]) ->
  IV = element(I,RSide),
  NL3 = 
  case check_records_same_type(NR, IV) of
    true ->
      NL2 = proc_new_vars2(IV, NL),
      [{[N,{'$record_marker',record_type(NR)}] ++ NL2P, NL2V} || {NL2P, NL2V} <- NL2];
    false ->
      []
  end,
  proc_new_vars2(RSide, NL3 ++ L2, T).

print_proc_new_vars(Rec, Proc_new_vars) ->
  Filtered_proc_new_vars =
    lists:filter(fun({_Var, LVals}) ->
      case LVals of
        [] ->
          false;
        _ ->
          {_, LValsHV} = hd(LVals),
          not lists:all(fun({_,V}) -> LValsHV == V end, LVals)
      end
    end, Proc_new_vars),
  case Filtered_proc_new_vars of
    [] -> ok;
    _ ->
      ?DBG("In record '~w' following fields should have the same value:", [record_type(Rec)]),
      [begin
         ?DBG(" for var ~w:", [Var]),
         [?DBG("   ~p: ~p", [path_to_str(Path), Val]) || {Path, Val} <- LVals],
         ?DBG("")
      end || {Var, LVals} <- Filtered_proc_new_vars]
  end.


proc_values({'$record_marker',R,L}, RSide) ->
  case check_records_same_type(R, RSide) of
    true ->
      L1 = proc_values(L, RSide),
      [{P,VL,VR} || {P,VL,VR} <- lists:flatten(L1)];
    false ->
      [{['$nested_record_types_mismatch'], record_type(R), record_type(RSide)}]
  end;
proc_values(L, RSide) when is_list(L) ->
  [proc_values(E, RSide) || E <- L];
proc_values({N,I,{'$record_marker',NR,NL}}, RSide) ->
  IV = element(I,RSide),
  NL2 = proc_values({'$record_marker',NR,NL}, IV),
  [{[N,{'$record_marker',record_type(NR)}] ++ NL2P, VL, VR} || {NL2P, VL, VR} <- NL2];
proc_values({N,I,VL}, RSide) ->
  {[N], VL, element(I, RSide)}.

print_proc_values(Rec, Proc_values) ->
  Filtered_proc_values =
    lists:filter(fun({_Path, VL, VR}) -> VL /= VR end, Proc_values),
  case Filtered_proc_values of
    [] -> ok;
    _ ->
      ?DBG("In record '~w' following fields does not match on left and right:", [record_type(Rec)]),
      [begin
         ?DBG("  ~p: left = ~p vs right = ~p", [path_to_str(Path), VL, VR])
      end || {Path, VL, VR} <- Filtered_proc_values]
  end.


check_records_same_type(Rec1, Rec2) when is_tuple(Rec1), is_tuple(Rec2) ->
  Rec1List = tuple_to_list(Rec1),
  Rec2List = tuple_to_list(Rec2),
  Rec1ListLen = length(Rec1List),
  Rec2ListLen = length(Rec2List),
  (Rec1ListLen == Rec2ListLen) andalso (hd(Rec1List) == hd(Rec2List));
check_records_same_type(_, _) ->
  false.

record_type(Rec) when is_tuple(Rec) ->
  hd(tuple_to_list(Rec));
record_type(Rec) ->
  {'$not_record', Rec}.

path_to_str(Path) ->
  path_to_str("", Path).
path_to_str(Str, []) ->
  Str;
path_to_str(Str, [F|T]) when is_atom(F) ->
  S = lists:flatten(io_lib:format("field '~p'",[F])),
  path_to_str(Str ++ S, T);
path_to_str(Str, [{'$record_marker',R}|T]) when is_atom(R) ->
  S = lists:flatten(io_lib:format(" (record type '~p') ",[R])),
  path_to_str(Str ++ S, T).
