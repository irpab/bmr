% <p>parse transform for match expressions with records in left side like
% #record{ field_1 = value_1 } = something_that_return_record() that will wrap
% this match_expr with special code that will work in case of badmatch exception
% and will indicate which exact fields were not matched.</p>
% <p>example of initial code and result of parse_transform:
% A = 5,
% #record{
%   a = A,
%   b = B,
%   c = C,
%   d = 7
% } = return_record_fun()
% --->
% begin
%   {R, [B, C]} = try
%     _tmp_R = return_record_fun(),
%     #record{
%       a = A,
%       b = _tmp_B,
%       c = _tmp_C,
%       d = 7
%     } = _tmp_R,
%     {_tmp_R, [_tmp_B, _tmp_C]}
%   catch
%     error:{badmatch, _tmp_R} ->
%       bmr:bmr(...), % all needed information about record
%       erlang:error({badmatch, _tmp_R})
%   end,
%   R
% end
% </p>

-module(bmr_pt).

-author("Pavel Baturko (pabftk@gmail.com)").

-export([parse_transform/2]).

-define(DBG(Str, Args), ?DBGN(Str ++ "~n", Args)).
-define(DBG(Str), ?DBG(Str, [])).
-define(DBGN(Str, Args), io:format(Str, Args)).
-define(DBGN(Str), ?DBGN(Str, [])).

-record(pt_state, {
  try_var_cnt = 0  % counter that will be used in auto-generated variables names
                   % to avoid names duplications
, used_vars = []   % list of variables that were defined before currently being
                   % processed fun body subtree
, used_vars_0 = [] % additional temporary list that used only inside fun body subtrees
, file_name = unknown_file % filename of file under our parse_transform. used
                           % in exception to indicate file / line number with failed match_expr
}).

parse_transform(Forms, _Options) ->
  Tree = erl_syntax:form_list(Forms),
  Modified_tree = proc_app_tree(Tree, #pt_state{}),
  Modified_forms = erl_syntax:revert_forms(Modified_tree),
  Modified_forms.

proc_app_tree(App_tree, Pt_state) ->
  [App_tree1] = erl_syntax:subtrees(App_tree),
  File_name = extract_attr_file_name(App_tree1),
  Pt_state1 = Pt_state#pt_state{ file_name = File_name },

  % find all function clauses and process them separately
  Proc_function_fun = fun(Tree) ->
      proc_function(Tree, Pt_state1)
    end,
  erl_syntax:make_tree(form_list, [lists:map(Proc_function_fun, App_tree1)]).

extract_attr_file_name([Tree | Rest_tree]) ->
  case erl_syntax:type(Tree) of
    attribute ->
      case erl_syntax:atom_value(erl_syntax:attribute_name(Tree)) of
        file ->
          erl_syntax:string_value(hd(erl_syntax:attribute_arguments(Tree)));
        _ ->
          extract_attr_file_name(Rest_tree)
      end;
    _ ->
      extract_attr_file_name(Rest_tree)
  end.

proc_function(Tree, Pt_state) ->
  case erl_syntax:type(Tree) of
    function ->
      Fun_clauses = erl_syntax:function_clauses(Tree),
      Proc_fun_clause_fun = fun(Fun_clause) ->
          proc_fun_clause(Fun_clause, Pt_state)
        end,
      New_fun_clauses = lists:map(Proc_fun_clause_fun, Fun_clauses),
      New_function_tree = erl_syntax:function(erl_syntax:function_name(Tree), New_fun_clauses),
      New_function_tree;
    _ ->
      Tree
  end.

get_vars_from_tree(Tree) ->
  sets:to_list(erl_syntax_lib:variables(Tree)).

% function body of function clause consists of list of subtrees.
% each subtree will be processed separately.
% for each subtree all variables from previous subtrees will be "used"
proc_fun_clause(Tree, Pt_state) ->
  Clause_body = erl_syntax:clause_body(Tree),
  Vars_from_fun_clause0 = [get_vars_from_tree(Clause_pattern) || Clause_pattern <- erl_syntax:clause_patterns(Tree)],
  Vars_from_fun_clause = lists:flatten(Vars_from_fun_clause0),
  {New_clause_body, _} = lists:mapfoldl(fun(Sub_tree1, Pt_state1) ->
      {Sub_tree2, Pt_state2} = proc_sub_trees(Sub_tree1, Pt_state1),
      Vars_from_sub_tree = get_vars_from_tree(Sub_tree1),
      New_used_vars = lists:usort(Pt_state2#pt_state.used_vars ++ Vars_from_sub_tree),
      Pt_state3 = Pt_state2#pt_state{
        used_vars = New_used_vars,
        used_vars_0 = New_used_vars
      },
      {Sub_tree2, Pt_state3}
    end,
    Pt_state#pt_state{
      used_vars = Vars_from_fun_clause
    , used_vars_0 = Vars_from_fun_clause
    },
    Clause_body),
  erl_syntax:clause(erl_syntax:clause_patterns(Tree), erl_syntax:clause_guard(Tree), New_clause_body).

proc_sub_trees(Tree, Pt_state) ->
  proc_sub_tree(Tree, Pt_state).

% recursive fun for processing subtrees.
% process childs first, extract new variables but do not use them for
% parent node processing since they are "new" for parent node.
proc_sub_tree(Tree, Pt_state = #pt_state{ used_vars_0 = Used_vars_0 }) ->
  {New_tree, New_pt_state} = erl_syntax_lib:mapfold_subtrees(
    fun(Sub_tree1, Pt_state1) ->
      {Sub_tree2, Pt_state2} = proc_sub_tree(Sub_tree1, Pt_state1),
      New_var =
      case erl_syntax:type(Sub_tree2) of
        variable -> [erl_syntax:variable_name(Sub_tree2)];
        _        -> []
      end,
      Pt_state3 = Pt_state2#pt_state{
        used_vars_0 = lists:usort(Pt_state2#pt_state.used_vars_0 ++ New_var)
      },
      {Sub_tree2, Pt_state3}
    end, Pt_state, Tree),

  % call proc_node that will actually do our parse_transform
  {New_tree2, New_pt_state2} = proc_node(New_tree, New_pt_state#pt_state{
      used_vars_0 = Used_vars_0
    }),
  Used_vars_after_subtrees = New_pt_state#pt_state.used_vars_0,
  {New_tree2, New_pt_state2#pt_state{ used_vars_0 = Used_vars_after_subtrees }}.

proc_node(N, Pt_state) ->
  proc_node(N, Pt_state, erl_syntax:type(N)).

proc_node(N, Pt_state, match_expr) ->
  proc_match_expr(N, Pt_state);
proc_node(N, Pt_state, _Type) ->
  {N, Pt_state}.

proc_match_expr(N, Pt_state) ->
  P = erl_syntax:match_expr_pattern(N),
  B = erl_syntax:match_expr_body(N),
  PT = erl_syntax:type(P),
  BT = erl_syntax:type(B),
  proc_match_expr(N, Pt_state, PT, P, BT, B).

% some kind of Maybe Monad?
% macroses for simplified version of code:
% case P1 of
%   C1 ->
%     Expr1,
%     Expr2,
%     case P2 of
%       C2 ->
%         case P3 of
%           C3 ->
%             Expr3,
%             R1;
%           C4 ->
%             R2;
%           _ ->
%             Rdef
%         end;
%       _ ->
%         Rdef
%     end;
%   _ ->
%     Rdef
% end
% this can be represented as
% ?M([
%   ?F(begin
%     C1 = P1,
%     Expr1,
%     Expr2,
%     ?M([
%       ?F(begin
%         C2 = P2,
%         ?M([
%           ?F(begin
%             C3 = P3,
%             Expr3,
%             R1
%           end),
%           ?F(begin
%             C4 = P3,
%             R2
%           end)
%         ], Rdef)
%       end)
%     ], Rdef)
%   end)
% ], Rdef)

-define(F(A), fun() -> A end).
-define(M(L, Def_val), case_iter(L, Def_val)).

case_iter([], Def_val) ->
  Def_val;
case_iter([F | T], Def_val) ->
  try
    F()
  catch _:_ ->
    case_iter(T, Def_val)
  end.

% parse_transform applicable to forms of match_expr:
% #record{} = ...
% {ok, #record{}} = ...
% {ok, Var = #record{}} = ...
proc_match_expr(N, Pt_state, PT, P, _BT, B) ->
  ?M([
    ?F(begin
      record_expr = PT,
      proc_bmr_expr(rec, N, Pt_state, P, B)
    end),
    ?F(begin
      tuple = PT,
      2 = erl_syntax:tuple_size(P),
      [E1, E2] = erl_syntax:tuple_elements(P),
      [E1Type, E2Type] = [erl_syntax:type(E) || E <- [E1, E2]],
      atom = E1Type,
      ok = erl_syntax:atom_value(E1),
      ?M([
        ?F(begin
          record_expr = E2Type,
          proc_bmr_expr(ok_rec, N, Pt_state, P, B)
        end),
        ?F(begin
          match_expr  = E2Type, E2P = erl_syntax:match_expr_pattern(E2),
          E2B = erl_syntax:match_expr_body(E2),
          E2PT = erl_syntax:type(E2P),
          E2BT = erl_syntax:type(E2B),
          {variable, record_expr} = {E2PT, E2BT},
          proc_bmr_expr(ok_var_rec, N, Pt_state, P, B)
         end)
      ], {N, Pt_state})
    end)
  ], {N, Pt_state}).

proc_bmr_expr(Node_type, Node, Pt_state, Left_side_expr0, Right_side_expr0) ->
  Var_cnt = Pt_state#pt_state.try_var_cnt,
  % transform forms
  %  {ok, #rec{...}} = ... and {ok, R = #rec{...}} = ...
  % to
  %  {ok, R} = ...,
  %  #rec{...} = R
  {Left_side_expr, Right_side_expr, Pre_action} =
  case Node_type of
    rec ->
      {Left_side_expr0, Right_side_expr0, erl_syntax:atom(do_nothing)};
    Node_type when Node_type == ok_rec; Node_type == ok_var_rec ->
      [Left_side_expr01, Left_side_expr1] = erl_syntax:tuple_elements(Left_side_expr0),
      {Left_side_expr2, Right_side_expr2} =
      case Node_type of
        ok_rec ->
          {Left_side_expr1, erl_syntax:variable(create_cnt_var_name("_Ok_Rec_Var_", Var_cnt))};
        ok_var_rec ->
          {erl_syntax:match_expr_body(Left_side_expr1),
           erl_syntax:match_expr_pattern(Left_side_expr1)}
      end,
      Pre_action1 = erl_syntax:match_expr(
        erl_syntax:tuple([Left_side_expr01, Right_side_expr2]),
        Right_side_expr0),
      {Left_side_expr2, Right_side_expr2, Pre_action1}
  end,

  none = erl_syntax:record_expr_argument(Left_side_expr), % only this form is supported
  Rec_type = erl_syntax:record_expr_type(Left_side_expr),
  Rec_fields = erl_syntax:record_expr_fields(Left_side_expr),
  Line_num = erl_syntax:get_pos(Node),

  % replace all variables introduced in record with temporary
  % vars. this is needed because compiler restricts defining
  % new vars inside try block that are used outside of it.
  % so this method will be used: new tmp vars will be defined
  % inside try block and returned from try to new vars that
  % were defined by user:
  % was: #record{ field = Var1 }
  % new: [Var1, Var2] = try #record{ f1 = _tmp_Var1, f2 = _tmp_Var2 }, [_tmp_Var1, _tmp_Var2] end
  {New_vars_list, Values, Fields_new_vars_replaced}
    = collect_and_update_new_vars_fields(Rec_type, Rec_fields, Pt_state),
  Values_AST = create_values_ast(Values),
  New_vars_list_AST = create_new_vars_ast(Rec_type, New_vars_list),

  R_side_var_name = create_cnt_var_name("_RSideEx_", Var_cnt),
  R_side_var_AST = erl_syntax:variable(R_side_var_name),

  % this is full information that is needed to format message
  % that will indicate what exact fields not matched in case
  % of badmatch exception
  Bmr_args = [
    erl_syntax:record_expr(none, Rec_type, []),
    Values_AST,
    New_vars_list_AST,
    R_side_var_AST,
    erl_syntax:abstract({Pt_state#pt_state.file_name, Line_num})
  ],

  L_side_with_new_vars_replaced_AST = erl_syntax:record_expr(
    none,
    Rec_type,
    Fields_new_vars_replaced), 

  New_vars_names_pairs = [
    {erl_syntax:variable(N), 
     erl_syntax:variable(create_new_var_tmp_name(N))
    } || {N, _} <- New_vars_list],
  {New_vars_AST_list, New_vars_replaced_AST_list} = lists:unzip(New_vars_names_pairs),
  New_vars_AST = erl_syntax:list(New_vars_AST_list),
  New_vars_replaced_AST = erl_syntax:list(New_vars_replaced_AST_list),

  L_side_var_name = create_cnt_var_name("_LSideVar_", Var_cnt),
  L_side_var_AST = erl_syntax:variable(L_side_var_name),
  Try_expr_body_AST = [
    erl_syntax:match_expr(L_side_var_AST, Right_side_expr),
    erl_syntax:match_expr(L_side_with_new_vars_replaced_AST, L_side_var_AST),
    erl_syntax:tuple([
      L_side_var_AST,
      New_vars_replaced_AST
    ])
  ],

  Try_expr_catch_pattern_AST = erl_syntax:class_qualifier(
    erl_syntax:atom(error),
    erl_syntax:tuple([erl_syntax:atom(badmatch),R_side_var_AST])
  ),
  Try_expr_catch_body_AST = [
    create_mfa_call_ast(bmr, bmr, Bmr_args),
    create_mfa_call_ast(erlang, error,
      [erl_syntax:tuple([erl_syntax:atom(badmatch),R_side_var_AST])])
  ],
  Try_expr_handler_AST = erl_syntax:clause([Try_expr_catch_pattern_AST], [], Try_expr_catch_body_AST),
  Result_R_side_var_AST = erl_syntax:variable(create_cnt_var_name("_ResRSideV_", Var_cnt)),
  New_node = 
  erl_syntax:block_expr([
    Pre_action,
    erl_syntax:match_expr(
      erl_syntax:tuple([
        Result_R_side_var_AST,
        New_vars_AST
      ]),
      erl_syntax:try_expr(Try_expr_body_AST, [Try_expr_handler_AST])
    ),
    Result_R_side_var_AST % ?
  ]),

  New_pt_state = Pt_state#pt_state{try_var_cnt = Var_cnt + 1},
  {New_node, New_pt_state}.

create_cnt_var_name(Str, Cnt) when is_list(Str), is_integer(Cnt) ->
  list_to_atom(Str ++ integer_to_list(Cnt)).

create_new_var_tmp_name(V) when is_atom(V) ->
  list_to_atom("_Tmp_Var_" ++ atom_to_list(V)).

create_mfa_call_ast(M, F, A) ->
  erl_syntax:application(erl_syntax:module_qualifier(
    erl_syntax:atom(M), erl_syntax:atom(F)),
    A).

collect_and_update_new_vars_fields(Rec_type, Rec_fields, Pt_state) ->
  collect_and_update_new_vars_fields(Rec_type, [], [], [], Rec_fields, Pt_state).

collect_and_update_new_vars_fields(Rec_type, Values, New_vars, New_rec_fields, [], _Pt_state) ->
  RecName = erl_syntax:atom_value(Rec_type),
  {New_vars,
   {'$record_marker', RecName, lists:reverse(Values)},
   lists:reverse(New_rec_fields)};
collect_and_update_new_vars_fields(Rec_type, Values, New_vars, New_rec_fields, [Rec_field | Rec_fields_rest], Pt_state) ->
  Field_val = erl_syntax:record_field_value(Rec_field),
  Field_name = erl_syntax:record_field_name(Rec_field),
  Field_name_atom = erl_syntax:atom_value(Field_name),

  {Extracted_new_vars, New_value, New_rec_field_val} =
  case erl_syntax:type(Field_val) of
    variable ->
      Var_name = erl_syntax:variable_name(Field_val),
      New_tmp_var_name = create_new_var_tmp_name(Var_name),
      Replaced_var_AST = erl_syntax:variable(New_tmp_var_name),
      proc_field_val(Var_name, Pt_state, Field_val, Replaced_var_AST);
    match_expr ->
      Match_L_side = erl_syntax:match_expr_pattern(Field_val),
      Match_R_side = erl_syntax:match_expr_body(Field_val),
      Match_L_side_type = erl_syntax:type(Match_L_side),
      Match_R_side_type = erl_syntax:type(Match_R_side),
      {Var_name, Other_side} =
      if Match_L_side_type == variable ->
           {erl_syntax:variable_name(Match_L_side), Match_R_side};
         Match_R_side_type == variable ->
           {erl_syntax:variable_name(Match_R_side), Match_L_side}
      end,
      New_tmp_var_name = create_new_var_tmp_name(Var_name),
      New_tmp_var_AST = erl_syntax:variable(New_tmp_var_name),

      N_rec_expr_AST =
      if Match_L_side_type == record_expr -> Match_L_side;
         Match_R_side_type == record_expr -> Match_R_side;
         true -> not_record
      end,
      {N_rec_new_vars, New_val0, Mod_other_side} =
      case N_rec_expr_AST of
        not_record -> {[], '$no_value', Other_side};
        _          -> new_vars_from_rec(N_rec_expr_AST, Pt_state)
      end,

      Replaced_var_AST =
      if Match_L_side_type == variable ->
           erl_syntax:match_expr(New_tmp_var_AST, Mod_other_side);
         Match_R_side_type == variable ->
           erl_syntax:match_expr(Mod_other_side, New_tmp_var_AST)
      end,
      {Var_list, New_val1, New_field_val} = proc_field_val(Var_name, Pt_state, Field_val, Replaced_var_AST),
      New_val2 = case New_val1 of
        '$no_value' -> New_val0;
        _           -> '$no_value'
      end,
      {Var_list ++ N_rec_new_vars, New_val2, New_field_val};
    record_expr ->
      new_vars_from_rec(Field_val, Pt_state);
    _ ->
      {[], Field_val, Field_val}
  end,
  New_rec_field = erl_syntax:record_field(Field_name, New_rec_field_val),
  New_vars_upd = lists:foldl(fun
    (Var_name, New_vars_acc) when is_atom(Var_name) ->
      add_new_var(Var_name, New_vars_acc, Field_name_atom);
    ({Var_name, {NR_name, L}}, New_vars_acc) when is_atom(Var_name), is_atom(NR_name), is_list(L) ->
      add_new_var(Var_name, New_vars_acc, {Field_name_atom,NR_name,L})
    end,
    New_vars, Extracted_new_vars),
  New_values =
  case New_value of
    '$no_value' -> Values;
    _            -> [{Field_name_atom, New_value} | Values]
  end,
  collect_and_update_new_vars_fields(Rec_type, New_values, New_vars_upd, [New_rec_field | New_rec_fields], Rec_fields_rest, Pt_state).

add_new_var(Var_name, New_vars_acc, NewElem) ->
  case lists:keyfind(Var_name, 1, New_vars_acc) of
    false ->
      lists:keystore(Var_name, 1, New_vars_acc, {Var_name, [NewElem]}) ;
    {Var_name, Fields} ->
      lists:keyreplace(Var_name, 1, New_vars_acc, {Var_name, [NewElem | Fields]})
  end.

new_vars_from_rec(Field_val, Pt_state) ->
  N_rec_fields = erl_syntax:record_expr_fields(Field_val),
  N_rec_type = erl_syntax:record_expr_type(Field_val),
  {N_new_vars, New_value, N_new_rec_fields} = collect_and_update_new_vars_fields(N_rec_type, N_rec_fields, Pt_state),
  N_rec_name = erl_syntax:atom_value(N_rec_type),
  {[{V,{N_rec_name,L}} || {V,L} <- N_new_vars],
   New_value,
   erl_syntax:record_expr(none, N_rec_type, N_new_rec_fields)}.

proc_field_val(Var_name, Pt_state, Field_val, Replaced_var_AST) ->
  case lists:member(Var_name, Pt_state#pt_state.used_vars_0) of
    true  -> {[]        , Field_val  , Field_val};
    false -> {[Var_name], '$no_value', Replaced_var_AST}
  end.

create_values_ast(Values = {'$record_marker',R,_}) ->
  create_values_ast2(R, Values).

create_values_ast2(_RN, {'$record_marker',R,L}) ->
  erl_syntax:tuple([
    erl_syntax:atom('$record_marker'),
    erl_syntax:record_expr(none, erl_syntax:atom(R), []),
    create_values_ast2(R, L)
  ]);
create_values_ast2(RN, L) when is_list(L) ->
  erl_syntax:list([create_values_ast2(RN, E) || E <- L]);
create_values_ast2(RN, {F,V}) ->
  erl_syntax:tuple([
    erl_syntax:atom(F),
    erl_syntax:record_index_expr(erl_syntax:atom(RN), erl_syntax:atom(F)),
    create_values_ast2(RN, V)
  ]);
create_values_ast2(_RN, AST) ->
  AST.

create_new_vars_ast(Rec_type, New_vars_list) ->
  create_new_vars_ast(Rec_type, [], New_vars_list).

create_new_vars_ast(Rec_type, New_vars_list_AST, []) ->
  erl_syntax:tuple([
    erl_syntax:record_expr(none, Rec_type, []),
    erl_syntax:list(lists:reverse(New_vars_list_AST))
  ]);
create_new_vars_ast(Rec_type, New_vars_list_AST, [{V,L} | T]) ->
  Fields_4_var_AST = create_new_vars_ast2(Rec_type, L),
  New_AST = erl_syntax:tuple([
    erl_syntax:atom(V),
    Fields_4_var_AST
  ]),
  create_new_vars_ast(Rec_type, [New_AST | New_vars_list_AST], T).

create_new_vars_ast2(Rec_type, L) ->
  create_new_vars_ast2(Rec_type, [], L).

create_new_vars_ast2(_RecType, List_AST, []) ->
  erl_syntax:list(lists:reverse(List_AST));
create_new_vars_ast2(Rec_type, List_AST, [F | T]) when is_atom(F) ->
  New_AST = erl_syntax:tuple([
    erl_syntax:atom(F),
    erl_syntax:record_index_expr(Rec_type, erl_syntax:atom(F))
  ]),
  create_new_vars_ast2(Rec_type, [New_AST | List_AST], T);
create_new_vars_ast2(Rec_type, List_AST, [{F,Nrec,L} | T]) when is_atom(F) ->
  New_AST = erl_syntax:tuple([
    erl_syntax:atom(F),
    erl_syntax:record_index_expr(Rec_type, erl_syntax:atom(F)),
    erl_syntax:record_expr(none, erl_syntax:atom(Nrec), []),
    create_new_vars_ast2(erl_syntax:atom(Nrec), L)
  ]),
  create_new_vars_ast2(Rec_type, [New_AST | List_AST], T).
