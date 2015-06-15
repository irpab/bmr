% <p>Set of examples that will show capabilities of bmr_pt and bmr modules.
% compile and run test/0 with and without 'bmr' flag and compare results.</p>
-module(bmr_example).

-ifdef(bmr).
-compile({parse_transform, bmr_pt}).
-define(EXCEPTION_TYPE, bmr).
-else.
-define(EXCEPTION_TYPE, usual).
-endif.

-compile(export_all).

-record(rec, {a, b, c, d, e, f, g, k, l}).

-record(rec2, {x, y, z}).

-record(rec3, {s, t, q, w}).


return_ok_record() ->
  {ok, return_record()}.

return_record() ->
  #rec{
    a = 1,
    b = 2,
    c = #rec2{
      x = 11,
      y = #rec3{
        s = 111,
        t = {112, a},
        q = b,
        w = a
      },
      z = 12
    },
    d = 3,
    e = a,
    f = b,
    g = a,
    k = k1,
    l = l1
  }.

return_record_2() ->
  #rec{
    a = 1,
    b = 2,
    c = #rec2{
      x = 11,
      y = 12
    },
    d = vd,
    e = ve
  }.

test() ->
  Tests = [
      {test_no_exception_1, 'not throw', "shows behaviour when no exception occurs"}
    , {test_no_exception_2, 'not throw', "same as 1 but format is {ok, #rec{}} = ..."}
    , {test_no_exception_3, 'not throw', "same as 1 but format is {ok, R = #rec{}} = ..."}
    , {test_exception_1   , 'throw'    , "shows detection of mismatch in fields"}
    , {test_exception_2   , 'throw'    , "same as 1 but format is {ok, #rec{}} = ..."}
    , {test_exception_3   , 'throw'    , "same as 1 but format is {ok, R = #rec{}} = ..."}
    , {test_exception_5   , 'throw'    , "shows detection of root record mismatch"}
    , {test_exception_6   , 'throw'    , "shows detection of nested records mismatch"}
  ],
  [exec_test(T) || T <- Tests],
  ok.

exec_test({Name, ShouldThrow, CaseDoc}) ->
  io:format("starting ~w: it should ~w an exception (~w) (~p)~n",
  	[Name, ShouldThrow, ?EXCEPTION_TYPE, CaseDoc]),
  R = try
  	apply(?MODULE, Name, []),
  	no_exception
  catch E1:E2 ->
  	{exception, E1, E2}
  end,
  io:format("end ~w: result ~p~n~n~n~n", [Name, R]).

% shows behaviour when no exception occurs
test_no_exception_1() ->
  VA = a,
  V3 = 3,
  #rec{
    a = 1,
    c = #rec2{
      x = 11,
      y = R3 = #rec3{ % nested records could be putted into var
        s = NVs,
        t = {112, a},
        w = NVw
      },
      z = 12
    },
    d = V3,
    e = VA,
    f = b
  } = return_record(),
  NVs = 111,
  a = NVw,
  R3 = #rec3{s = 111, t = {112, a}, q = b, w = a},
  ok.

% same as 1 but format is {ok, #rec{}} = ...
test_no_exception_2() ->
  VA = a,
  V3 = 3,
  {ok, #rec{
    a = 1,
    c = #rec2{
      x = 11,
      y = R3 = #rec3{ % nested records could be putted into var
        s = NVs,
        t = {112, a},
        w = NVw
      },
      z = 12
    },
    d = V3,
    e = VA,
    f = b
  }} = return_ok_record(),
  NVs = 111,
  NVw = a,
  R3 = #rec3{s = 111, t = {112, a}, q = b, w = a},
  ok.

% same as 1 but format is {ok, R = #rec{}} = ...
test_no_exception_3() ->
  VA = a,
  V3 = 3,
  {ok, _R = #rec{
    a = 1,
    c = #rec2{
      x = 11,
      y = R3 = #rec3{ % nested records could be putted into var
        s = NVs,
        t = {112, a},
        w = NVw
      },
      z = 12
    },
    d = V3,
    e = VA,
    f = b
  }} = return_ok_record(),
  NVs = 111,
  NVw = a,
  R3 = #rec3{s = 111, t = {112, a}, q = b, w = a},
  ok.

% shows:
% wrong value (with value, with variable, inside nested records)
% detect same new variable wrong values (inside nested records)
% working with new variables (1+)
test_exception_1() ->
  Vb = bbb,
  Vq = b,
  #rec{
    a = 11, % wrong value
    b = Vb, % wrong value with variable
    c = #rec2{
      x = X,
      y = R3 = #rec3{ % nested records could be putted into var
        % s = 111,
        t = {112, b}, % wrong value inside nested records
        q = Vq,
        w = Vdfw % detect same new variable wrong values (inside nested records)
      },
      z = 12
    },
    d = Vdfw, % detect same new variable wrong values
    e = Veg, % working with new variables 1+
    f = Vdfw, % detect same new variable wrong values
    g = Veg, % working with new variables 1+
    k = Vkl,
    l = Vkl
  } = return_record(),
  X = 11,
  ok.

% same as 1 but format is {ok, #rec{}} = ...
test_exception_2() ->
  Vb = bbb,
  Vq = b,
  {ok, #rec{
    a = 11, % wrong value
    b = Vb, % wrong value with variable
    c = #rec2{
      x = X,
      y = R3 = #rec3{ % nested records could be putted into var
        % s = 111,
        t = {112, b}, % wrong value inside nested records
        q = Vq,
        w = Vdfw % detect same new variable wrong values (inside nested records)
      },
      z = 12
    },
    d = Vdfw, % detect same new variable wrong values
    e = Veg, % working with new variables 1+
    f = Vdfw, % detect same new variable wrong values
    g = Veg, % working with new variables 1+
    k = Vkl,
    l = Vkl
  }} = return_ok_record(),
  X = 11,
  ok.

% same as 1 but format is {ok, R = #rec{}} = ...
test_exception_3() ->
  Vb = bbb,
  Vq = b,
  {ok, _R = #rec{
    a = 11, % wrong value
    b = Vb, % wrong value with variable
    c = #rec2{
      x = X,
      y = R3 = #rec3{ % nested records could be putted into var
        % s = 111,
        t = {112, b}, % wrong value inside nested records
        q = Vq,
        w = Vdfw % detect same new variable wrong values (inside nested records)
      },
      z = 12
    },
    d = Vdfw, % detect same new variable wrong values
    e = Veg, % working with new variables 1+
    f = Vdfw, % detect same new variable wrong values
    g = Veg, % working with new variables 1+
    k = Vkl,
    l = Vkl
  }} = return_ok_record(),
  X = 11,
  ok.

% shows detection of root record mismatch
test_exception_5() ->
  #rec3{ % wrong root record
    s = 11,
    t = 12
  } = return_record_2(),
  ok.

% shows detection of nested records mismatch
test_exception_6() ->
  #rec{
    a = #rec2{}, % wrong nested record
    b = 22,
    c = #rec3{ % wrong nested record
      s = A % this will be ignored
    },
    d = A,
    e = A
  } = return_record_2(),
  ok.

test_exception_01() ->
  #rec{
    b = A % ok, new var for the whole field
    % b = {A} % new var not for the whole field will trigger compile error
  } = return_record_2(),
  % {ok, #rec{} = R} = return_ok_record(), % compile error since bmr_pt
                                           % will replace #rec{} = R with
                                           % inappropriate code
  1 = A,
  ok.

% this is test that verifies that parse transform can process
% variables in function pattern, case clause
test_var_in_clause(X) ->
  Z = 4,
  case {a1, l} of
    a2 ->
      #rec{
        a = X
      } = #rec{ a = {45} };
    {a1, D} ->
      C = 9,
      #rec{
        a = X
       ,b = Z
       ,c = C
       ,d = D
      } = #rec{ a = 1, b = 4, c = 7, d = 17 }
  end,

  ok.
