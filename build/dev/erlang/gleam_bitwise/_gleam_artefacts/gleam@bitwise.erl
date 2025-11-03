-module(gleam@bitwise).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/bitwise.gleam").
-export(['and'/2, 'not'/1, 'or'/2, exclusive_or/2, shift_left/2, shift_right/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(" A set of functions for bitwise operations on integers.\n").

-file("src/gleam/bitwise.gleam", 5).
?DOC(" Calculates the bitwise AND of its arguments.\n").
-spec 'and'(integer(), integer()) -> integer().
'and'(X, Y) ->
    erlang:'band'(X, Y).

-file("src/gleam/bitwise.gleam", 15).
?DOC(" Calculates the bitwise NOT of its argument.\n").
-spec 'not'(integer()) -> integer().
'not'(X) ->
    erlang:'bnot'(X).

-file("src/gleam/bitwise.gleam", 25).
?DOC(" Calculates the bitwise OR of its arguments.\n").
-spec 'or'(integer(), integer()) -> integer().
'or'(X, Y) ->
    erlang:'bor'(X, Y).

-file("src/gleam/bitwise.gleam", 35).
?DOC(" Calculates the bitwise XOR of its arguments.\n").
-spec exclusive_or(integer(), integer()) -> integer().
exclusive_or(X, Y) ->
    erlang:'bxor'(X, Y).

-file("src/gleam/bitwise.gleam", 45).
?DOC(" Calculates the result of an arithmetic left bitshift.\n").
-spec shift_left(integer(), integer()) -> integer().
shift_left(X, Y) ->
    erlang:'bsl'(X, Y).

-file("src/gleam/bitwise.gleam", 55).
?DOC(" Calculates the result of an arithmetic right bitshift.\n").
-spec shift_right(integer(), integer()) -> integer().
shift_right(X, Y) ->
    erlang:'bsr'(X, Y).
