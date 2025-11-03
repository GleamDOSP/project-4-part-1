-module(prng@seed).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/prng/seed.gleam").
-export([new/1, random/0]).
-export_type([seed/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type seed() :: any().

-file("src/prng/seed.gleam", 12).
?DOC(" Creates a new seed from a given integer.\n").
-spec new(integer()) -> seed().
new(Int) ->
    prng_ffi:new_seed(Int).

-file("src/prng/seed.gleam", 18).
?DOC(
    " Creates a new random seed. You can use it when you don't care about\n"
    " having reproducible results and just need to get some values out of a\n"
    " generator using the `random.step` function.\n"
).
-spec random() -> seed().
random() ->
    prng_ffi:new_seed(gleam@int:random(4294967296)).
