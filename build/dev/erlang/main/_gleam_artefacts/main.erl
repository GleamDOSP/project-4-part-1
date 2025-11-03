-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/main.gleam").
-export([main/0]).

-file("src/main.gleam", 3).
-spec main() -> integer().
main() ->
    simulation@main:main().
