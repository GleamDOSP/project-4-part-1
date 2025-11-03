-module(prng@random).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/prng/random.gleam").
-export([step/2, sample/2, to_yielder/2, to_random_yielder/1, random_sample/1, int/2, float/2, constant/1, fixed_size_list/2, then/2, list/1, map/2, weighted/2, try_weighted/1, map2/3, pair/2, uniform/2, try_uniform/1, choose/2, map3/4, map4/5, map5/6, fixed_size_string/1, string/0, bit_array/0, set/1, fixed_size_set/2, dict/2, fixed_size_dict/3]).
-export_type([generator/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This package provides many building blocks that can be used to define\n"
    " pure generators of pseudo-random values.\n"
    "\n"
    " This is based on the great\n"
    " [Elm implementation](https://package.elm-lang.org/packages/elm/random/1.0.0/)\n"
    " of [Permuted Congruential Generators](https://www.pcg-random.org).\n"
    "\n"
    " _It is not cryptographically secure!_\n"
    "\n"
    " You can use this cheatsheet to navigate the module documentation:\n"
    "\n"
    " <table>\n"
    " <tr>\n"
    "   <td>Building generators</td>\n"
    "   <td>\n"
    "     <a href=\"#int\">int</a>,\n"
    "     <a href=\"#float\">float</a>,\n"
    "     <a href=\"#string\">string</a>,\n"
    "     <a href=\"#fixed_size_string\">fixed_size_string</a>,\n"
    "     <a href=\"#bit_array\">bit_array</a>,\n"
    "     <a href=\"#uniform\">uniform</a>,\n"
    "     <a href=\"#weighted\">weighted</a>,\n"
    "     <a href=\"#choose\">choose</a>,\n"
    "     <a href=\"#constant\">constant</a>\n"
    "   </td>\n"
    " </tr>\n"
    " <tr>\n"
    "   <td>Transform and compose generators</td>\n"
    "   <td>\n"
    "     <a href=\"#map\">map</a>,\n"
    "     <a href=\"#then\">then</a>,\n"
    "     <a href=\"#pair\">pair</a>\n"
    "   </td>\n"
    " </tr>\n"
    " <tr>\n"
    "   <td>Generating common data structures</td>\n"
    "   <td>\n"
    "     <a href=\"#fixed_size_list\">fixed_size_list</a>,\n"
    "     <a href=\"#list\">list</a>,\n"
    "     <a href=\"#fixed_size_dict\">fixed_size_dict</a>,\n"
    "     <a href=\"#dict\">dict</a>\n"
    "     <a href=\"#fixed_size_set\">fixed_size_set</a>,\n"
    "     <a href=\"#set\">set</a>\n"
    "   </td>\n"
    " </tr>\n"
    " <tr>\n"
    "   <td>Getting reproducible values out of generators</td>\n"
    "   <td>\n"
    "     <a href=\"#step\">step</a>,\n"
    "     <a href=\"#sample\">sample</a>,\n"
    "     <a href=\"#to_yielder\">to_yielder</a>\n"
    "   </td>\n"
    " </tr>\n"
    " <tr>\n"
    "   <td>Getting truly random values out of generators</td>\n"
    "   <td>\n"
    "     <a href=\"#random_sample\">random_sample</a>,\n"
    "     <a href=\"#to_random_yielder\">to_random_yielder</a>\n"
    "   </td>\n"
    " </tr>\n"
    " </table>\n"
    "\n"
).

-opaque generator(HJY) :: {generator,
        fun((prng@seed:seed()) -> {HJY, prng@seed:seed()})}.

-file("src/prng/random.gleam", 154).
?DOC(
    " Steps a `Generator(a)` producing a random value of type `a` using the given\n"
    " seed as the source of randomness.\n"
    "\n"
    " The stepping logic is completely deterministic. This means that, given a\n"
    " seed and a generator, you'll always get the same result.\n"
    "\n"
    " This is why this function also returns a new seed that can be used to make\n"
    " subsequent calls to `step` to get other random values.\n"
    "\n"
    " Stepping a generator by hand can be quite cumbersome, so I recommend you\n"
    " try [`to_yielder`](#to_yielder),\n"
    " [`to_random_yielder`](#to_random_yielder), or [`sample`](#sample) instead.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let initial_seed = seed.new(11)\n"
    " let dice_roll = random.int(1, 6)\n"
    " let #(first_roll, new_seed) = random.step(dice_roll, initial_seed)\n"
    " let #(second_roll, _) = random.step(dice_roll, new_seed)\n"
    "\n"
    " #(first_roll, second_roll)\n"
    " // -> #(3, 2)\n"
    " ```\n"
).
-spec step(generator(HJZ), prng@seed:seed()) -> {HJZ, prng@seed:seed()}.
step(Generator, Seed) ->
    (erlang:element(2, Generator))(Seed).

-file("src/prng/random.gleam", 164).
?DOC(
    " Generates a single value using the given generator and seed.\n"
    "\n"
    " This is just a shorthand for the `step` function that drops the new\n"
    " seed. It can be useful if you just need to get a single value out of\n"
    " a generator and need the result to be reproducible.\n"
).
-spec sample(generator(HKB), prng@seed:seed()) -> HKB.
sample(Generator, Seed) ->
    erlang:element(1, step(Generator, Seed)).

-file("src/prng/random.gleam", 224).
?DOC(
    " Turns the given generator into an infinite stream of random values generated\n"
    " with it.\n"
    "\n"
    " `seed` is the seed used to get the initial random value and start the\n"
    " infinite sequence.\n"
    "\n"
    " If you don't care about the initial seed and reproducibility is not your\n"
    " goal, you can use `to_random_yielder` which works like this function and\n"
    " randomly picks the initial seed.\n"
).
-spec to_yielder(generator(HKI), prng@seed:seed()) -> gleam@yielder:yielder(HKI).
to_yielder(Generator, Seed) ->
    gleam@yielder:unfold(
        Seed,
        fun(Seed@1) ->
            {Value, New_seed} = step(Generator, Seed@1),
            {next, Value, New_seed}
        end
    ).

-file("src/prng/random.gleam", 210).
?DOC(
    " Turns the given generator into an infinite stream of random values generated\n"
    " with it.\n"
    "\n"
    " The initial seed is chosen randomly so you won't have control over which\n"
    " values are generated and may get different results each time you call this\n"
    " function.\n"
    "\n"
    " If you want to have control over the initial seed used to get the infinite\n"
    " sequence of values, you can use `to_yielder`.\n"
).
-spec to_random_yielder(generator(HKF)) -> gleam@yielder:yielder(HKF).
to_random_yielder(Generator) ->
    to_yielder(Generator, prng@seed:random()).

-file("src/prng/random.gleam", 191).
?DOC(
    " Generates a single value using the given generator.\n"
    "\n"
    " The initial seed is chosen randomly so you won't have control over which\n"
    " value is generated and may get different results each time you call this\n"
    " function.\n"
    "\n"
    " This is useful if you want to quickly get a value out of a generator and\n"
    " do not care about reproducibility (if you want to decide which seed is\n"
    " used for the generation process you'll have to use `random.step`).\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Imagine you want to perform some action, say only 40% of the times.\n"
    " Your code may look like this:\n"
    "\n"
    " ```gleam\n"
    " let probability = random.float(0.0, 1.0)\n"
    " case random.random_sample(probability) <= 0.4 {\n"
    "   True -> perform_action()\n"
    "   False -> Nil // do nothing\n"
    " }\n"
    " ```\n"
).
-spec random_sample(generator(HKD)) -> HKD.
random_sample(Generator) ->
    Result@1 = case gleam@yielder:first(to_random_yielder(Generator)) of
        {ok, Result} -> Result;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"prng/random"/utf8>>,
                        function => <<"random_sample"/utf8>>,
                        line => 196,
                        value => _assert_fail,
                        start => 6431,
                        'end' => 6498,
                        pattern_start => 6442,
                        pattern_end => 6452})
    end,
    Result@1.

-file("src/prng/random.gleam", 264).
-spec sort_ascending(HKM, HKM, fun((HKM, HKM) -> gleam@order:order())) -> {HKM,
    HKM}.
sort_ascending(One, Other, Compare) ->
    case Compare(One, Other) of
        lt ->
            {One, Other};

        eq ->
            {One, Other};

        gt ->
            {Other, One}
    end.

-file("src/prng/random.gleam", 258).
?DOC(
    " Generates integers in the given inclusive range.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Say you want to model the outcome of a dice, you could use `int` like this:\n"
    "\n"
    " ```gleam\n"
    " let dice_roll = random.int(1, 6)\n"
    " ```\n"
).
-spec int(integer(), integer()) -> generator(integer()).
int(From, To) ->
    {generator,
        fun(Seed) ->
            {Low, High} = sort_ascending(From, To, fun gleam@int:compare/2),
            prng_ffi:random_int(Seed, Low, High)
        end}.

-file("src/prng/random.gleam", 283).
?DOC(
    " Generates floating point numbers in the given inclusive range.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let probability = random.float(0.0, 1.0)\n"
    " ```\n"
).
-spec float(float(), float()) -> generator(float()).
float(From, To) ->
    {generator,
        fun(Seed) ->
            {Low, High} = sort_ascending(From, To, fun gleam@float:compare/2),
            prng_ffi:random_float(Seed, Low, High)
        end}.

-file("src/prng/random.gleam", 305).
?DOC(
    " Always generates the given value, no matter the seed used.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let always_eleven = random.constant(11)\n"
    " random.random_sample(always_eleven)\n"
    " // -> 11\n"
    " ```\n"
).
-spec constant(HKO) -> generator(HKO).
constant(Value) ->
    {generator, fun(Seed) -> {Value, Seed} end}.

-file("src/prng/random.gleam", 465).
-spec get_by_weight({float(), HLG}, list({float(), HLG}), float()) -> HLG.
get_by_weight(First, Others, Countdown) ->
    {Weight, Value} = First,
    case Others of
        [] ->
            Value;

        [Second | Rest] ->
            Positive_weight = gleam@float:absolute_value(Weight),
            case gleam@float:compare(Countdown, Positive_weight) of
                lt ->
                    Value;

                eq ->
                    Value;

                gt ->
                    get_by_weight(Second, Rest, Countdown - Positive_weight)
            end
    end.

-file("src/prng/random.gleam", 552).
-spec do_fixed_size_list(list(HLT), prng@seed:seed(), generator(HLT), integer()) -> {list(HLT),
    prng@seed:seed()}.
do_fixed_size_list(Acc, Seed, Generator, Length) ->
    case Length =< 0 of
        true ->
            {Acc, Seed};

        false ->
            {Value, Seed@1} = step(Generator, Seed),
            do_fixed_size_list([Value | Acc], Seed@1, Generator, Length - 1)
    end.

-file("src/prng/random.gleam", 544).
?DOC(
    " Generates a lists of a fixed size; its values are generated using the\n"
    " given generator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Imagine you're modelling a game of\n"
    " [Risk](https://en.wikipedia.org/wiki/Risk_(game)); when a player \"attacks\"\n"
    " they can roll three dice. You may model that outcome using `fixed_size_list`\n"
    " like this:\n"
    "\n"
    " ```gleam\n"
    " let dice_roll = random.int(1, 6)\n"
    " let attack_outcome = random.fixed_size_list(dice_roll, 3)\n"
    " ```\n"
).
-spec fixed_size_list(generator(HLP), integer()) -> generator(list(HLP)).
fixed_size_list(Generator, Length) ->
    {generator,
        fun(Seed) -> do_fixed_size_list([], Seed, Generator, Length) end}.

-file("src/prng/random.gleam", 746).
?DOC(
    " Transforms a generator into another one based on its generated values.\n"
    "\n"
    " The random value generated by the given generator is fed into the `do`\n"
    " function and the returned generator is used as the new generator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " `then` is a really powerful function, almost all functions exposed by this\n"
    " library could be defined in term of it!\n"
    " Take as an example `map`, it can be implemented like this:\n"
    "\n"
    " ```gleam\n"
    " fn map(generator: Generator(a), with fun: fn(a) -> b) -> Generator(b) {\n"
    "   random.then(generator, fn(value) {\n"
    "     random.constant(fun(value))\n"
    "   })\n"
    " }\n"
    " ```\n"
    "\n"
    " Notice how the `do` function needs to return a `Generator(b)`, you can\n"
    " achieve that by wrapping any constant value with the `random.constant`\n"
    " generator.\n"
    "\n"
    " > Code written with `then` can gain a lot in readability if you use the\n"
    " > `use` syntax, especially if it has some deep nesting. As an example, this\n"
    " > is how you can rewrite the previous example taking advantage of `use`:\n"
    " >\n"
    " > ```gleam\n"
    " > fn map(generator: Generator(a), with fun: fn(a) -> b) -> Generator(b) {\n"
    " >   use value <- random.then(generator)\n"
    " >   random.constant(fun(value))\n"
    " > }\n"
    " > ```\n"
).
-spec then(generator(HNI), fun((HNI) -> generator(HNK))) -> generator(HNK).
then(Generator, Generator_from) ->
    {generator,
        fun(Seed) ->
            {Value, Seed@1} = step(Generator, Seed),
            _pipe = Generator_from(Value),
            step(_pipe, Seed@1)
        end}.

-file("src/prng/random.gleam", 573).
?DOC(
    " Generates a list with a random size with at most 32 items.\n"
    " Each item is generated using the given generator.\n"
    "\n"
    " This is similar to `fixed_size_list` with the difference that the size\n"
    " is chosen randomly.\n"
).
-spec list(generator(HLX)) -> generator(list(HLX)).
list(Generator) ->
    then(int(0, 32), fun(Size) -> fixed_size_list(Generator, Size) end).

-file("src/prng/random.gleam", 773).
?DOC(
    " Transforms the values produced by a generator using the given function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Imagine you want to make a generator for boolean values that returns\n"
    " `True` and `False` with the same probability. You could do that using `map`\n"
    " like this:\n"
    "\n"
    " ```gleam\n"
    " let bool_generator = random.int(1, 2) |> random.map(fn(n) { n == 1 })\n"
    " ```\n"
    "\n"
    " Here `map` allows you to transform the values produced by the initial\n"
    " integer generator - either 1 or 2 - into boolean values: when the original\n"
    " generator produces a 1, `bool_generator` will produce `True`; when the\n"
    " original generator produces a 2, `bool_generator` will produce `False`.\n"
).
-spec map(generator(HNN), fun((HNN) -> HNP)) -> generator(HNP).
map(Generator, Fun) ->
    {generator,
        fun(Seed) ->
            {Value, Seed@1} = step(Generator, Seed),
            {Fun(Value), Seed@1}
        end}.

-file("src/prng/random.gleam", 420).
?DOC(
    " Generates values from the given ones with a weighted probability.\n"
    "\n"
    " This generator can guarantee to produce values since it always takes at\n"
    " least one item (as its first argument); if it were to accept just a list of\n"
    " options, it could be called like this:\n"
    "\n"
    " ```gleam\n"
    " weighted([])\n"
    " ```\n"
    "\n"
    " In which case it would be impossible to actually produce any value: none was\n"
    " provided!\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Given the following type to model the outcome of a coin flip:\n"
    "\n"
    " ```gleam\n"
    " pub type CoinFlip {\n"
    "   Heads\n"
    "   Tails\n"
    " }\n"
    " ```\n"
    "\n"
    " You could write a generator for a loaded coin that lands on head 75% of the\n"
    " times like this:\n"
    "\n"
    " ```gleam\n"
    " let loaded_coin = random.weighted(#(0.75, Heads), [#(0.25, Tails)])\n"
    " ```\n"
    "\n"
    " In this example the weights add up to 1, but you could use any number: the\n"
    " weights get added up to a `total` and the probability of each option is its\n"
    " `weight` / `total`.\n"
).
-spec weighted({float(), HKY}, list({float(), HKY})) -> generator(HKY).
weighted(First, Others) ->
    Normalise = fun(Pair) ->
        gleam@float:absolute_value(gleam@pair:first(Pair))
    end,
    Total = Normalise(First) + gleam@float:sum(
        gleam@list:map(Others, Normalise)
    ),
    map(
        float(+0.0, Total),
        fun(_capture) -> get_by_weight(First, Others, _capture) end
    ).

-file("src/prng/random.gleam", 458).
?DOC(
    " This function works exactly like `weighted` but will return an `Error(Nil)`\n"
    " if the provided argument is an empty list since the generator wouldn't be\n"
    " able to produce any value in that case.\n"
    "\n"
    " It generates values from the given list with a weighted probability.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " random.try_weighted([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " For example if you consider the following type definition to model color:\n"
    "\n"
    " ```gleam\n"
    " type CoinFlip {\n"
    "   Heads\n"
    "   Tails\n"
    " }\n"
    " ```\n"
    "\n"
    " This call of `try_weighted` will produce a generator wrapped in an `Ok`:\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(coin_1) =\n"
    "   random.try_weighted([#(0.75, Heads), #(0.25, Tails)])\n"
    " let coin_2 = random.uniform(#(0.75, Heads), [#(0.25, Tails)])\n"
    " ```\n"
    "\n"
    " The generators `coin_1` and `coin_2` will behave exactly the same.\n"
).
-spec try_weighted(list({float(), HLB})) -> {ok, generator(HLB)} | {error, nil}.
try_weighted(Options) ->
    case Options of
        [First | Rest] ->
            {ok, weighted(First, Rest)};

        [] ->
            {error, nil}
    end.

-file("src/prng/random.gleam", 817).
?DOC(
    " Combines two generators into a single one. The resulting generator produces\n"
    " values obtained by applying `fun` to the values generated by the given\n"
    " generators.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Imagine you need to generate random points in a 2D space:\n"
    "\n"
    " ```gleam\n"
    " pub type Point {\n"
    "   Point(x: Float, y: Float)\n"
    " }\n"
    " ```\n"
    "\n"
    " You can compose two basic generators into a `Point` generator using `map2`:\n"
    "\n"
    " ```gleam\n"
    " let x_generator = random.float(-1.0, 1.0)\n"
    " let y_generator = random.float(-1.0, 1.0)\n"
    " let point_generator = map2(x_generator, y_generator, Point)\n"
    " ```\n"
    "\n"
    " > Notice how you could get the same result using `then`:\n"
    " >\n"
    " > ```gleam\n"
    " > pub fn point_generator() -> Generator(Point) {\n"
    " >   use x <- random.then(random.float(-1.0, 1.0))\n"
    " >   use y <- random.then(random.float(-1.0, 1.0))\n"
    " >   random.constant(Point(x, y))\n"
    " > }\n"
    " > ```\n"
    " >\n"
    " > the `use` syntax paired with `then` may be confusing for other people\n"
    " > reading your code, especially Gleam newcomers.\n"
    " >\n"
    " > Usually `map2`/`map3`/... will be more than enough if you just need to\n"
    " > combine simple generators into more complex ones.\n"
).
-spec map2(generator(HNR), generator(HNT), fun((HNR, HNT) -> HNV)) -> generator(HNV).
map2(One, Other, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Other, Seed@1),
            {Fun(A, B), Seed@2}
        end}.

-file("src/prng/random.gleam", 525).
?DOC(
    " Generates pairs of values obtained by combining the values produced by the\n"
    " given generators.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let one_to_five = random.int(1, 5)\n"
    " let probability = random.float(0.0, 1.0)\n"
    " let ints_and_floats = random.pair(one_to_five, probability)\n"
    "\n"
    " random.random_sample(ints_and_floats)\n"
    " // -> #(3, 0.22)\n"
    " ```\n"
).
-spec pair(generator(HLK), generator(HLM)) -> generator({HLK, HLM}).
pair(One, Other) ->
    map2(One, Other, fun gleam@pair:new/2).

-file("src/prng/random.gleam", 342).
?DOC(
    " Generates values from the given ones with an equal probability.\n"
    "\n"
    " This generator can guarantee to produce values since it always takes at\n"
    " least one item (as its first argument); if it were to accept just a list of\n"
    " options, it could be called like this:\n"
    "\n"
    " ```gleam\n"
    " uniform([])\n"
    " ```\n"
    "\n"
    " In which case it would be impossible to actually produce any value: none was\n"
    " provided!\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Given the following type to model colors:\n"
    "\n"
    " ```gleam\n"
    " pub type Color {\n"
    "   Red\n"
    "   Green\n"
    "   Blue\n"
    " }\n"
    " ```\n"
    "\n"
    " You could write a generator that returns each color with an equal\n"
    " probability (~33%) each color like this:\n"
    "\n"
    " ```gleam\n"
    " let color = random.uniform(Red, [Green, Blue])\n"
    " ```\n"
).
-spec uniform(HKQ, list(HKQ)) -> generator(HKQ).
uniform(First, Others) ->
    weighted(
        {1.0, First},
        gleam@list:map(
            Others,
            fun(_capture) -> gleam@pair:new(1.0, _capture) end
        )
    ).

-file("src/prng/random.gleam", 378).
?DOC(
    " This function works exactly like `uniform` but will return an `Error(Nil)`\n"
    " if the provided argument is an empty list since the generator wouldn't be\n"
    " able to produce any value in that case.\n"
    "\n"
    " It generates values from the given list with equal probability.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " random.try_uniform([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " For example if you consider the following type definition to model color:\n"
    "\n"
    " ```gleam\n"
    " type Color {\n"
    "   Red\n"
    "   Green\n"
    "   Blue\n"
    " }\n"
    " ```\n"
    "\n"
    " This call of `try_uniform` will produce a generator wrapped in an `Ok`:\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(color_1) = random.try_uniform([Red, Green, Blue])\n"
    " let color_2 = random.uniform(Red, [Green, Blue])\n"
    " ```\n"
    "\n"
    " The generators `color_1` and `color_2` will behave exactly the same.\n"
).
-spec try_uniform(list(HKT)) -> {ok, generator(HKT)} | {error, nil}.
try_uniform(Options) ->
    case Options of
        [First | Rest] ->
            {ok, uniform(First, Rest)};

        [] ->
            {error, nil}
    end.

-file("src/prng/random.gleam", 505).
?DOC(
    " Generates two values with equal probability.\n"
    "\n"
    " This is a shorthand for `random.uniform(one, [other])`, but can read better\n"
    " when there's only two choices.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Given the following type to model the outcome of a coin flip:\n"
    "\n"
    " ```gleam\n"
    " pub type CoinFlip {\n"
    "   Heads\n"
    "   Tails\n"
    " }\n"
    " ```\n"
    "\n"
    " You can write a generator for coin flip outcomes like this:\n"
    "\n"
    " ```gleam\n"
    " let flip = random.choose(Heads, Tails)\n"
    " ```\n"
).
-spec choose(HLI, HLI) -> generator(HLI).
choose(One, Other) ->
    uniform(One, [Other]).

-file("src/prng/random.gleam", 861).
?DOC(
    " Combines three generators into a single one. The resulting generator\n"
    " produces values obtained by applying `fun` to the values generated by the\n"
    " given generators.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Imagine you're writing a generator for random enemies in a game you're\n"
    " making:\n"
    "\n"
    " ```gleam\n"
    " pub type Enemy {\n"
    "   Enemy(health: Int, attack: Int, defense: Int)\n"
    " }\n"
    " ```\n"
    "\n"
    " Each enemy starts with a random health (that can go from 50 to 100) and\n"
    " random values for the `attack` and `defense` stats (each can be in a range\n"
    " from 1 to 5):\n"
    "\n"
    " ```gleam\n"
    " let health_generator = random.int(50, 100)\n"
    " let attack_generator = random.int(1, 5)\n"
    " let defense_generator = random.int(1, 5)\n"
    "\n"
    " let enemy_generator =\n"
    "   random.map3(\n"
    "     health_generator,\n"
    "     attack_generator,\n"
    "     defense_generator,\n"
    "     Enemy,\n"
    "   )\n"
    " ```\n"
).
-spec map3(
    generator(HNX),
    generator(HNZ),
    generator(HOB),
    fun((HNX, HNZ, HOB) -> HOD)
) -> generator(HOD).
map3(One, Two, Three, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Two, Seed@1),
            {C, Seed@3} = step(Three, Seed@2),
            {Fun(A, B, C), Seed@3}
        end}.

-file("src/prng/random.gleam", 878).
?DOC(
    " Combines four generators into a single one. The resulting generator\n"
    " produces values obtained by applying `fun` to the values generated by the\n"
    " given generators.\n"
).
-spec map4(
    generator(HOF),
    generator(HOH),
    generator(HOJ),
    generator(HOL),
    fun((HOF, HOH, HOJ, HOL) -> HON)
) -> generator(HON).
map4(One, Two, Three, Four, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Two, Seed@1),
            {C, Seed@3} = step(Three, Seed@2),
            {D, Seed@4} = step(Four, Seed@3),
            {Fun(A, B, C, D), Seed@4}
        end}.

-file("src/prng/random.gleam", 900).
?DOC(
    " Combines five generators into a single one. The resulting generator\n"
    " produces values obtained by applying `fun` to the values generated by the\n"
    " given generators.\n"
    "\n"
    " > There's no `map6`, `map7`, and so on. If you feel like you need to compose\n"
    " > together even more generators, you can use the `random.then` function.\n"
).
-spec map5(
    generator(HOP),
    generator(HOR),
    generator(HOT),
    generator(HOV),
    generator(HOX),
    fun((HOP, HOR, HOT, HOV, HOX) -> HOZ)
) -> generator(HOZ).
map5(One, Two, Three, Four, Five, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Two, Seed@1),
            {C, Seed@3} = step(Three, Seed@2),
            {D, Seed@4} = step(Four, Seed@3),
            {E, Seed@5} = step(Five, Seed@4),
            {Fun(A, B, C, D, E), Seed@5}
        end}.

-file("src/prng/random.gleam", 937).
?DOC(
    " Generates Strings with the given number number of UTF code points.\n"
    "\n"
    " > ⚠️ The generated codepoints will be in the range from 0 (inclusive) to\n"
    " > 1023 (inclusive). If you feel like these strings are not enough for your\n"
    " > needs, please open an issue! I'd love to hear your use case and improve\n"
    " > the package.\n"
).
-spec fixed_size_string(integer()) -> generator(binary()).
fixed_size_string(Size) ->
    _pipe = fixed_size_list(utf_codepoint_in_range(0, 1023), Size),
    map(_pipe, fun gleam_stdlib:utf_codepoint_list_to_string/1).

-file("src/prng/random.gleam", 925).
?DOC(
    " Generates Strings with a random number of UTF code points, between\n"
    " 0 (included) and 32 (included).\n"
    "\n"
    " This is similar to `fixed_size_string`, with the difference that the\n"
    " size is randomly generated as well.\n"
).
-spec string() -> generator(binary()).
string() ->
    then(int(0, 32), fun(Size) -> fixed_size_string(Size) end).

-file("src/prng/random.gleam", 706).
?DOC(" Generates `BitArray`s with a random size.\n").
-spec bit_array() -> generator(bitstring()).
bit_array() ->
    map(string(), fun gleam_stdlib:identity/1).

-file("src/prng/random.gleam", 948).
?DOC(
    " I'm not exposing this function because, if one is not careful with the range,\n"
    " it might lead to a nasty infinite loop.\n"
    " When I come up with a better alternative I might make a similar API public,\n"
    " for now, if someone wants to do something unsafe they will have to\n"
    " manually reimplement it.\n"
).
-spec utf_codepoint_in_range(integer(), integer()) -> generator(integer()).
utf_codepoint_in_range(Lower, Upper) ->
    then(
        int(Lower, Upper),
        fun(Raw_codepoint) -> case gleam@string:utf_codepoint(Raw_codepoint) of
                {ok, Codepoint} ->
                    constant(Codepoint);

                {error, _} ->
                    utf_codepoint_in_range(Lower, Upper)
            end end
    ).

-file("src/prng/random.gleam", 699).
?DOC(
    " Generates a `Set(a)` where each item is generated using the provided\n"
    " generator.\n"
    "\n"
    " This is similar to `fixed_size_set` with the difference that the set is\n"
    " going to have a random size between 0 (inclusive) and 32 (inclusive).\n"
).
-spec set(generator(HND)) -> generator(gleam@set:set(HND)).
set(Generator) ->
    then(int(0, 32), fun(Size) -> fixed_size_set(Generator, Size) end).

-file("src/prng/random.gleam", 651).
?DOC(
    " Generates a `Set(a)` where each item is generated using the provided\n"
    " generator.\n"
    "\n"
    " > ⚠️ This function makes a best effort at generating a set with exactly the\n"
    " > specified number of items, but beware that it may contain less items if\n"
    " > the given generator cannot generate enough distinct values.\n"
).
-spec fixed_size_set(generator(HMU), integer()) -> generator(gleam@set:set(HMU)).
fixed_size_set(Generator, Size) ->
    _pipe = gleam@int:max(Size, 0),
    do_fixed_size_set(Generator, _pipe, 0, 0, gleam@set:new()).

-file("src/prng/random.gleam", 659).
-spec do_fixed_size_set(
    generator(HMY),
    integer(),
    integer(),
    integer(),
    gleam@set:set(HMY)
) -> generator(gleam@set:set(HMY)).
do_fixed_size_set(Generator, Size, Unique_items, Consecutive_attempts, Acc) ->
    Has_required_size = Unique_items =:= Size,
    gleam@bool:guard(
        Has_required_size,
        constant(Acc),
        fun() ->
            Has_reached_maximum_attempts = Consecutive_attempts >= 10,
            gleam@bool:guard(
                Has_reached_maximum_attempts,
                constant(Acc),
                fun() ->
                    then(
                        Generator,
                        fun(Item) -> case gleam@set:contains(Acc, Item) of
                                true ->
                                    _pipe = (Consecutive_attempts + 1),
                                    do_fixed_size_set(
                                        Generator,
                                        Size,
                                        Unique_items,
                                        _pipe,
                                        Acc
                                    );

                                false ->
                                    _pipe@1 = gleam@set:insert(Acc, Item),
                                    do_fixed_size_set(
                                        Generator,
                                        Size,
                                        Unique_items + 1,
                                        0,
                                        _pipe@1
                                    )
                            end end
                    )
                end
            )
        end
    ).

-file("src/prng/random.gleam", 639).
?DOC(
    " Generates a `Map(k, v)` where each key value pair is generated using the\n"
    " provided generators.\n"
    "\n"
    " This is similar to `fixed_size_dict` with the difference that the map is\n"
    " going to have a random number of key-value pairs between 0 (inclusive) and\n"
    " 32 (inclusive).\n"
).
-spec dict(generator(any()), generator(any())) -> generator(gleam@dict:dict(any(), any())).
dict(Keys, Values) ->
    then(int(0, 32), fun(Size) -> fixed_size_dict(Keys, Values, Size) end).

-file("src/prng/random.gleam", 588).
?DOC(
    " Generates a `Dict(k, v)` where each key value pair is generated using the\n"
    " provided generators.\n"
    "\n"
    " > ⚠️ This function makes a best effort at generating a map with exactly the\n"
    " > specified number of keys, but beware that it may contain less items if\n"
    " > the keys generator cannot generate enough distinct keys.\n"
).
-spec fixed_size_dict(generator(HMB), generator(HMD), integer()) -> generator(gleam@dict:dict(HMB, HMD)).
fixed_size_dict(Keys, Values, Size) ->
    _pipe = gleam@int:max(Size, 0),
    do_fixed_size_dict(Keys, Values, _pipe, 0, 0, maps:new()).

-file("src/prng/random.gleam", 597).
-spec do_fixed_size_dict(
    generator(HMG),
    generator(HMI),
    integer(),
    integer(),
    integer(),
    gleam@dict:dict(HMG, HMI)
) -> generator(gleam@dict:dict(HMG, HMI)).
do_fixed_size_dict(Keys, Values, Size, Unique_keys, Consecutive_attempts, Acc) ->
    Has_required_size = Unique_keys =:= Size,
    gleam@bool:guard(
        Has_required_size,
        constant(Acc),
        fun() ->
            Has_reached_maximum_attempts = Consecutive_attempts >= 10,
            gleam@bool:guard(
                Has_reached_maximum_attempts,
                constant(Acc),
                fun() ->
                    then(Keys, fun(Key) -> case gleam@dict:has_key(Acc, Key) of
                                true ->
                                    _pipe = (Consecutive_attempts + 1),
                                    do_fixed_size_dict(
                                        Keys,
                                        Values,
                                        Size,
                                        Unique_keys,
                                        _pipe,
                                        Acc
                                    );

                                false ->
                                    then(
                                        Values,
                                        fun(Value) ->
                                            _pipe@1 = gleam@dict:insert(
                                                Acc,
                                                Key,
                                                Value
                                            ),
                                            do_fixed_size_dict(
                                                Keys,
                                                Values,
                                                Size,
                                                Unique_keys + 1,
                                                0,
                                                _pipe@1
                                            )
                                        end
                                    )
                            end end)
                end
            )
        end
    ).
