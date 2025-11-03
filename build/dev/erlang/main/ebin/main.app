{application, main, [
    {vsn, "0.1.0"},
    {applications, [gleam_erlang,
                    gleam_otp,
                    gleam_stdlib,
                    gleam_time,
                    gleeunit,
                    prng]},
    {description, ""},
    {modules, [main,
               reddit_engine@engine,
               reddit_engine@main,
               simulation@main,
               simulation@simulator]},
    {registered, []}
]}.
