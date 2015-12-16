-module(etest_runner).
-export([run_all/0, run_all/1, run_funs/2, noop/0]).


run_all() ->
    stderr("~p: No tests found.~n", [?MODULE]),
    erlang:halt(1).

run_all(ModuleNames) ->
    init_statistics(),
    stderr("~n"),
    lists:foreach(fun run_suite/1, ModuleNames),
    print_statistics(),
    erlang:halt(get(errors)).


init_statistics() ->
    _ = [put(K, 0) || K <- [errors, successes, tests]],
    ok.

inc(Name) ->
    put(Name, get(Name) + 1).

print_statistics() ->
    Errors = get(errors),
    Stats = lists:flatten(io_lib:format(
                            "    Failed:  ~p"
                            "    Success: ~p"
                            "    Total:   ~p",
                            [Errors, get(successes), get(tests)])),
    Banner = string:copies("#", length(Stats) + 4),
    Summary = io_lib:format("~s~n~s~n~n", [Banner, Stats]),
    Color = summary_color(Errors),
    stderr(colorize(Summary, Color)).


summary_color(0) -> green;
summary_color(_) -> red.

colorize(String, Color) ->
    io_lib:format("~s~s~s", [ascii_color(Color), String, ascii_color(reset)]).

ascii_color(reset) -> "\x1b[0m";
ascii_color(red) -> "\x1b[31m";
ascii_color(green) -> "\x1b[32m".


run_suite(Module) ->
    Tests = test_functions(Module),
    {BeforeSuiteMod, BeforeSuiteFun} = maybe_fun(Module, before_suite),
    {AfterSuiteMod, AfterSuiteFun}  = maybe_fun(Module, after_suite),
    BeforeTest = maybe_fun(Module, before_test),
    AfterTest = maybe_fun(Module, after_test),

    try
        BeforeSuiteMod:BeforeSuiteFun(),
        lists:foreach(fun(T) -> run_test(T, [BeforeTest, T, AfterTest]) end, Tests),
        AfterSuiteMod:AfterSuiteFun()
    catch
        _:Error ->
            inc(errors),
            print_error(Error, erlang:get_stacktrace()),
            stderr("~n")
    end,
    ok.

test_functions(Module) ->
    Functions = exported_functions(Module),
    TestFunctions = lists:filter(fun is_test_function/1, Functions),
    FocusFunctions = lists:filter(fun is_focus_test/1, TestFunctions),

    ActiveFunctions =
        case FocusFunctions of
            [] -> TestFunctions;
            _  -> FocusFunctions
        end,

    [{Module, F} || F <- ActiveFunctions].

exported_functions(Module) ->
    try
        [Function || {Function, _Artiy} <- Module:module_info(exports)]
    catch
        _:Error ->
            print_error(Error, erlang:get_stacktrace()),
            erlang:halt(1)
    end.

is_test_function(Function) ->
    re:run(atom_to_list(Function), "^(focus_)?test_") =/= nomatch.

is_focus_test(Function) ->
    re:run(atom_to_list(Function), "^focus_") =/= nomatch.

maybe_fun(Module, Function) ->
    case proplists:is_defined(Function, Module:module_info(exports)) of
        true  -> {Module, Function};
        false -> {?MODULE, noop}
    end.


run_test({TestModule, TestFunction}, Functions) ->
    inc(tests),

    HeaderPrefix = lists:flatten(io_lib:format("~p : ~p ", [TestModule, TestFunction])),
    Header = string:left(HeaderPrefix, 80, $.),
    stderr("~s~n", [Header]),

    Before = erlang:monotonic_time(),
    try
        run_with_timeout(Functions, 5000),
        inc(successes)
    catch
        _:Error ->
            inc(errors),
            handle_error(Error)
    end,

    After = erlang:monotonic_time(),
    Millis = erlang:convert_time_unit(After - Before, native, milli_seconds),
    DurationStr = lists:flatten(io_lib:format(" ~pms", [Millis])),
    Footer = string:right(DurationStr, 80, $=),
    stderr("~s~n~n", [Footer]),

    ok.


handle_error({error, {Module, Function, Error, Trace}}) ->
    print_error(Error, {{function, Module, Function}, {stacktrace, Trace}});

handle_error({timeout, Timeout, Pid, ProcessInfo}) ->
    print_error({timeout, Timeout}, {{pid, Pid}, {process_info, ProcessInfo}}).

print_error(Error, Trace) ->
    Message = io_lib:format("Error:~n\t~p~nContext:~n\t~p~n", [Error, Trace]),
    stderr(colorize(Message, red)),
    ok.

stderr(Format) -> io:format(standard_error, Format, []).
stderr(Format, Args) -> io:format(standard_error, Format, Args).


run_with_timeout(Functions, Timeout) ->
    Pid = spawn(?MODULE, run_funs, [Functions, self()]),
    receive
        ok -> ok;
        {error, Reason} -> throw({error, Reason})
    after
        Timeout ->
            ProcessInfo = process_info(Pid),
            exit(Pid, kill),
            throw({timeout, Timeout, Pid, ProcessInfo})
    end.

run_funs([], Parent) -> Parent ! ok;

run_funs([{Module, Function}|Rest], Parent) ->
    try
        Module:Function(),
        run_funs(Rest, Parent)
    catch
        _:Error ->
            Trace = erlang:get_stacktrace(),
            Parent ! {error, {Module, Function, Error, Trace}}
    end.


noop() -> ok.
