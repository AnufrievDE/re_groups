-module(re_groups).

-include("re_groups.hrl").

-export([
    compile/1,
    compile/2,
    run/2,
    run/3,
    remove/1]).

-type(compile_opts() ::
    #{
        parts => boolean() | only,
        compile_opts => list(),
        sep => binary()
    }).

-type run_opts() ::
    #{
        parts => boolean(), 
        run_opts => named | not_named | list()
    }.

-type alias() :: atom().
-type subj() :: iodata().

-type re() :: re:mp() | raw_re().

-type raw_re() :: iodata() | unicode:charlist().
-type alliased_raw_re() :: {one_or_more_group_aliases(), raw_re()}.

-type raw_res() :: list(raw_re()) | list(alliased_raw_re()).

-type alias_or_re() :: alias() | re().

-type one_or_more_mps() :: re:mp() | list(re:mp()).

-type group_alias() :: term().
-type one_or_more_group_aliases() :: group_alias() | list(group_alias()).

-type captured() :: term().

-type captured_result() :: {group_alias(), captured()} | captured().

-type captured_results() :: list({group_alias(), captured()}) | list(captured()).

-type run_error() :: nomatch | bad_re | any().

%% dummy record for re:mp() type
-record(re_pattern, {a,b,c,d}).

%% API

-spec compile({alias(), raw_res()} | raw_res()) ->
    {ok, {alias(), one_or_more_mps()} | one_or_more_mps()}.
compile(REList) ->
    compile(REList, #{}).

-spec compile({alias(), raw_res()} | raw_res(), compile_opts()) ->
    {ok, {alias(), one_or_more_mps()} | one_or_more_mps()}.
compile({Alias, REList}, Opts) when is_list(REList), is_atom(Alias) ->
    Parts = maps:get(parts, Opts, false),
    CompOpts = maps:get(compile_opts, Opts, ?re_compile_opts),
    case Parts of
        false ->
            compile(Alias, REList, CompOpts, maps:get(sep, Opts, ?re_sep));
        true ->
            compile(Alias, REList, CompOpts, maps:get(sep, Opts, ?re_sep)),
            compile_parts(Alias, REList, CompOpts);
        only ->
            compile_parts(Alias, REList, CompOpts)
    end;
compile(REList, Opts) when is_list(REList) ->
    compile({undefined, REList}, Opts).

compile(Alias, REList, CompOpts, Sep) ->
    {REPartsAliases, REParts} = safe_unzip(REList),
    RE = join_binaries(Sep, REParts),
    {ok, MP} = re:compile(RE, CompOpts),

    maybe_store_and_return(Alias, MP,
        fun() -> store(Alias, MP, REPartsAliases) end).

compile_parts(Alias, REList, CompOpts) ->
    Items = lists:map(
        fun(RE) ->
            {REPartAliases, REPart} = safe_unzip([RE]),
            {ok, MP} = re:compile(REPart, CompOpts),
            item(MP, REPartAliases)
        end, REList),
    maybe_store_and_return(Alias, [MP || #{mp := MP} <- Items],
        fun() -> store_parts(Alias, Items) end).

safe_unzip(REList) ->
    {Aliases, REs} =
    lists:unzip(
        lists:map(
            fun({Aliases, RegExp}) when is_list(Aliases) -> {Aliases, RegExp};
               ({Alias, RegExp}) -> {[Alias], RegExp};
               (RegExp) -> {[], RegExp}
            end, REList)
    ),
    {lists:flatten(Aliases), REs}.

maybe_store_and_return(Alias, ReturnValue, StoreFun)
    when is_function(StoreFun, 0) ->
    case Alias =/= undefined andalso StoreFun() of
        false ->
            {ok, ReturnValue};
        ok ->
            {ok, {Alias, ReturnValue}}
    end.

item(MP, []) -> #{mp => MP};
item(MP, GroupsAliases) -> #{mp => MP, aliases => GroupsAliases}.

store(Alias, MP, GroupsAliases) ->
    Existing = persistent_term:get(Alias, #{}),
    persistent_term:put(Alias, maps:merge(Existing, item(MP, GroupsAliases))).

store_parts(Alias, Items) ->
    Existing = persistent_term:get(Alias, #{}),
    persistent_term:put(Alias, maps:merge(Existing, #{parts => Items})).

retrieve(Alias) ->
    persistent_term:get(Alias).

-spec remove(alias()) -> ok | {error, noexist}.
remove(Alias) when is_atom(Alias) ->
    case persistent_term:erase(Alias) of
        true -> ok;
        false -> {error, noexist}
    end.

-spec run(subj(), alias_or_re()) ->
    {ok, captured_results()} | {error, run_error()} |
    list({ok, captured_result()} | {error, run_error()}).
run(Subject, Alias) ->
    run(Subject, Alias, #{parts => false, run_opts => not_named}).

-spec run(subj(), alias_or_re(), run_opts()) ->
    {ok, captured_results()} | {error, run_error()} |
    list({ok, captured_result()} | {error, run_error()}).
run(Subject, Alias, Opts) when is_atom(Alias) ->
    UOpts = maps:update_with(parts, fun(V) -> V end, false, Opts),
    do_run(Subject, retrieve(Alias), UOpts);
run(Subject, REOrMP, Opts) when is_binary(REOrMP) orelse is_list(REOrMP)
    orelse is_record(REOrMP, re_pattern) ->
    UOpts = maps:update_with(parts, fun(V) -> V end, false, Opts),
    do_run(Subject, REOrMP, [], UOpts);
run(_Subject, _REOrMP, _Opts) ->
    {error, bad_re}.

do_run(Subject, #{mp := MP} = Item, #{parts := false} = Opts) ->
    Aliases = maps:get(aliases, Item, []),
    do_run(Subject, MP, Aliases, Opts);
do_run(Subject, #{parts := Parts} = _Item, #{parts := true} = Opts) ->
    UOpts = Opts#{parts => false},
    [do_run(Subject, Item, UOpts) || Item <- Parts];
do_run(_, _Item, _Opts) ->
    {error, bad_item_or_opts}.
    
do_run(Subject, REOrMP, Aliases, Opts) ->
    RunOpts = maps:get(run_opts, Opts, not_named),
    {RERunOpts, UAliases} = run_opts_and_aliases(REOrMP, RunOpts, Aliases),
    run_re(Subject, REOrMP, UAliases, RERunOpts).

run_opts_and_aliases(#re_pattern{} = MP, named, Aliases) ->
    {?re_capture_named_opts,
        case Aliases of
            [] ->
                {namelist, Names} = re:inspect(MP, namelist),
                Names;
            _ -> Aliases
        end};
run_opts_and_aliases(#re_pattern{}, not_named, Aliases) ->
    {?re_capture_opts, Aliases};
run_opts_and_aliases(_RE, named, Aliases) ->
    {?re_run_named_opts, Aliases};
run_opts_and_aliases(_RE, not_named, Aliases) ->
    {?re_run_opts, Aliases};
run_opts_and_aliases(_REorMP, Opts, Aliases) when is_list(Opts) ->
    {Opts, Aliases}.

run_re(Subject, REOrMP, Aliases, RunOpts) ->
    case run_re(Subject, REOrMP, RunOpts) of
        {ok, Result} ->
            case Aliases of
                [] -> {ok, Result};
                _ -> {ok, lists:zip(Aliases, Result)}
            end;
        Other -> Other
    end.

run_re(Subject, REOrMp, Opts) ->
    case re:run(Subject, REOrMp, Opts) of
        {match, Result} ->
            {ok, Result};
        match ->
            ok;
        nomatch ->
            {error, nomatch};
        Error ->
            Error
    end.

join_binaries(ListOfBinaries) when is_list(ListOfBinaries) ->
    <<Bin || Bin <- ListOfBinaries>>.
join_binaries(Sep, ListOfBinaries) when is_binary(Sep), is_list(ListOfBinaries) ->
    join_binaries(lists:join(Sep, ListOfBinaries)).