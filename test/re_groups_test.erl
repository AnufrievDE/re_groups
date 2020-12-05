-module(re_groups_test).

-include_lib("eunit/include/eunit.hrl").

with_alias_test() ->
    RE1WithGroup = <<"([a-zA-z]*)_">>,
    RE2WithGroup = <<"abc(def)">>,

    %% compile
    {ok, {some_re_alias, MP}} = 
        re_groups:compile({some_re_alias, [RE1WithGroup, RE2WithGroup]}),

    #{mp := MP2} = persistent_term:get(some_re_alias),
    ?assertEqual(MP, MP2),

    Res = re:run(<<"asdqw_abcdef">>, MP, [{capture, all_but_first, binary}]),
    ?assertEqual({match, [<<"asdqw">>, <<"def">>]}, Res),
    
    %% run
    Res2 = re_groups:run(<<"asdqw_abcdef">>, some_re_alias),
    ?assertEqual({ok, [<<"asdqw">>, <<"def">>]}, Res2),

    re_groups:remove(some_re_alias).

with_alias_parts_test() ->
    RE1WithGroup = <<"([a-zA-z]*)_">>,
    RE2WithGroup = <<"abc(def)">>,

    %% compile
    {ok, {some_re_alias, [_MP1, _MP2] = Parts}} = 
        re_groups:compile({some_re_alias, [RE1WithGroup, RE2WithGroup]},
            #{parts => true}),

    #{parts := Parts1} = persistent_term:get(some_re_alias),
    ?assertEqual(Parts, [maps:get(mp, Item) || Item <- Parts1]),
    
    %% run
    Res1 = re_groups:run(<<"asdqw_abcdef">>, some_re_alias, #{parts => true}),
    ?assertEqual([{ok, [<<"asdqw">>]}, {ok, [<<"def">>]}], Res1),
    
    Res2 = re_groups:run(<<"abcdef_">>, some_re_alias, #{parts => true}),
    ?assertEqual([{ok, [<<"abcdef">>]}, {ok, [<<"def">>]}], Res2),
    
    Res3 = re_groups:run(<<"aaa_">>, some_re_alias, #{parts => true}),
    ?assertEqual([{ok, [<<"aaa">>]}, {error, nomatch}], Res3),

    re_groups:remove(some_re_alias).

with_alias_and_names_test() ->
    REListWithGroups = 
        [{<<"re1_name">>, <<"([a-zA-z]*)_">>}, {re2_name, <<"abc(def)">>}],

    %% compile
    {ok, {some_re_alias, MP}} =
        re_groups:compile({some_re_alias, REListWithGroups}),

    #{mp := MP2, aliases := [<<"re1_name">>, re2_name]} =
        persistent_term:get(some_re_alias),
    ?assertEqual(MP, MP2),

    Res = re:run(<<"asdqw_abcdef">>, MP, [{capture, all_but_first, binary}]),
    ?assertEqual({match, [<<"asdqw">>, <<"def">>]}, Res),
    
    %% run
    Res2 = re_groups:run(<<"asdqw_abcdef">>, some_re_alias),
    ?assertEqual({ok,
        [{<<"re1_name">>, <<"asdqw">>}, {re2_name, <<"def">>}]}, Res2),

    re_groups:remove(some_re_alias).

without_alias_test() ->
    RE1WithGroup = <<"([a-zA-z]*)_">>,
    RE2WithGroup = <<"abc(def)">>,

    %% compile
    {ok, MP} = re_groups:compile([RE1WithGroup, RE2WithGroup]),

    Res = re:run(<<"asdqw_123_abcdef">>, MP, [{capture, all_but_first, binary}]),
    ?assertEqual({match, [<<"asdqw">>, <<"def">>]}, Res),
    
    %% run
    Res2 = re_groups:run(<<"asdqw_abcdef">>, MP),
    ?assertEqual({ok, [<<"asdqw">>, <<"def">>]}, Res2),

    %Res3 = re_groups:run(<<"asdqw_abcdef">>, [RE1WithGroup, RE2WithGroup]),
    %?assertEqual({ok, [<<"asdqw">>, <<"def">>]}, Res3),

    re_groups:remove(some_re_alias).