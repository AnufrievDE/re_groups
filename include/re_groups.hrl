-define(re_compile_opts, [ucp, unicode]).

-define(re_capture_opts, [{capture, all_but_first, binary}]).
-define(re_run_opts, ?re_compile_opts ++ ?re_capture_opts).

-define(re_capture_named_opts, [{capture, all_names, binary}]).
-define(re_run_named_opts, ?re_compile_opts ++ ?re_capture_named_opts).

-define(re_sep, <<"[\\s\\S]*?">>).

-define(re_group_name(BinName), <<"?<", BinName/bytes, ">">>).