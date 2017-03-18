-module(prometheus_push_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CT, "text/plain; version=0.0.4").

to_string_test() ->
  ?assertMatch("qwe", prometheus_push:to_string(qwe)),
  ?assertMatch("qwe", prometheus_push:to_string(<<"qwe">>)),
  ?assertMatch("qwe", prometheus_push:to_string("qwe")),
  ?assertMatch("44.2", prometheus_push:to_string(44.2)),

  ?assertMatch({'EXIT',
                {{to_string_failed, #{}}, _}},
               catch prometheus_push:to_string(#{})).

encode_grouping_key_test() ->
  ?assertMatch(<<"/qwe%2Fqwe/qwe%2Fqwe">>,
               iolist_to_binary(
                 prometheus_push:encode_grouping_key(#{"qwe/qwe" => "qwe/qwe"}))),

  ?assertMatch(<<"/qwe%2Fqwe/qwe%2Fqwe">>,
               iolist_to_binary(
                 prometheus_push:encode_grouping_key([{"qwe/qwe", "qwe/qwe"}]))).

get_config_from_env_test() ->
  try
    begin
      ?assertMatch(Map when map_size(Map) == 0,
                            prometheus_push:get_config_from_env()),


      application:set_env(prometheus, pushgateway, #{"qwe" => "qwe"}),
      ?assertMatch(#{"qwe" := "qwe"},
                   prometheus_push:get_config_from_env()),

      application:set_env(prometheus, pushgateway, [{"qwe", "qwe"}]),
      ?assertMatch(#{"qwe" := "qwe"},
                   prometheus_push:get_config_from_env()),
      ?assertMatch({ok, #{"qwe" := "qwe"}}, application:get_env(prometheus, pushgateway))
    end
  after
    application:set_env(prometheus, pushgateway, #{})
  end.

prepare_request_params_test() ->
  try
    begin
      prometheus:start(),
      ?assertMatch({'EXIT',
                    {{badkey, job}, _}},
                   catch prometheus_push:prepare_request_params(#{})),
      ?assertMatch({'EXIT',
                    {{unknown_registry, qwe}, _}},
                   catch prometheus_push:prepare_request_params(#{job => "qwe",
                                                                  registry => qwe})),


      ?assertMatch({"qwe", default, #{}, "http://127.0.0.1:9091"},
                   prometheus_push:prepare_request_params(#{job => "qwe"})),

      application:set_env(prometheus, pushgateway, [{job, "qwe"}]),
      ?assertMatch({"qwe", default, #{}, "http://127.0.0.1:9091"},
                   prometheus_push:prepare_request_params(#{})),

      application:set_env(prometheus, pushgateway, [{job, "qwe"},
                                                    {registry, qwe}]),
      ?assertMatch({"qwe", default, #{}, "http://127.0.0.1:9091"},
                   prometheus_push:prepare_request_params(#{registry => default}))
    end
  after
    application:set_env(prometheus, pushgateway, #{})
  end.

construct_url_test() ->
  ?assertMatch(["address", "/metrics/job/", "qwe%2Fqwe",
                [["/", "qwe%2Fqwe", "/", "qwe%2Fqwe"]]],
               prometheus_push:construct_url("address", "qwe/qwe",
                                             #{"qwe/qwe" => "qwe/qwe"})).

operations_test_() ->
  {foreach,
   fun() ->
       meck:new(httpc)
   end,
   fun(_) ->
       meck:unload(httpc)
   end,
   [
    fun() ->
        meck:expect(httpc, request,
                    fun(put,
                        {"http://127.0.0.1:9091/metrics/job/qwe", [], ?CT, Body}, [], [])
                        when size(Body) > 0 ->
                        {ok, {{"HTTP/1.1", 202, "Accepted"},
                              [{"date", "Sat, 18 Mar 2017 07:59:36 GMT"},
                               {"content-length", "0"},
                               {"content-type", "text/plain; charset=utf-8"}],
                              []}}
                    end),

        ?assertEqual(ok, prometheus_push:push(#{job => "qwe"})),
        ?assert(meck:validate(httpc))
    end,
    fun() ->
        meck:expect(httpc, request,
                    fun(put,
                        {"http://127.0.0.1:9091/metrics/job/qwe/qwe/qwe",
                         [], ?CT, Body}, [], [])
                        when size(Body) > 0 ->
                        {ok, {{"HTTP/1.1", 202, "Accepted"},
                              [{"date", "Sat, 18 Mar 2017 07:59:36 GMT"},
                               {"content-length", "0"},
                               {"content-type", "text/plain; charset=utf-8"}],
                              []}}
                    end),

        ?assertEqual(ok, prometheus_push:push(#{job => "qwe",
                                                grouping_key => #{"qwe" => "qwe"}})),
        ?assert(meck:validate(httpc))
    end,
    fun() ->
        meck:expect(httpc, request,
                    fun(post,
                        {"http://127.0.0.1:9091/metrics/job/qwe", [], ?CT, Body}, [], [])
                        when size(Body) > 0 ->
                        {ok, {{"HTTP/1.1", 202, "Accepted"},
                              [{"date", "Sat, 18 Mar 2017 07:59:36 GMT"},
                               {"content-length", "0"},
                               {"content-type", "text/plain; charset=utf-8"}],
                              []}}
                    end),

        ?assertEqual(ok, prometheus_push:add("qwe")),
        ?assert(meck:validate(httpc))
    end,
    fun() ->
        meck:expect(httpc, request,
                    fun(delete,
                        {"http://127.0.0.1:9091/metrics/job/qwe", [], ?CT, <<>>}, [], [])
                       ->
                        {ok, {{"HTTP/1.1", 202, "Accepted"},
                              [{"date", "Sat, 18 Mar 2017 07:59:36 GMT"},
                               {"content-length", "0"},
                               {"content-type", "text/plain; charset=utf-8"}],
                              []}}
                    end),

        ?assertEqual(ok, prometheus_push:delete("qwe")),
        ?assert(meck:validate(httpc))
    end
   ]}.
