-module(prometheus_push).

-export([push/0,
         push/1,

         add/0,
         add/1,

         delete/0,
         delete/1
        ]).

-export([hostname_grouping_key/0]).

-ifdef(TEST).
-export([encode_grouping_key/1,
         to_string/1,
         get_config_from_env/0,
         prepare_request_params/1,
         construct_url/3]).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

push() ->
  request(put, #{}).

push(Options) when is_map(Options) ->
  request(put, Options);
push(Job) ->
  request(put, #{job => Job}).

add() ->
  request(post, #{}).

add(Options) when is_map(Options) ->
  request(post, Options);
add(Job) ->
  request(post, #{job => Job}).

delete() ->
  request(delete, #{}).

delete(Options) when is_map(Options) ->
  request(delete, Options);
delete(Job) ->
  request(delete, #{job => Job}).

hostname_grouping_key() ->
  {ok, Hostname} = inet:gethostname(),
  #{
     <<"instance">> => Hostname
   }.

%%%===================================================================
%%% Private functions
%%%===================================================================

request(Method, Request) ->
  try
    {Job, Registry, GroupingKey, Address} = prepare_request_params(Request),

    URL = construct_url(Address, Job, GroupingKey),

    ContentType = prometheus_text_format:content_type(),

    Body =
      case Method of
        delete ->
          <<>>;
        _ ->
          prometheus_text_format:format(Registry)
      end,

    request(Method, URL, ContentType, Body)

  catch
    error:{badkey, Key} ->
      {error, {missing_option, Key}};
    error:{unknown_registry, UR} ->
      {error, {unknown_registry, UR}};
    error:{to_string_failed, IS} ->
      {error, {to_string_failed, IS}}
  end.

request(Method, URL0, CT0, Body) ->
  URL = binary_to_list(iolist_to_binary(URL0)),
  CT = binary_to_list(iolist_to_binary(CT0)),

  case httpc:request(Method, {URL, [], CT, Body}, [], []) of
    {ok, {{_, 202, _}, _, _}} ->
      ok;
    Error -> Error
  end.

prepare_request_params(Request) ->
  Config0 = get_config_from_env(),

  Config = maps:merge(Config0, Request),

  Job = maps:get(job, Config),

  Registry = maps:get(registry, Config, default),
  case prometheus_registry:exists(Registry) of
    true -> ok;
    false ->
      erlang:error({unknown_registry, Registry})
  end,

  GroupingKey = maps:get(grouping_key, Config, #{}),

  Address = maps:get(address, Config, "http://127.0.0.1:9091"),

  {Job, Registry, GroupingKey, Address}.

get_config_from_env() ->
  case application:get_env(prometheus, pushgateway, #{}) of
    LC when is_list(LC) ->
      MC = maps:from_list(LC),
      application:set_env(prometheus, pushgateway, MC),
      MC;
    MC when is_map(MC) ->
      MC
  end.

encode_grouping_key(GK) when is_map(GK) ->
  maps:fold(fun(K, V, Acc) ->
                [encode_grouping_key_pair(K, V) ++ Acc]
            end,
            [],
            GK);
encode_grouping_key(GK) when is_list(GK) ->
  lists:foldl(fun({K, V}, Acc) ->
                  [encode_grouping_key_pair(K, V) ++ Acc]
              end,
              [],
              GK).

encode_grouping_key_pair(K, V) ->
  ["/", http_uri:encode(to_string(K)),
   "/", http_uri:encode(to_string(V))].

to_string(Val) ->
  binary_to_list(iolist_to_binary(to_string_(Val))).

to_string_(Val) when is_atom(Val) ->
  atom_to_binary(Val, utf8);
to_string_(Val) when is_number(Val) ->
  io_lib:format("~p", [Val]);
to_string_(Val) ->
  try iolist_to_binary(Val) of
      Str -> Str
  catch
    error:badarg ->
      erlang:error({to_string_failed, Val})
  end.

construct_url(Address, Job, GroupingKey) ->
  [Address,
   "/metrics/job/", http_uri:encode(to_string(Job)),
   encode_grouping_key(GroupingKey)].
