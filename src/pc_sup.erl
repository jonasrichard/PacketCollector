-module(pc_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {#{startegy => one_for_one,
            intensity => 5,
            period => 1000},
          [#{id => pc_logger,
             start => {pc_logger, start_link, ["log"]},
             restart => permanent,
             shutdown => brutal_kill,
             type => worker,
             modules => []},
           #{id => pc_collector,
             start => {pc_collector, start_link, []},
             restart => permanent,
             shutdown => brutal_kill,
             type => worker,
             modules => []},
           #{id => pc_listener,
             start => {pc_listener, start_link, [4000, "log"]},
             restart => permanent,
             shutdown => brutal_kill,
             type => worker,
             modules => []}
          ]}
         }.
