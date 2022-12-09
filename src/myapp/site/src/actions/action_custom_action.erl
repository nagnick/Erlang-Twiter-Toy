%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (action_custom_action).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    render_action/1
]).

%% move the following record definition to site/include/records.hrl
%-record(custom_action, {?ACTION_BASE(action_custom_action),
%        attr1 :: any(),
%        attr2 :: any()
%    }).

%-spec render_action(#custom_action{}) -> actions().
%render_action(_Record = #custom_action{}) ->
%    "alert('Hello, from custom_action!');".
