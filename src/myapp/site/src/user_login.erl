%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from user_login.erl!".

body() -> 
    [
        #span { text="Twitter Clone In Erlang With Nitrogen" },
        #panel { style="center-align", body=[
            #textbox { id=username, placeholder="Username", next=textbox2 },
             #p{},
            #password { id=password, placeholder="Password"},
            #p{},
            #button { text="logIn", postback=click },

            #p{},
            #panel { id=placeholder }
        ]}
    ].
	
event(click) ->
    Username = wf:q(username),
    Password = wf:q(password),
    wf:insert_top(placeholder, "<p>You logged in!!!" ++ Username ++ Password).
