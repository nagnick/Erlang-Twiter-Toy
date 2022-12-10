%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_login).
-compile(export_all).
-import(engine,[logOn/2,getEngine/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Twitter Clone".

body() -> 
    [
        #span { style="text-align: center",text="Twitter Clone" },
        #panel { style="text-align: center", body=[
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
    %logOn(Username,getEngine()), % replace with a validation function don't return userPID here
    %wf:insert_top(placeholder, "<p>You logged in!!!" ++ Username ++ Password),
    wf:user(Username),
    wf:redirect("/user/home").
