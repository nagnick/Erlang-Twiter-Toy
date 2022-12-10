%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_login).
-compile(export_all).
-import(engine,[logOn/3,getEngine/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Twitter Clone".

body() -> 
    [
        #span {text="Twitter Clone" },
        #panel {  body=[
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
    Result = logOn(Username,Password,getEngine()),
    if
        Result == error->
            wf:user("Error" ++ "||" ++ "Error"),
            wf:update(placeholder, "Incorrect username or password");
        true->
            wf:user(Username++ "||" ++ Password),
            wf:redirect("/user/home")
    end.
