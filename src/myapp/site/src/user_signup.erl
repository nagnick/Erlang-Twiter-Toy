%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_signup).
-import(engine,[getEngine/0,registerUser/2]).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from user_signup.erl!".

body() -> 
    [
        #span { text="Twitter Clone In Erlang With Nitrogen" },
        #panel { style="center-align", body=[
            #textbox { id=username, placeholder="Username", next=textbox2 },
             #p{},
            #password { id=password, placeholder="Password"},
            #p{},
            #button { text="signup", postback=signup },

            #p{},
            #panel { id=placeholder }
        ]}
    ].
	
event(signup) ->
    Username = wf:q(username),
    Password = wf:q(password),
    registerUser(getEngine(),Username),
    wf:insert_top(placeholder, "<p>You Signed up. You may now sign in." ++ Username ++ Password),
    wf:redirect("/user/login").
