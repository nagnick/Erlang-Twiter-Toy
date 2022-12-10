%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_signup).
-import(engine,[getEngine/0,registerUser/3]).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Twitter Clone".

body() -> 
    [
        #span { text="Twitter Clone Sign Up Now!" },
        #panel { style="center-align", body=[
            #textbox { id=username, placeholder="Username", next=textbox2 },
             #p{},
            #password { id=password, placeholder="Password"},
            #p{},
            #button { text="signup", postback=signup },
            #button { text="Go to logIn", postback=click },
            #p{},
            #panel { id=placeholder }
        ]}
    ].
	

event(Click) ->
    if
        Click == signup->
            Username = wf:q(username),
            Password = wf:q(password),
            Result = registerUser(getEngine(),Username,Password),
            if
                Result == error->
                    wf:user("Error" ++ "||" ++ "Error"),
                    wf:update(placeholder, "User Exists");
                true->
                    wf:user(Username ++ "||" ++ Password),
                    wf:redirect("/user/home")
            end;
        true->
             wf:redirect("/user/login")
    end.
    %wf:insert_top(placeholder, "<p>You Signed up. You may now sign in." ++ Username ++ Password),
