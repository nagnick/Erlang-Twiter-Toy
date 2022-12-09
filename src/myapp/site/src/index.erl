%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-import(engine,[logOn/2,getEngine/0]).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Nitrogen".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    [
        #h1 { text="Welcome to Nitrogen" },
        #p{},
        "
        If you can see this page, then your Nitrogen server is up and
        running. Click the button below to test postbacks.
        ",
        #p{}, 	
        #button { id=login, text="Click to login", postback={click,login} },
		#button { id=signup, text="Click to signup", postback={click,signup} },
		#p{}
    ].
	
event({click,Button}) ->
if
    Button == signup->
        wf:redirect("/user/signup");
    Button == login->
        wf:redirect("/user/login");
    true->
        ok
end.

