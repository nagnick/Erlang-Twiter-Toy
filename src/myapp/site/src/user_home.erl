%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_home).
-import(engine,[getEngine/0,registerUser/2,logOn/2,userSendTweet/3]).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Twitter".

body() -> 
    {ok,_PID} = wf:comet(fun()-> dataLoop(logOn(wf:user(),getEngine())) end), % run on load
    [
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Twitter Clone" },
            #p{},
            #textbox { id=username, placeholder="Username to subscribe to" },
            #button { text="Subscribe to user", postback={click, subscribeToUser} },
            #p{},
            #textbox { id=hashtag, placeholder="HashTag to subscribe to" },
            #button { text="Subscribe to hashtag", postback={click, subscribeToHashTag} },
            #p{},
            #textbox { id=tweet, placeholder="New Tweet" },
            #button { text="Send Tweet", postback={click,sendTweet} },
            #p{},
            #button { text="Get Tweets", postback={click,getTweet} },
            #p{},
            #panel { id=placeholder}
        ]}
    ].
	
event({click, Button}) ->
EnginePID = getEngine(),
UserName = wf:user(),
UserPID = logOn(wf:user(),getEngine()),
io:format("~p~n",[UserPID]),
if
    Button == sendTweet->
        Tweet = wf:q(tweet),
        userSendTweet(EnginePID,UserName,Tweet);
    Button == subscribeToUser->
        UserToSubTo = wf:q(username),
        UserPID ! {subscribeToU, UserToSubTo};
    Button == subscribeToHashTag->
        HashTag = wf:q(hashtag),
        UserPID ! {subscribeToH, HashTag};
    Button == getTweet->
        UserPID ! {getFeed, self()},
        receive
        {tweet,Tweet}->
            tweetFeed([Tweet]),
            wf:flush();
       {tweetList,ListOfTweets}->
            tweetFeed(ListOfTweets),
            wf:flush()
        end
end,
    wf:insert_top(placeholder, "<p> You clicked the button!" ++ wf:user()).

dataLoop(UserPID)->
    UserPID ! {updateWebUser, self()},
    UserPID ! {getFeed, self()},
    receive
        'INIT'->
            UserPID ! {updateWebUser, self()},
            dataLoop(UserPID);
        {'JOIN', _}->
            dataLoop(UserPID);
        {'EXIT', _, _} -> 
            logOn(wf:user(),getEngine()) ! {updateWebUser, null};
        {tweet,Tweet}->
            tweetFeed([Tweet]),
            NewFeed = [Tweet],
            wf:flush(),
            dataLoop(UserPID,NewFeed);
       {tweetList,ListOfTweets}->
            tweetFeed(ListOfTweets),
            NewFeed = ListOfTweets,
            wf:flush(),
            dataLoop(UserPID,NewFeed)
    end.

dataLoop(UserPID,Feed)->
    receive
        'INIT'->
            UserPID ! {updateWebUser, self()},
            dataLoop(UserPID,Feed);
        {'JOIN', _}->
             dataLoop(UserPID,Feed);
        {'EXIT', _, _} -> 
            logOn(wf:user(),getEngine()) ! {updateWebUser, null};
        {tweet,Tweet}->
            tweetFeed([Tweet]),
            NewFeed = [Tweet] ++ Feed,
            wf:flush(),
            dataLoop(UserPID,NewFeed);
       {tweetList,ListOfTweets}->
            tweetFeed(ListOfTweets),
            NewFeed = ListOfTweets ++ Feed,
            wf:flush(),
            dataLoop(UserPID,NewFeed)
    end.


renderTweet({UserName,Text})->
    "<p>" ++ "By: " ++ UserName ++" Tweet: " ++ Text.
tweetFeed([])->
    ok;
tweetFeed(Feed)->
    RedTweets = renderTweet(hd(Feed)),
    wf:insert_top(placeholder,RedTweets),
    tweetFeed(tl(Feed)).
