%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (user_home).
-import(engine,[getEngine/0,logOn/3,userSendTweet/3,userSendRetweet/3]).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").


main() -> #template { file="./site/templates/bare.html" }.

title() -> "Twitter".

body() ->
    User = wf:user(),
    if
        User == undefined->
            wf:user("Error" ++ "||" ++ "Error");
        true->
            ok
    end,
    {ok,_PID} = wf:comet(fun()-> SplitList = string:split(wf:user(),"||"), dataLoop(logOn(hd(SplitList),hd(tl(SplitList)),getEngine())) end), % run on load
    [
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Twitter Clone" },
            #p{},
            #textbox { id=username, placeholder="Username to subscribe to" },
            #button { text="Subscribe to user", postback={click, subscribeToUser,junk} },
            #p{},
            #textbox { id=hashtag, placeholder="HashTag to subscribe to" },
            #button { text="Subscribe to hashtag", postback={click, subscribeToHashTag,junk} },
            #p{},
            #textbox { id=tweet, placeholder="New Tweet" },
            #button { text="Send Tweet", postback={click,sendTweet,junk} },
            #p{},
            #button { text="Get Tweets", postback={click,getTweet,junk} },
            #p{},
            #panel { id=placeholder},
            #panel{id = tweetFeed}
        ]}
    ].
	
event(Click) ->
EnginePID = getEngine(),
SplitList = string:split(wf:user(),"||"),
Username = hd(SplitList),
UserPID = logOn(Username,hd(tl(SplitList)),EnginePID),
{click,Button,_} = Click,
if
    UserPID == error->
        wf:update(placeholder, "You are not signed in.");
    true->
        if
            Button == sendTweet->
                Tweet = wf:q(tweet),
                userSendTweet(EnginePID,Username,Tweet);
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
                end;
            Button == reTweet->
                {click,_,Tweet} = Click,
                userSendRetweet(EnginePID,Username,Tweet)
        end
end.
    %wf:insert_top(placeholder, "<p> You clicked the button!" ++ wf:user()).


dataLoop(UserPID)->
    if
        UserPID == null->
            wf:update(placeholder, "You are not signed in.");
        UserPID == error->
            wf:update(placeholder, "You are not signed in.");
        true->
            UserPID ! {updateWebUser, self()},
            UserPID ! {getFeed, self()},
            receive
                'INIT'->
                    UserPID ! {updateWebUser, self()},
                    dataLoop(UserPID);
                {'JOIN', _}->
                    dataLoop(UserPID);
                {'EXIT', _, _} -> 
                    UserPID ! {updateWebUser, null};
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
            end
    end.

dataLoop(UserPID,Feed)->
    receive
        'INIT'->
            UserPID ! {updateWebUser, self()},
            dataLoop(UserPID,Feed);
        {'JOIN', _}->
             dataLoop(UserPID,Feed);
        {'EXIT', _, _} -> 
            UserPID ! {updateWebUser, null};
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


renderTweet(Tweet)->
    {UserName,Text} = Tweet,
    #panel{body=[
    "<p>" ++ "By: " ++ UserName ++" Tweet: " ++ Text,
    #p{},
    #button { text="Retweet", postback={click,reTweet,Tweet}},
    #hr{}]}.
tweetFeed([])->
    ok;
tweetFeed(Feed)->
    RedTweets = renderTweet(hd(Feed)),
    wf:insert_top(tweetFeed,RedTweets),
    tweetFeed(tl(Feed)).
