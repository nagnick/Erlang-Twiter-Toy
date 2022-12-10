-module(engine).
-compile(export_all).
-import(string,[equal/2]). 
% point of access for client to access user account this must change to accept the websocket design

logOn(UserName,Password,EnginePID)->%userStartUp part 2 stuff WIP pass this engine useractor to the websocket actor as api
  EnginePID ! {logOn,UserName,Password,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks
userActor(Username,UserFeed,EnginePID,WebUser)-> % middle man of engine and webuser actors
  receive
    die ->
      ok;
    {tweet, Message}-> % received from engine % in part 2 these will be relayed over the socket if connected
      if
        WebUser == null->
          ok;
        true ->
          WebUser ! {tweet,Message}
      end,
      userActor(Username,[Message] ++ UserFeed,EnginePID,WebUser);
    {tweets, ListOfTweets}-> % received from engine as a result of following someone new
      if
        WebUser == null->
          ok;
        true ->
          WebUser ! {tweetList,ListOfTweets}
      end,
      userActor(Username,ListOfTweets++ UserFeed,EnginePID,WebUser);
      % recived from web users
    {sendTweet,Tweet}->
        userSendTweet(EnginePID,Username,Tweet),
        userActor(Username,UserFeed,EnginePID,WebUser);
    {subscribeToU, User}->
        userSubscribeTo(EnginePID,user,User),
        userActor(Username,UserFeed,EnginePID,WebUser);
    {subscribeToH, HashTag}->
        userSubscribeTo(EnginePID,hash,HashTag),
        userActor(Username,UserFeed,EnginePID,WebUser);
    {updateWebUser, NewWebUser}->
        userActor(Username,UserFeed,EnginePID,NewWebUser);
    {getFeed, UserPID}->
        UserPID ! {tweetList,UserFeed},
        userActor(Username,UserFeed,EnginePID,WebUser)
  end.
%userActions
userSubscribeTo(EnginePID,UserOrHashTag,ID)-> % done
  EnginePID ! {subscribeTo,self(),UserOrHashTag,ID}.% UserOrHashTag = 'user' for user and 'hash' for hashTag

userSendTweet(EnginePID,UserName,Tweet)-> % done
  % tweets can be original or a retweet(gotten from userfeed) left for user to decide as engine see's no difference between them
  EnginePID ! {distributeTweet,{UserName,Tweet}}.

userSendRetweet(EnginePID,Username,OriginalTweet)->% done originalTweet = {OriginalSender, OriginalMessage}
  {OSender,OMessage} = OriginalTweet,
  Tweet =  "ReTweet: \"" ++ OMessage ++ ("\" Original creator: " ++ OSender), % retweets you send the original tweet as your own but at end you mention original poster
  EnginePID ! {distributeTweet,{Username,Tweet}}.


%engine stuff
getEngine()->
  PID = whereis(engine),
  if
    PID == undefined->
      startEngine();
    true->
      PID
  end.
startEngine()-> % returns enginePID % done
  UserDatabase = #{},
  HashTagDatabase = #{},
  PID = spawn(engine,engineActor,[UserDatabase,HashTagDatabase]),
  register(engine, PID),
  PID.
killEngine(EnginePID)-> % done
  EnginePID ! die.

registerUser(EnginePID, Username,Password)-> % done
  EnginePID ! {registerUser,Username,Password,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.

engineActor(UserDatabase,HashTagDatabase)->
  % this actor will have to be able to recive connection requests through logOn and pair that connection to the appropriate user actor
  % maybe put a socketServer listen on a seperate actor that will pass the connection to this user and give it to userActors to pass information back and forth
  receive
    die -> % done
      killActorInUserDatabase(UserDatabase),
      ok;
  % loop ends here don't call self
    {registerUser,Username,Password,PID}-> % done
      {_,_,_,ActorPID} = maps:get(Username,UserDatabase,{"error",[],[],null}),
      if
        ActorPID == null-> % create user
          PID2 =  spawn(engine,userActor,[Username,[],self(),null]), %TotalNumberOfUsers used for tweet simulation...
          PID ! PID2,
          engineActor(maps:put(Username,{Password,[],[],PID2},UserDatabase),HashTagDatabase);
        true-> % error user exists
          PID ! error,
          engineActor(UserDatabase,HashTagDatabase)
      end;
    {logOn,Username,Password,PID}-> % done
      {Password1,_,_,ActorPID} = maps:get(Username,UserDatabase,{"error",[],[],null}),
      Result = equal(Password, Password1),
      if
        Result->
          PID ! ActorPID;
        true->
          PID ! error
      end,
      engineActor(UserDatabase,HashTagDatabase);
    {subscribeTo,UserPID,user,UserToSubscribeTo}-> % done
      {Pass,Tweets,Subs,SubActorPID} = maps:get(UserToSubscribeTo,UserDatabase,{"error",[],[],null}),
      %Send to user all tweets sent from before subed
      if
        SubActorPID == null->
          engineActor(UserDatabase,HashTagDatabase),
          UserPID ! error;
        true->
          UserPID ! {tweets,Tweets},
          engineActor(maps:put(UserToSubscribeTo,{Pass,Tweets,[UserPID|Subs],SubActorPID},UserDatabase),HashTagDatabase)
      end;
    {subscribeTo,UserPID,hash,HashToSubscribeTo}-> % done
      {Tweets,Subs} = maps:get(HashToSubscribeTo,HashTagDatabase,{[],[]}),
      %Send to user all tweets sent from before subed
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,maps:put(HashToSubscribeTo,{Tweets,[UserPID|Subs]},HashTagDatabase));
    {distributeTweet,FullTweet}-> % done
      {Tweeter,Tweet} = FullTweet,
      % Tweets sent as {User,message} tweet will be just the message
      ListOfHashTags = tweetSpecialCharParse(Tweet,"#"),
      ListOfMentions = tweetSpecialCharParse(Tweet,"@"),
      spawn(engine,processHashTags,[ListOfHashTags,FullTweet,self()]),
      spawn(engine,processMentions,[ListOfMentions,FullTweet,UserDatabase]),
      {Pass,Tweets,Subs,SubActorPID} = maps:get(Tweeter,UserDatabase), % send to followers
      spawn(engine,distributerActor,[Subs,FullTweet]),% send tweet with distributer in own process
      % add to Tweeter's Tweet list
      engineActor(maps:put(Tweeter,{Pass,[FullTweet | Tweets],Subs,SubActorPID},UserDatabase),HashTagDatabase);
    {distributeHashTag,HashTag,Tweet}-> % done
      {Tweets,Subs} = maps:get(HashTag,HashTagDatabase,{[],[]}),
      spawn(engine,distributerActor,[Subs,Tweet]),
      HashTagDatabase2 = maps:put(HashTag,{[Tweet|Tweets],Subs},HashTagDatabase),
      engineActor(UserDatabase,HashTagDatabase2)
  end.

processHashTags([],_,_)->
  ok;
processHashTags(ListOfHashTags,Tweet,EnginePID)-> % sends hashtags to engine to distribute/store
  EnginePID! {distributeHashTag,hd(ListOfHashTags),Tweet},
  processHashTags(tl(ListOfHashTags),Tweet,EnginePID).

processMentions([],_,_)->
  ok;
processMentions(ListOfMentions,Tweet,UserDatabase)-> % sends tweet to users mentioned in tweet
  {_,_,_,ActorPID} = maps:get(string:sub_string(hd(ListOfMentions),2),UserDatabase,{"error",[],[],null}),
  if
    ActorPID == null-> % no actor with that name don't do anything
      ok;
    true ->
      {U,T} = Tweet,
      ActorPID ! {tweet, {U,"Mentioned in Tweet:" ++ T}}
  end,
  processMentions(tl(ListOfMentions),Tweet,UserDatabase).

distributerActor([],_)-> % distributes tweets to actors in SendToList allows engine to focus on main functionality
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).

%helpers
actorKiller([])-> %Tell actors to kill themselves
  ok;
actorKiller(ListOfActors)->
  PID = hd(ListOfActors),
  PID ! die,
  actorKiller(tl(ListOfActors)).

killActorInUserDatabase(UserDatabase)->
  UserList = [PIDs || {_,{_,_,PIDs}} <- maps:to_list(UserDatabase)],
  actorKiller(UserList). % kill the user actors

specialCharSearch([],_,ListOfHashTags)->
  ListOfHashTags;
specialCharSearch(List_of_Tweet,Char,ListOfHashTags)->
  Split_string = hd(List_of_Tweet),
  StartOfHash = string:find(Split_string,Char),
  if
    StartOfHash == nomatch->
      specialCharSearch(tl(List_of_Tweet),Char,ListOfHashTags);
    true ->
      Matched_tag = string:equal(Char,string:sub_string(StartOfHash,1, 1)),
      if
        Matched_tag->
          %io:fwrite("HashTag is ~p ~n",[StartOfHash]),
          specialCharSearch(tl(List_of_Tweet),Char,[StartOfHash|ListOfHashTags]);
        true->
          specialCharSearch(tl(List_of_Tweet),Char,ListOfHashTags)
      end
  end.

tweetSpecialCharParse(Tweet,SpecialChar)-> % returns list of hashtags if "#" or mentions if "@"...
  List_of_Tweet = string:split(Tweet, " ",all),
  specialCharSearch(List_of_Tweet,SpecialChar,[]).