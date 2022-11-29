-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,insert/3,query/2]).

-export([]).
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks

userActor(Username,UserFeed,EnginePID)-> % main userloop
  receive
    {tweet, Message}-> % received from engine
      userActor(Username,[Message | UserFeed],EnginePID);
    {tweets, ListOfTweets}-> % received from engine as a result of following someone new WIP
      % should probably sort the new tweets into my feed maybe with a sorting actor?
      userActor(Username,[ListOfTweets|UserFeed],EnginePID)
  end.
%userActions
userSubscribeTo(EnginePID,UserOrHashTag,ID)-> % done
  EnginePID ! {subscribeTo,self(),UserOrHashTag,ID}.% UserOrHashTag = user for user and hash for hashTag

userSendTweet(EnginePID,UserName,Tweet)-> % done
  EnginePID ! {distributeTweet,UserName,Tweet}.

registerUser(EnginePID, Username)-> % done
  EnginePID ! {registerUser,Username}.

logOn(UserName,EnginePID)->%userStartUp part 2 stuff WIP
  EnginePID ! {logOn,UserName,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.

%engine stuff
start()-> % returns enginePID % done
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase]).
killEngine(EnginePID)-> % done
  EnginePID ! die.

engineActor(UserDatabase,HashTagDatabase)->
  receive
    die -> % WIP
      databaseKiller(UserDatabase), % maybe pass in function so that actors are killed?
      databaseKiller(HashTagDatabase);
      % loop ends here
    {registerUser,Username}-> % done
      PID =  spawn(twitterEngine,userActor,[Username,[],self()]),
      insert(UserDatabase,Username, {[],[],PID}),%USERDATA = {User'sTweets,Subscribers(followers),UserActorPID}
      engineActor(UserDatabase,HashTagDatabase);
    {logOn,Username,PID}-> % done
      {_,_,ActorPID} = query(UserDatabase,Username),
      PID ! ActorPID,
      engineActor(UserDatabase,HashTagDatabase);
    {subscribeTo,UserPID,user,UserToSubscribeTo}-> % WIP
      %should i check if UserToSubscribeTo exists in DB? return null if not in DB
      {Tweets,Subs,SubActorPID} = query(UserDatabase,UserToSubscribeTo),
      insert(UserDatabase,UserToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID}),% update followers list should i check if already in list?
      %Send to user all tweets messed from before subed (user must organize if desired)
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,HashTagDatabase);
    {subscribeTo,UserPID,hash,HashToSubscribeTo}-> %WIP
      %should i check if HashToSubscribeTo exists in DB? return null if not in DB
      {Tweets,Subs,SubActorPID} = query(HashTagDatabase,HashToSubscribeTo),
      insert(HashTagDatabase,HashToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID}),% update followers list
      %Send to user all tweets messed from before subed (user must organize if desired)
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,HashTagDatabase);
    {distributeTweet,Tweeter,Tweet}-> % WIP
      %add parsing to add hashtag tweets to hashTag database
      {Tweets,Subs,SubActorPID} = query(UserDatabase,Tweeter), % send to followers
      spawn(twitterEngine,distributerActor,[Subs,Tweet]),% send tweet with distributer in own process
      % add to Tweeter's Tweet list
      insert(UserDatabase,Tweeter,{[Tweet | Tweets],Subs,SubActorPID}), % update users list of tweets
      engineActor(UserDatabase,HashTagDatabase);
    {distributeHashTag,HashTag,Tweet}-> % done
      Result = query(HashTagDatabase,HashTag),
      if
        Result == null ->
          {Tweets,Subs} = {[],[]},
          insert(HashTagDatabase,HashTag, {[],[]});
        true ->
          {Tweets,Subs} = Result
      end,
      distributerActor(Subs,Tweet),
      insert(HashTagDatabase,HashTag, {[Tweet|Tweets]}),
      engineActor(UserDatabase,HashTagDatabase)
  end.

distributerActor([],_)-> % distributes tweets to actors in SendToList
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).