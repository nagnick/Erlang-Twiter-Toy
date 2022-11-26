-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,insert/3,query/2]).

-export([]).
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks

userActor(Username,UserFeed,EnginePID)-> % main userloop
  receive
    {tweet, Message}-> % received from engine
      userActor(Username,[Message | UserFeed],EnginePID);
    {tweets, ListOfTweets}-> % received from engine as a result of following someone new
      % should probably sort the new tweets into my feed?
      userActor(Username,[ListOfTweets|UserFeed],EnginePID)
  end.
%userActions

userSendTweet(EnginePID,UserName,Tweet)->
  EnginePID ! {distributeTweet,UserName,Tweet}.

userFollowUser(EnginePID,Username,ToFollowUsername)->
  EnginePID ! {addFollow,Username, ToFollowUsername}.

registerUser(EnginePID, Username)->
  EnginePID ! {registerUser,Username}.

logOn(UserName,EnginePID)->
  EnginePID ! {logOn,UserName,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.

%engine stuff
start()-> % returns enginePID
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase]).
killEngine(EnginePID)->
  EnginePID ! die.

engineActor(UserDatabase,HashTagDatabase)->
  receive
    die ->
      databaseKiller(UserDatabase), % maybe pass in function so that actors are killed? or turn on/off actor?
      databaseKiller(HashTagDatabase);
      % loop ends here
    {registerUser,Username}->
      PID = spawn(twitterEngine,userActor,[Username,[],self()]),% actor values = Username,UserFeed,EnginePID
      insert(UserDatabase,Username, {[],[],[],PID}),%USERDATA = {UsersTweets,Following,Followers,UserActorPID} NOTE Following& Followers are those users PIDs
      engineActor(UserDatabase,HashTagDatabase);
    {logOn,Username,PID}->
      {_,_,_,UserActorPID} = query(UserDatabase,Username),
      PID ! UserActorPID,
      engineActor(UserDatabase,HashTagDatabase);
    {addFollow,Username, ToFollowUserName}-> % Username now follows ToFollowUsername
      {A,B,C,FollowUserPID} = query(UserDatabase,ToFollowUserName),
      {D,E,F,UserActorPID} = query(UserDatabase,Username),
      insert(UserDatabase,ToFollowUserName,{A,B,[UserActorPID|C],FollowUserPID}),% update followers list
      %add ToFollowUser's tweetList into users feed (user must organize if desired)
      UserActorPID ! {tweets,A},
      insert(UserDatabase,Username,{D,[FollowUserPID| E],F,UserActorPID}), % update following list
      engineActor(UserDatabase,HashTagDatabase);
    {distributeTweet,Tweeter,Tweet}->
      %add parsing to add hashtag tweets to hashTag database
      {_,_,SendToList,_} = query(UserDatabase,Tweeter), % send to followers
      spawn(twitterEngine,distributerActor,[SendToList,Tweet]),% send tweet with distributer in own process
      engineActor(UserDatabase,HashTagDatabase);
    %WIP
    {registerHashTag,HashTag,HashTagData}->
      insert(HashTagDatabase,HashTag,HashTagData),
      engineActor(UserDatabase,HashTagDatabase);
    {retriveHashTag,RequestingProcess,HashTag}->
      RequestingProcess ! query(HashTagDatabase,HashTag),
      engineActor(UserDatabase,HashTagDatabase)
  end.

distributerActor([],_)-> % distributes tweets to actors in SendToList
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).