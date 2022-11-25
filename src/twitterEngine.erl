-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,insert/3,query/2]).

-export([]).
%users send/receive tweets. engine distributes tweets
%user stuff
userActor(TweetList,UserFeed,EnginePID)->
  receive
    {tweet, Message}->
      userActor(TweetList,[Message | UserFeed],EnginePID)
  end.
userSendTweet(EnginePID,UserName,Tweet)->
  EnginePID ! {distributeTweet,UserName,Tweet}.

registerUser(Database, Username)->
  PID = spawn(twitterEngine,userActor,[[],[],[],Database]),
  %add PID to userData and push into database
  Value = {PID,[],[],[]},
  insert(Database,Username,Value).


%engine stuff
start()-> % returns enginePID
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase]).

engineActor(UserDatabase,HashTagDatabase)->
  receive
    {registerUser,UserName,PID}-> %USERDATA = {UsersTweets,Following,Followers,PID}
      insert(UserDatabase,UserName, {[],[],[],PID});
    {registerHashTag,HashTag,HashTagData}->
      insert(HashTagDatabase,HashTag,HashTagData);
    {distributeTweet,Tweeter,Tweet}->
      {_,_,SendToList,_} = query(UserDatabase,Tweeter),
      spawn(twitterEngine,distributerActor,[SendToList,Tweet]);
    %WIP
    {retriveUser,RequestingProcess,UserName}->
      RequestingProcess ! query(UserDatabase,UserName);
    {retriveHashTag,RequestingProcess,HashTag}->
      RequestingProcess ! query(HashTagDatabase,HashTag)
  end,
  engineActor(UserDatabase,HashTagDatabase).

distributerActor([],_)-> % distributes tweets to actors in SendToList
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).