-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,userDatabaseKiller/1,insert/3,query/2]).

-export([simulate/2,spinUpUsers/2,zipF/4,userSubscribeTo/3,userSendTweet/3,registerUser/2,logOn/2,startEngine/0,killEngine/1,engineActor/2,distributerActor/2,userActor/3]).
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks
simulate(NumberOfUsers,ZipfAlpha)->
  zipF(ZipfAlpha,1,2,0),
  EnginePID = startEngine(),
  spinUpUsers(EnginePID,NumberOfUsers).
% use zipf zipf(probabilitymas, numberofUsers) returns an array of numbers one for each user... this number will be the
% amount of subscribers for each user. some are very popular most are not.
% if all users have a rank value 1 - n we can calculate their popularity
zipF(AlphaExponentCharacterizingDistribution, 1, X,RunningSum)-> % implemented from http://www.math.wm.edu/~leemis/chart/UDR/PDFs/Zipf.pdf
  Sum = 1 + RunningSum,
  math:ceil(1/math:pow(X,AlphaExponentCharacterizingDistribution)* Sum); % ceiling to get integers >=1
zipF(AlphaExponentCharacterizingDistribution, N, X, RunningSum)-> % N is total users and X is the rank of a random user
  Sum = math:pow(1/N,AlphaExponentCharacterizingDistribution) + RunningSum,
  zipF(AlphaExponentCharacterizingDistribution,N-1,X,Sum).

spinUpUsers(_,0)->
  ok;
spinUpUsers(EnginePID,NumberOfUsers)->
  RandomUsername = "RANDOM",
  registerUser(EnginePID,RandomUsername),
  spinUpUsers(EnginePID,NumberOfUsers-1).

userActor(Username,UserFeed,EnginePID)-> % main userloop this will become the socket in part 2
  % random chance of sending a tweet
  %if totalNumMySubscribers <= rand(totalUsers). in simulate we assign users subscribers according to zipf
  % do:
    %if randomchance = 1 send tweet
    % else do nothing
  % else: nothing
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

logOn(UserName,EnginePID)->%userStartUp part 2 stuff WIP pass this engine useractor to the websocket actor as api
  EnginePID ! {logOn,UserName,self()},
  receive
    PID->
      PID %% return the PID of the userActor for the username given
  end.

%engine stuff
startEngine()-> % returns enginePID % done
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase]).
killEngine(EnginePID)-> % done
  EnginePID ! die.

engineActor(UserDatabase,HashTagDatabase)->
  receive
    die -> % done
      userDatabaseKiller(UserDatabase), % special version that kills the userActors as well
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
      % tweet = [heloo world #uf #top5 @mybestfriend]
      % ListofTags = [#uf ,#top5] from parser
      %add parsing to add hashtag tweets to hashTag database
        %ListofTags =[],% parser(Tweet),
        %ListOfMentions =
        %self() ! {distributeHashTag,hd(ListofTags),Tweet},
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