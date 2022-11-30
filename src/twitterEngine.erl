-module(twitterEngine).
-import(database,[createDatabase/1,chordActor/1,databaseKiller/1,userDatabaseKiller/1,insert/3,query/2,numberToString/1]).

-export([simulate/2,zipF/4,userSubscribeTo/3,userSendTweet/3,registerUser/2,logOn/2,startEngine/1,killEngine/1,engineActor/3,distributerActor/2,userActor/4]).
%users send/receive tweets. engine distributes tweets
%user stuff User can only contact the engine to accomplish tasks
simulate(NumberOfUsers,ZipfAlpha)->
  zipF(ZipfAlpha,1,2,0),
  EnginePID = startEngine(NumberOfUsers),
  ListOfUserNames = spinUpUsers(EnginePID,NumberOfUsers,[]),
  EnginePID ! {setupSim, ListOfUserNames,NumberOfUsers,ZipfAlpha}.

% use zipf zipf(alpha, TotalNumberofUsers,RankOfUser,0) returns an array of numbers one for each user... this number will be the
% amount of subscribers for each user. some are very popular most are not.
% if all users have a rank value 1 - n we can calculate their popularity
zipF(AlphaExponentCharacterizingDistribution, 1, X,RunningSum)-> % implemented from http://www.math.wm.edu/~leemis/chart/UDR/PDFs/Zipf.pdf
  Sum = 1 + RunningSum,
  math:ceil(1/math:pow(X,AlphaExponentCharacterizingDistribution)* Sum); % ceiling to get integers >=1
zipF(AlphaExponentCharacterizingDistribution, N, X, RunningSum)-> % N is total users and X is the rank of a random user
  Sum = math:pow(1/N,AlphaExponentCharacterizingDistribution) + RunningSum,
  zipF(AlphaExponentCharacterizingDistribution,N-1,X,Sum).

spinUpUsers(_,0,ListOfUserNames)->
  ListOfUserNames;
spinUpUsers(EnginePID,NumberOfUsers,ListOfUserNames)->
  RandomUsername = numberToString(NumberOfUsers), % create random username based off userNumber
  registerUser(EnginePID,RandomUsername),
  spinUpUsers(EnginePID,NumberOfUsers-1,[RandomUsername|ListOfUserNames]).

userActor(Username,UserFeed,TotalNumberOfUsers,EnginePID)-> % main userloop this will become the socket in part 2
  % random chance of sending a tweet
  %if totalNumMySubscribers <= rand(totalUsers). in simulate we assign users subscribers according to zipf
  % do:
    %if randomchance = 1 send tweet
    % else do nothing
  % else: nothing
  receive
    {tweet, Message}-> % received from engine % in part 2 these will be relayed over the socket if connected
      userActor(Username,[Message | UserFeed],TotalNumberOfUsers,EnginePID);
    {tweets, ListOfTweets}-> % received from engine as a result of following someone new WIP
      % should probably sort the new tweets into my feed maybe with a sorting actor?
      userActor(Username,[ListOfTweets|UserFeed],TotalNumberOfUsers,EnginePID)
  end.
%userActions
userSubscribeTo(EnginePID,UserOrHashTag,ID)-> % done
  EnginePID ! {subscribeTo,self(),UserOrHashTag,ID}.% UserOrHashTag = 'user' for user and 'hash' for hashTag

userSendTweet(EnginePID,UserName,Tweet)-> % done
  % tweets can be original or a retweet(gotten from userfeed) left for user to decide as engine see's no difference between them
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
startEngine(TotalNumberOfUsers)-> % returns enginePID % done
  UserDatabase = createDatabase(10),
  HashTagDatabase = createDatabase(5),
  spawn(twitterEngine,engineActor,[UserDatabase,HashTagDatabase,TotalNumberOfUsers]).
killEngine(EnginePID)-> % done
  EnginePID ! die.

engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers)->
  receive
    die -> % done
      userDatabaseKiller(UserDatabase), % special version that kills the userActors as well
      databaseKiller(HashTagDatabase),
      ok;
      % loop ends here don't call self
    {setupSim, ListOfUserNames,ZipfAlpha}-> % done
      %set each user up to have all its subs based on zipF distribution
      engineSimSetup(UserDatabase,ListOfUserNames,ListOfUserNames,ZipfAlpha,TotalNumberOfUsers,1),
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers);
    {registerUser,Username}-> % done
      PID =  spawn(twitterEngine,userActor,[Username,[],TotalNumberOfUsers,self()]), %TotalNumberOfUsers used for tweet simulation...
      insert(UserDatabase,Username, {[],[],PID}),%USERDATA = {User'sTweets,Subscribers(followers),UserActorPID}
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers);
    {logOn,Username,PID}-> % done
      {_,_,ActorPID} = query(UserDatabase,Username),
      PID ! ActorPID,
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers);
    {subscribeTo,UserPID,user,UserToSubscribeTo}-> % WIP
      %should i check if UserToSubscribeTo exists in DB? return null if not in DB
      {Tweets,Subs,SubActorPID} = query(UserDatabase,UserToSubscribeTo),
      insert(UserDatabase,UserToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID}),% update followers list should i check if already in list?
      %Send to user all tweets messed from before subed (user must organize if desired)
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers);
    {subscribeTo,UserPID,hash,HashToSubscribeTo}-> %WIP
      %should i check if HashToSubscribeTo exists in DB? return null if not in DB
      {Tweets,Subs,SubActorPID} = query(HashTagDatabase,HashToSubscribeTo),
      insert(HashTagDatabase,HashToSubscribeTo,{Tweets,[UserPID|Subs],SubActorPID}),% update followers list
      %Send to user all tweets messed from before subed (user must organize if desired)
      UserPID ! {tweets,Tweets},
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers);
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
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers);
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
      engineActor(UserDatabase,HashTagDatabase,TotalNumberOfUsers)
  end.
engineSimSetup(_,[],_,_,_,_)-> % set each user to have a num of subs based on zipf distribution
  ok;
engineSimSetup(UserDatabase,ListOfUsers,CompleteListOfUsers,ZipfAlpha,TotalNumberOfUsers,Rank)->
  NumberOfSubsUserShouldHave = zipF(ZipfAlpha,TotalNumberOfUsers,Rank,0),
  {ListOfSubs,_} = lists:split(NumberOfSubsUserShouldHave,CompleteListOfUsers), % returns first n number of users from
  % list they are now followers of this user
  {UserTweets,_,UserActorPID} = query(UserDatabase,hd(ListOfUsers)),
  insert(UserDatabase,hd(ListOfUsers),{UserTweets,ListOfSubs,UserActorPID}),
  engineSimSetup(UserDatabase,tl(ListOfUsers),CompleteListOfUsers,ZipfAlpha,TotalNumberOfUsers,Rank+1).

distributerActor([],_)-> % distributes tweets to actors in SendToList
  ok;
distributerActor(SendToList,Tweet)->
  hd(SendToList) ! {tweet,Tweet},
  distributerActor(tl(SendToList),Tweet).