Twitter



Anurag - See how I can implement sqlite with erlang. How can I implement register new user. 



Database design:
main functions:
1. query(which database actor has my user data) for user data location.(once you know where it is you can update or retreive the user data) 
2. add new user data entry to database
3 main parts:
1. Table actor(this is entry point to distributed database): This actor has a map that holds all database actor pids keyed on their unique hashed ID. (unique hash ID is used to figure out where twitter user is stored if userID hash <= actorID hash user is stored in that actor(Like chord stored in actor with greatest hashed ID not exceeding my userID hash value))
2. Database actor(a single node of the distributed database): This actor holds a map that holds twitter users infomrmation keyed on twitter userID.
3. User data = {userID, list of all my tweets, list of followers, list of who I follow} may change in future

Send/receive engine design:
main functions:

1.send out tweets to all users active and into that users database file

2.user login(retreive my home feed of twets from people I follow)
  2a. query all the data from people I follow
  2b. merge the tweets from people I follow into single list(home feed) based on tweet timestamp most recent on top
  
