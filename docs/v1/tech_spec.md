# Scope

This spec spells out the technical specifications of the following features.

- logging in
- creating a chat room
- home page that displays a list of the top ten most popular chats wo
- participating in chat

# Logging In

A users logs into taplike.com via Twitter.

## Backend

A user logs into taplike.com via the /login url. This redirects a user to https://api.twitter.com/oauth/authorize which requests authorization to

- Read Tweets from your timeline.
- See who you follow, and follow new people.
- Update your profile.
- Post Tweets for you.

Upon successful authorization, we execute a query for the user with the twitter id of the authenticating user. If no such user exists the table, a new User  row is persisted to the “taplike_users” table. The schema for this table, in Yesod model style is:

```haskell
User json sql=taplike_users
    twitterUserId Int64
    twitterScreenName Text
    profileImageUrl Text
    emailAddress Text Maybe
    Primary twitterUserId
    UniqueUser twitterUserId
```

## Frontend

If a user is logged in a session value (the twitter user id) is present in all request headers. The logged-in status of a taplike user can be detected by looking for this value. This value is exposed to the client side (JS, HTML, etc) via a variable declared in a Yesod Handler.


# Creating a chat

An authenticated user can create a chat room.

The POST /newchat endpoint requires a JSON body, consisting of the following object:

```json
{title: String, description: String}
```

A success response consists of an HTTP 201 status and a json object containing the newly created room id as well as the room object itself:

```json
{"id":13,"chat_room":{"createdBy":174050963,"title":"haha","description":"lol"}}
```

If a user is not authenticated (because they’re not “logged in”) a 401 status is returned with the text “UNAUTHORIZED” as the body of the response.

## Frontend

After a successful room creation, the user should be directed to the newly created room. The `id` returned in the http response can be used to construct a type safe url to redirect with.

After redirecting to the new room, a modal should appear prompting the user to invite others to the new room. The modal will include a copyable link, a button to tweet and a button to share on FB. Both Twitter and FB buttons will pop open a new window with standard twitter and FB sharing functionality. The presence of a query param ?invite=true will be included in the url a user is redirected to after creating a room. This presence of this param
