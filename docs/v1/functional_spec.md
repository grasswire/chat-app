# Functional Spec - V1

# Scenario 1 - Wants to chat about hip hop

Levi wants to chat with people about hiphop. He finds a link to taplike.com, clicks it and he sees a page which describes taplike as a place where anyone can chat with other people about things they’re interested in.

In the product description, Levi sees a link for that lets him “start a chat now”. He clicks the link, which takes him to a twitter login page. He clicks it and authorizes taplike. He’s redirected back to taplike and sees a form asking him for a title and topic.

He puts “hip hop” as the “title” and “all things hip hop” in the “description”. He clicks “go” and is directed to a new chat room. On the left he sees his profile pic as one of the members currently in the room. He’s the only person there and modal pops up prompting Levi to invite other people to chat. It says “It’s lonely in here. Invite other people to chat”. There’s a link he can copy, a button to tweet about the chat and a button to post to Facebook.

Levi clicks to tweet. When he clicks the button he sees the message that will be sent. There’s a button to send this tweet. He clicks it and sees a tweet compose window open up. The tweet says ““Come chat about “all things hip hop” on taplike at taplike.com/room/hip-hop”. Levi clicks to send that tweet.

- Need a modal sketch for the sharing function


# Scenario 2 - Joe joining a chat from a tweet link

Joe sees a tweet that Levi posted about a hip hop chat. Curious, he clicks the link and is taken to the chatroom. This is Joe’s first time visiting taplike.com. A modal pops up prompting Joe to sign in with his twitter account. He logs in and directed back to the chatroom. A message appears in the chat room which says “joecianflione joined the chat!”

Joe says “hey” and starts chatting with Levi.

Next to every message Joe and Levi send, there’s a heart icon. Levi sends a message containing a link to an awesome track he wants Joe to listen to. Joe clicks the heart icon. He notices the message he liked show up [somewhere on the page]. It’s a copy of the message along with a label that says “1 like”.


# Scenario 3 - Bored person browsing

Lisa is bored at work. She sees someone tweet a link to taplike.com. She clicks it and sees a description of taplike and a list of 10 chat rooms. Each chat is represented as a tile with a title and a description of what’s currently being discussed. She likes hip hop. A lot. She sees the hip hop room and clicks to enter. On entering, she sees people chatting, a list of most liked messages. The login modal displays, prompting her to log in. She clicks to login and is redirected back to the chat. Lisa gets really into the chat and starts looking at the most liked messages. She really likes one of the messages from earlier. So she clicks to like it. When she likes it, she sees that message move up to the top and the label changes from “1 like” to “2 likes”.

# Non Goals

This version will not support the following features:

- unfurling links and media
- tracking all the rooms a user is participating in and allowing them to easily jump between them
- persisting all messages (note that liked messages will be persisted so that they can be displayed and organized in the “most liked” section).
