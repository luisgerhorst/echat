echat
=====

Web chat JavaScript Framwork built with Erlang.

Create your own web chat interface with a stable and fast Erlang backend.

# Usage

Start the chat/file server. The file server will deliver all files in the `priv` directory.

## Load

Copy this into your chat's html file to load the required JavaScript frameworks.

```html
<script src="/jquery-2.0.3.min.js"></script>
<script src="/bullet.js"></script> <!-- backend magic delivers this file -->
<script src="/echat.js"></script>
```

## Message

```js
{
	username: 'Name',
	content: 'Message',
	timestamp: 1384541189624.348,
}
```

## Start

Open a new connection to the server:

```js
var chat = window.echat(function () {
	// connected
});
```

## Register

Before you can send messages or join a chat room you have to register youself with a username. Each username can only be used by one user at a time, when the user disconnects the username can be used by another user again.

```js
chat.username('My Name', function (accepted) {
	// if accepted == false the username is already taken
});
```

## Join

You first have to join a room before you can send/receive messages from it.

```js
chat.join('Room Name', 10, function (users, messages) { // 10: existing messages to load
	/*
	-> on successful join
	users: array of usernames that are members of the room, you included
	messages: existing messages requested
	*/
}, function (action, username, allUsers) {
	/*
	-> each time a user joins/leaves this room
	action: "join" or "leave"
	username: user performing the action
	allUsers: array of usernames of room members
	*/
}, function (username, content, timestamp, allMessages) {
	/*
	-> each time another user sends a message into this room
	timestamp: unix timestamp in ms, is a float
	allMessages: array of so far loaded messages of this room
	*/
});
```

## Leave

You won't receive messages of this room anymore.

```js
chat.leave('Room Name');
```

## Send Message

After having joined a room you can send messages to it.

```js
chat.room('Room Name').send('Your Message', function (timestamp) {
	// on successfully sent, timstamp is unix in ms of the message
});
```

## Load Messages

Load messages from the server that were sent before you joined the room, `limit` here set to `10` is the number of messages to load.

```js
chat.room('Room Name').load(10, function (messages, allMessages) { // 10: number of messages to load
	/*
	messages: 
});
```
