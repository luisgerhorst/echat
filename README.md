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
	// when it's true you are ready to join a room
});
```

## Join

You first have to join a room before you can send/receive messages of the room.

```js
chat.join('Room Name', 10, function (users, messages) { // 10 = Existing messages to load
	// successfully joined
	// users: array of usernames that are members of the room, you included
	// messages: existing messages requested
}, function (action, username, allUsers) {
	// called when user leaves/joins the room
	// action is "join" or "leave"
	// username is the user that performs the action
	// allUsers is an array of all users that are now in the room
}, function (username, content, timestamp, allMessages) {
	// called each time another users sends a message into this room
	
});
```