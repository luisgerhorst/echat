echat
=====

Create your own web chat interface with a stable and fast Erlang backend and an easy to use JavaScript library.

[![Build Status](https://travis-ci.org/luisgerhorst/echat.png?branch=master)](https://travis-ci.org/luisgerhorst/echat)

# Usage

## Start

To compile and run the app you need `rebar` and `erl`.

1. Load dependencies and compile with `rebar get-deps compile`
3. Open the erlang shell with `erl -pa ebin deps/*/ebin -env ERL_LIBS .`
4. Start with `echat:start().` and stop with `echat:stop().`

The app has a built-in file server that delivers all files in the `priv` directory, default port is `8222` (can be changed in `src/echat.app.src:8`, recompile afterwards).

## HTML

Copy this into your html file to load the required JavaScript frameworks.

```html
<script src="/jquery-2.0.3.min.js"></script>
<script src="/bullet.js"></script> <!-- backend magic delivers this file -->
<script src="/echat.js"></script>
```

## Message Object

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
	// if accepted is false the username is already taken, choose another one
});
```

## Join

You first have to join a room before you can send/receive messages from it.

`10` is the number of messages to load from this room that were sent befor you joined.

The third parameter is a callback function called after receiving the response to the join request. It receives the users and an array of the requested messages of the room.

The next callback funtion is called everytime a user joins/leaves the room. It also gets an array with all usernames that are members of the room now.

The last argument is a callback function to be called when another user sends a message into this room. `allMessages` is an array of [message objects](#message-object).

```js
chat.join('Room Name', 10, function (users, messages) {
	// joined
}, function (action, username, allUsers) {
	// user joined/left
}, function (username, content, timestamp, allMessages) {
	// new message
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
	// success
});
```

## Load Messages

Load messages from the server that were sent before you joined the room, `limit` here set to `10` is the number of messages to load. `messages` is an array of [message objects](#message-object) received. `allMessages` are all messages in this room received so far.

```js
chat.room('Room Name').load(10, function (messages, allMessages) { // 10: number of messages to load
	// loaded
});
```

# Support

If you need help send me a [mail](mailto:luis@luisgerhorst.de).

# Todo

- Easy install & run with *relx*
- Example Interface
