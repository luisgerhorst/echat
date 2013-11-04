echat
=====

Web chat framework built with Erlang, [cowboy](https://github.com/extend/cowboy) and [bullet](https://github.com/extend/bullet).

# Usage

You can start the chat using `rebar compile && erl -pa ../echat/ebin deps/*/ebin`, it's very important that the parent folder has the name `echat`, then enter `application:start(echat).`. The default port is `8080` (open `localhost:8080` now). You can change the port in `src/echat_app.erl` line 2.

The client side is kind of framework, would be great if someone would build an interface for it.

1. Open your JavaScript console and type `chat('Your Name');` to start the chat.

2. Then use `chat.join('Room Name');` and `chat.leave('Room Name');` to join/leave a chat room. You can be member of multiple rooms at the same time.

3. After having joined a room use `chat.room('Room Name').send('Your Message');` to send a message into a room.

You can define a handler for new messages with `chat.onmessages(roomName, messagesArray)`. `messagesArray` is an array of messages such as 

```javascript
{
	content: 'Your Message',
	nickname: 'Sender Name',
	timestamp: 1383581585254873,
	userID: 1383581552494
}
```

`userID` is an integer that identifies a user (because there can be multiple users with the same nickname in a chat room). `timestamp` is the unix timestamp in ms at which the message was received by the server.

If you have an any questions/problems send me a [mail](mailto:luis@luisgerhorst.de).