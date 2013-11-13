(function () {
	
$(document).ready(function () {
	
	var chat = new Chat(function () { // on ready
		// allow user to register/show interface
	});
	
	chat.username('mark', function (accepted) { // username, on res
		// accepted?
	});
	
	chat.join('lobby', 5, function (users, messages) { // room name, number of old messages to load, on messages & users received
		// joined, here are the users
	}, function (action, username) { // on users change
		// each time users change
		// go threw interface html and remove user with matching username
	}, function (username, content, timestamp) { // on new message
		// new message
	});
	
	chat.room('lobby').send('Hi', function (timestamp) { // content, on success and res from server
		// sent & received, show in interface
	});
	
	chat.room('lobby').load(10, function (messages) { // number of old messages to load, on receive
		// messages requested
	});
	
	chat.leave('lobby');
	
});

function Chat(onReady) {
	
	var Chat = this;
	
	var onEvent = {},
		rooms = {};
	
	var bullet = $.bullet('ws://localhost:8080/bullet');
	
	bullet.onopen = function () {
		console.log('bullet: opened');
		onReady();
	};
	
	bullet.onmessage = function (event) {
		
		try {
			
			//console.log('received', event.data);
			var object = JSON.parse(event.data);
			
			var type = object.type,
				data = object.data,
				ref = object.ref;
				
			switch (type) {
				
				case 'register_res': // data = { username, accepted }
					onEvent[ref](data.accepted);
					onEvent[ref] = null;
					break;
					
				case 'users': // data = { room, users: [username], action: 'join'|'leave', username} }
					if (onEvent[ref]) { // after join, callabck set in Chat.join
						onEvent[ref](data.users);
						onEvent[ref] = null;
					} else {
						Chat.room(data.room).onUser(data.action, data.username, data.users);
					}
					break;
					
				case 'messages': // data = { room, messages: [{ username, content, timestamp }] }
					onEvent[ref](data.messages); // defined by Chat.room(name).load(number) or Chat.join(name, number, ...)
					onEvent[ref] = null;
					break;
					
				case 'message': // data = { room, message: { username, content, timestamp } }
					if (onEvent[ref]) { // after join, callabck set in Chat.join
						onEvent[ref](data.message.timestamp);
						onEvent[ref] = null;
					} else {
						Chat.room(data.room).onMessage(data.message.username, data.message.content, data.message.timestamp);
					}
					break;
					
				default:
					throw 'unexpected type';
					
			}
				
		} catch (error) {
			console.error('eChat: Unable to handle message', event.data, 'because of error', error);
		}
		
	};
	
	// send
	
	function newRef() {
		return Math.floor( Math.random() * Math.pow(2, 32) );
	}
	
	function send(type, data) {
			
		var ref = newRef();
		var json = JSON.stringify({
				type: type,
				data: data,
				ref: ref
		});
		
		bullet.send(json);
		return ref;
			
	}
	
	// api
	
	Chat.username = function (name, callback) {
		var ref = send('register', username);
		onEvent[ref] = callback;
	};
	
	Chat.join = function (room, messagesToLoad, onJoined, onUser, onMessage) {
		
		var usersRef = send('join', room); // will return as 'users'
		var messagesRef = send('messages_before', { // will return as 'messages'
			room: room,
			timestamp: Date.now(),
			limit: toLoad
		}, id);
		
		var toReceive = 2,
			users = null,
			messages = null;
		
		onEvent[usersRef] = function (newUsers) {
			users = newUsers;
			chunkReceived();
		};
		
		onEvent[messagesRef] = function (newMessages) {
			messages = newMessages;
			chunkReceived();
		};
		
		function chunkReceived() {
			toReceive--;
			if (!toReceive) onJoined(users, messages);
		}
		
		rooms[room] = new Room(room, onUser, onMessage);
		
	};
	
	// leave
	
	// Room
	
}

})();