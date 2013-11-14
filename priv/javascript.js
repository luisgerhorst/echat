(function () {
	
$(document).ready(function () {
	
	window.chat = new Chat(function () { // on ready
		// allow user to register/show interface
		
		chat.username('mark', function (accepted) { // username, on res
			// accepted?
			
			console.info('username mark', accepted);
			
		});
		
		chat.join('lobby', 5, function (users, messages) { // room name, number of old messages to load, on messages & users received
			// joined, here are the users
			
			console.info('joined', users, messages);
			
		}, function (action, username) { // on users change
			// each time users change
			// go threw interface html and remove user with matching username
			console.info('user lobby', action, username);
		}, function (username, content, timestamp) { // on new message
			// new message
			console.info('message lobby', username, content, timestamp);
		});
		
		chat.room('lobby').send('Hi', function (timestamp) { // content, on success and res from server
			// sent & received, show in interface
			console.info('mesage res', timestamp);
		});
		
		chat.room('lobby').load(10, function (messages) { // number of old messages to load, on receive
			// messages requested
			console.info('load messages', messages);
		});
		
		chat.leave('lobby');
		
	});
	
});

function Chat(onReady) {
	
	var Chat = this;
	
	var onEvent = {},
		rooms = {},
		username = null;
	
	// via http://stackoverflow.com/questions/7837456/comparing-two-arrays-in-javascript
	// attach the .compare method to Array's prototype to call it on any array
	Array.prototype.compare = function (array) {
		// if the other array is a falsy value, return
		if (!array)
			return false;
	
		// compare lengths - can save a lot of time
		if (this.length != array.length)
			return false;
	
		for (var i = 0; i < this.length; i++) {
			// Check if we have nested arrays
			if (this[i] instanceof Array && array[i] instanceof Array) {
				// recurse into the nested arrays
				if (!this[i].compare(array[i]))
					return false;
			}
			else if (this[i] != array[i]) {
				// Warning - two different object instances will never be equal: {x:20} != {x:20}
				return false;
			}
		}
		return true;
	};
	
	var bullet = $.bullet('ws://localhost:8080/bullet');
	
	(function () {
		
		var wasRead = false;
		
		bullet.onopen = function () {
			console.log('bullet: opened');
			if (!wasRead) {
				onReady();
				wasRead = true;
			} else { // reconnect
				try {
					
					Chat.username(username, function (accepted) {
						if (!accepted) throw 'username not available';
					});
					
					var reconnectTimestamp = Date.now(); // unix in ms for messages between
					for (var name in rooms) rooms[name].reconnect(reconnectTimestamp);
					
				} catch (error) {
					console.error('eChat: Unable to reconnect because or error', error);
				}
			}
		};
		
	})();
	
	bullet.onmessage = function (event) {
		
		try {
			
			console.log('received', event.data);
			var object = JSON.parse(event.data);
			
			var type = object.type,
				data = object.data,
				ref = object.ref;
				
			switch (type) {
				
				case 'register_res': // data = accepted
					onEvent[ref](data);
					onEvent[ref] = null;
					break;
					
				case 'users': // data = [username]
					onEvent[ref](data);
					onEvent[ref] = null;
					break;
					
				case 'messages': // data = [{ username, content, timestamp }]
					onEvent[ref](data);
					onEvent[ref] = null;
					break;
					
				case 'message_timestamp': // data = timestamp
					onEvent[ref](data);
					onEvent[ref] = null;
					break;
					
				case 'user': // data = { room, action: 'join'|'leave', username }
					rooms[data.room].onUser(data.action, data.username);
					break;
					
				case 'message': // data = { room, message: { username, content, timestamp } }
					rooms[data.room].onMessage(data.message.username, data.message.content, data.message.timestamp);
					break;
					
				default:
					throw 'invalid type';
					
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
		
		console.info('sending', json);
		
		bullet.send(json);
		return ref;
			
	}
	
	// api
	
	this.username = function (name, callback) {
		var ref = send('register', name);
		onEvent[ref] = function (accepted) {
			if (accepted) username = name;
			callback(accepted);
		};
	};
	
	this.join = function (room, messagesToLoad, onJoined, onUser, onMessage) {
		rooms[room] = new Room(room, messagesToLoad, onJoined, onUser, onMessage);
	};
	
	this.room = function (name) {
		return rooms[name];
	};
	
	this.leave = function (room) {
		send('leave', room);
		rooms[room] = null;
	};
	
	function Room(room, messagesToLoad, onJoinedCallback, onUserCallback, onMessageCallback) {
		
		var Room = this;
		
		var messages = [],
		    users = [];
		
		(function (room, limit, callback) {
		
			var usersRef = send('join', room); // will return as 'users'
			var messagesRef = send('messages_before', { // will return as 'messages'
				room: room,
				timestamp: Date.now(),
				limit: limit
			});
			
			// console.log('join refs', usersRef, messagesRef);
			
			var toReceive = 2,
				usersReceived = null,
				messagesReceived = null;
			
			onEvent[usersRef] = function (newUsers) {
				usersReceived = newUsers;
				chunkReceived();
			};
			
			onEvent[messagesRef] = function (newMessages) {
				messagesReceived = newMessages;
				chunkReceived();
			};
			
			// console.log('join handlers defined', onEvent);
			
			function chunkReceived() {
				toReceive--;
				if (!toReceive) {
					
					messages.push(messagesReceived);
					users.push(usersReceived);
					
					callback(usersReceived, messagesReceived);
					
				}
			}
		
		})(room, messagesToLoad, onJoinedCallback);
		
		// api
		
		this.send = function (content, callback) {
			
			var ref = send('message', {
					room: room,
					content: content
			});
			
			onEvent[ref] = function (timestamp) {
				
				messages.push({
					username: username,
					content: content,
					timestamp: timestamp
				});
				
				callback(timestamp);
				
			};
			
		};
		
		this.load = function (limit, callback) {
			
			console.log('load', messages);
			
			var ref = send('messages_before', {
					room: room,
					timestamp: messages[0].timestamp,
					limit: limit
			});
			
			onEvent[ref] = function (receivedMessages) {
				messages.push(receivedMessages);
				callback(receivedMessages);
			};
			
		};
		
		// event callbacks
		
		this.reconnect = function (reconnectTimestamp) {
			
			var usersRef = send('join', room);
			var messagesRef = send('messages_between', {
				room: room,
				startTimestamp: messages[messages.length-1].timestamp,
				endTimestamp: reconnectTimestamp
			});
			
			onEvent[usersRef] = function (newUsers) {
				// you can fix this, just call onUser for everything that changed. but it's complicated
				if (!users.compare(newUsers)) throw 'users have changed';
			};
			
			onEvent[messagesRef] = function (newMessages) {
				for (var i = 0; i < newMessages.length; i++) { // don't reverse
					var message = newMessages[i];
					Room.onMessage(message.username, message.content, message.timestamp);
				}
			};
			
		};
		
		this.onUser = function (action, performingUsername) {
			
			switch (action) {
			
				case 'join':
					users.push(performingUsername);
					break;
					
				case 'leave':
					for (var i = users.length; i--;) {
						if (users[i] === performingUsername) {
							users.splice(i, 1);
							i = 0;
						}
					}
					break;
					
				default:
					throw 'invalid action';
			
			}
			
			onUserCallback(action, performingUsername, users);
			
		};
		
		this.onMessage = function (username, content, timestamp) {
			
			messages.push({
				username: username,
				content: content,
				timestamp: timestamp
			});
			
			onMessageCallback(username, content, timestamp);
			
		};
		
	}
	
}

})();