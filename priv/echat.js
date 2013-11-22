(function () {
	
window.echat = function (onReady) {
	
	return new Chat(onReady);
	
};

function Chat(onReady) {
	
	var Chat = this;
	
	var onEvent = {},
	    rooms = {},
	    username = null;
	
	var bullet = $.bullet('ws://' + window.document.location.host + '/bullet');
	
	(function () {
		
		var wasRead = false;
		
		bullet.onopen = function () {
			// console.log('bullet: opened');
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
			
			// console.log('received', event.data);
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
		
		// console.info('sending', json);
		
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
		
		Room.messages = [];
		Room.users = [];
		
		(function (room, limit, callback) {
		
			var usersRef = send('join', room); // will return as 'Room.users'
			var messagesRef = send('messages_before', { // will return as 'Room.messages'
				room: room,
				timestamp: Date.now(),
				limit: limit
			});
			
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
			
			function chunkReceived() {
				toReceive--;
				if (!toReceive) {
					
					Room.messages = messagesReceived;
					Room.users = usersReceived;
					
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
				
				Room.messages.push({
					username: username,
					content: content,
					timestamp: timestamp
				});
				
				callback(timestamp);
				
			};
			
		};
		
		this.load = function (limit, callback) {
			
			var ref = send('messages_before', {
					room: room,
					timestamp: Room.messages[0].timestamp,
					limit: limit
			});
			
			onEvent[ref] = function (receivedMessages) {
				Room.messages = receivedMessages.concat(Room.messages);
				console.log('load:', Room.messages, receivedMessages);
				callback(receivedMessages, Room.messages);
			};
			
		};
		
		// event callbacks
		
		this.reconnect = function (reconnectTimestamp) {
			
			var usersRef = send('join', room);
			
			onEvent[usersRef] = function (newUsers) { // detects waht users joined/left the room while disconenct
				
				var n = newUsers,
				    o = Room.users;
				    
				for (var i = o.length; i--;) { // go threw old
					var u = o[i];
					if (n.indexOf(u) === -1) { // user doesn't exist anymore -> left
						Room.onUser('leave', u);
					}
				}
				    
				for (var i = n.length; i--;) { // go threw new
					var u = n[i];
					if (o.indexOf(n[i]) === -1) { // user didn't exist before -> joined
						Room.onUser('join', u);
					}
				}
				
			};
			
			if (Room.messages.length) { // if messages already loaded
				
				var messagesRef = send('messages_between', {
					room: room,
					startTimestamp: Room.messages[Room.messages.length-1].timestamp,
					endTimestamp: reconnectTimestamp
				});
				
				onEvent[messagesRef] = function (newMessages) {
					Room.messages = Room.messages.concat(newMessages);
					for (var i = 0; i < newMessages.length; i++) { // don't reverse
						var message = newMessages[i];
						Room.onMessage(message.username, message.content, message.timestamp);
					}
				};
				
			}
			
		};
		
		this.onUser = function (action, performingUsername) {
			
			switch (action) {
			
				case 'join':
					Room.users.push(performingUsername);
					break;
					
				case 'leave':
					for (var i = Room.users.length; i--;) {
						if (Room.users[i] === performingUsername) {
							Room.users.splice(i, 1);
							i = 0;
						}
					}
					break;
					
				default:
					throw 'invalid action';
			
			}
			
			onUserCallback(action, performingUsername, Room.users);
			
		};
		
		this.onMessage = function (username, content, timestamp) {
			
			Room.messages.push({
				username: username,
				content: content,
				timestamp: timestamp
			});
			
			onMessageCallback(username, content, timestamp, Room.messages);
			
		};
		
	}
	
}

})();