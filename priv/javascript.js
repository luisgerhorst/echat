(function () {
	
$(document).ready(function () {
	var chat = window.echat = new Chat();
	
	// test
	
	chat.ready(function () {
		
		console.log('opened');
		
		chat.username('mark', function (accepted) {
			console.log('callback', 'mark', accepted);
		});
		
		chat.join('lobby');
		
	});
	
});
	
function Chat() {
	
	var Chat = this;
	
	var bullet = $.bullet('ws://' + window.document.location.host + '/bullet');
	
	var firstOpen = true;
	
	bullet.onopen = function () {
		// console.log('bullet: opened');
		if (firstOpen) {
			firstOpen = false;
			Chat.ready();
		} else Chat.reconnect();
	};
	
	bullet.onmessage = function (event) {
		try {
			// console.log('received', event.data);
			var object = JSON.parse(event.data);
			handle(object.type, object.data, object.ref);
		} catch (error) {
			console.error('eChat: Unable to handle invalid message', event.data, 'because of error', error);
		}
	};
	
	var requestHandlers = {};
	
	function send(type, data) {
		
		var ref = newRef();
		var json = JSON.stringify({
			type: type,
			data: data,
			ref: ref
		});
		
		bullet.send(json);
		return ref;
		
		function newRef() {
			return Math.floor( Math.random() * Math.pow(2, 32) );
		}
		
	}
	
	function handle(type, data, ref) {
		switch (type) {
			case 'register_res':
				// data = { username, accepted }
				requestHandlers[ref](data.accepted);
				requestHandlers[ref] = null;
				break;
			case 'users':
				// data = { room, users: [username], action: 'join'|'leave', username} }
				if (requestHandlers[ref]) { // after join
					requestHandlers[ref](data.users);
					requestHandlers[ref] = null;
				} else {
					Chat.room(data.room).usersChange(data.action, data.username, data.users);
				}
				break;
			case 'messages':
				// data = { room, messages: [{ username, content, timestamp }] }
				
				break;
			case 'message':
				// data = { room, message: { username, content, timestamp } }
				
				break;
			default:
				throw 'unexpected type';
		}
	}
	
	// start
	
	Chat.ready = function (callback) {
		Chat.ready = callback;
	};
	
	// register
	
	Chat.username = function (username, callback) {
		if (!expectedRegisterRes) {
			var ref = send('register', username);
			onRes[ref] = callback;
		}
	};
	
	// rooms
	
	var rooms = {};
	
	// add
	
	Chat.join = function (name) {
		if (rooms[name]) throw 'already member';
		connection.join(name);
		rooms[room] = new Room(name);
	};
	
	// get
	
	Chat.room = function (name) {
		if (!rooms[name]) throw 'no member';
		return rooms[name];
	}
	
	// remove
	
	Chat.leave = function (name) {
		if (!rooms[name]) throw 'no member';
		rooms[name] = null;
	}
	
	// constructor
	
	function Room(name) {
	
		var Room = this;
	
		Room.send = function (content, callback) {
			connection.sendMessage(name, content);
		}
	
	}
	
}

function Connection() {
	
	var Connection = this;
	
	// fake handlers
	
	Connection.ready = function () {};
	Connection.reconnect = function () {};
	Connection.onRegisterRes = function () {};
	Connection.onUsersChange = function () {};
	Connection.onMessages = function () {};
	Connection.onMessage = function () {};
	
	// handle
	
	function handle(type, data, ref) {
		switch (type) {
			case 'register_res':
				// data = { username, accepted }
				Connection.onRegisterRes(data.username, data.accepted); // username, accepted
				break;
			case 'users':
				// data = { room, users: [username], action: 'join'|'leave', username} }
				Connection.onUsersChange(data.room, data.users, data.action, data.username); // room, users, action, username
				break;
			case 'messages':
				// data = { room, messages: [{ username, content, timestamp }] }
				Connection.onMessages(data.room, data.messages); // room, messages
				break;
			case 'message':
				// data = { room, message: { username, content, timestamp } }
				Connection.onMessage(data.room, data.message.username, data.message.content, data.message.timestamp); // room, username, content, timestamp
				break;
			default:
				throw 'unexpected type';
		}
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
		
		function newRef() {
			return Math.floor( Math.random() * Math.pow(2, 32) );
		}
		
	}
	
	Connection.register = function (username, res) {
		var ref = send('register', username, id);
		// add res
	};
	
	Connection.join = function (room) {
		send('join', room); // also id?
	};
	
	Connection.leave = function (room) {
		send('leave', room);
	};
	
	Connection.sendMessage = function (room, content) {
		var id = id();
		send('message', {
			room: room,
			content: content
		}, id);
	};
	
	Connection.loadMessagesBefore = function (room, timestamp, limit) {
		var id = id();
		send('messages_before', {
			room: room,
			timestamp: timestamp,
			limit: limit
		}, id);
	};
	
	Connection.loadMessagesBetween = function (room, startTimestamp, endTimestamp) {
		send('messages_between', {
			room: room,
			startTimestamp: startTimestamp,
			endTimestamp: endTimestamp
		});
	};
	
	// actions
	
	var bullet = $.bullet('ws://' + window.document.location.host + '/bullet');
	
	/*var firstOpen = true;
	
	bullet.onopen = function () {
		// console.log('bullet: opened');
		if (firstOpen) {
			firstOpen = false;
			Connection.ready();
		} else Connection.reconnect();
	};
	
	bullet.ondisconnect = function () {
		console.log('bullet: disconnected');
	};
	
	bullet.onclose = function () {
		console.log('bullet: closed');
	};*/
	
	bullet.onmessage = function (event) {
		try {
			// console.log('received', event.data);
			var object = JSON.parse(event.data);
			handle(object.type, object.data, object.ref);
		} catch (error) {
			console.error('eChat: Unable to handle invalid message', event.data, 'because of error', error);
		}
	};
	
}

/*

window.c = new Connection();

c.ready = function () {
	c.register('Luis');
	c.join('lobby');
	c.sendMessage('lobby', 'Hi');
};

c.reconnect = function () {
	c.register('Luis');
	c.join('lobby');
	c.sendMessage('lobby', 'I\'m back');
};

c.onRegisterRes = function (username, accepted) {
	var acceptedString = accepted ? 'accepted' : 'not accepted';
	console.log('Username "' + username + '" was ' + acceptedString + '.');
};

c.onMessage = function (room, username, content, timestamp) {
	console.log('Message from "' + username + '" with content "' + content + '" in room "' + room + '" received at ' + timestamp);
};

window.chat = login;

function login(nickname) {
	window.chat = new User(nickname);
	sendUser(window.chat.id, window.chat.nickname, []);
}

function User(nickname) {
	
	this.id = (new Date()).getTime();
	this.nickname = nickname;
	
	this.rooms = {};
	
	this.join = function (roomName) {
		roomName = roomName.toLowerCase();
		sendJoin(roomName);
		this.rooms[roomName] = new Room(roomName);
	}
	
	this.leave = function (roomName) {
		roomName = roomName.toLowerCase();
		sendLeave(roomName);
		this.rooms[roomName] = null;
	}
	
	this.room = function (roomName) {
		roomName = roomName.toLowerCase();
		return this.rooms[roomName];
	}
	
	this.reconnect = function () {
		var roomInfos = [];
		for (var name in this.rooms) {
			var room = this.rooms[name];
			roomInfos.push({ 
				name: room.name,
				latest: room.latestMessageTimestamp()
			});
		}
		sendUser(this.id, this.nickname, roomInfos);
	}
	
	this.onmessages = function (Func) {
		this.onmessagesCallback = Func;
	};
	
}

function Room(name) {
	
	this.name = name;
	
	// this.messages = [];
	
	this.latestMessageTimestamp = function () {
		return (this.messages.length > 0) ? this.messages[this.messages.length-1].timestamp : -1;
	};
	
	this.append = function (messages) {
		// this.messages.push.apply(this.messages, messages);
		console.log('Room "'+this.name+'" received messages', messages);
		if (window.chat.onmessagesCallback) window.chat.onmessagesCallback(this.name, messages);
	};
	
	this.send = function (content) {
		sendMessage(this.name, content);
	}
	
}*/

})();