$(document).ready(function () {
	
	// testing
	
	window.c = new Connection();
	
	c.onRegisterRes = function (username, accepted) {
		var acceptedString = accepted ? 'accepted' : 'not accepted';
		console.log('Username ' + username + ' was ' + acceptedString + '.');
	};
	
});

function Connection() {
	
	var Connection = this;
	
	// fake handlers
	
	Connection.onRegisterRes = function () {};
	
	Connection.onUsersChange = function () {};
	
	Connection.onMessages = function () {};
	
	Connection.onMessage = function () {};
	
	// handle
	
	function handle(type, data) {
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
	
	// actions
	
	var bullet = $.bullet('ws://localhost:8080/bullet');
	
	bullet.onopen = function () {
		console.log('bullet: opened');
	};
	
	bullet.ondisconnect = function () {
		console.log('bullet: disconnected');
	};
	
	bullet.onclose = function () {
		console.log('bullet: closed');
	};
	
	bullet.onmessage = function (event) {
		try {
			console.log('received', event.data);
			var object = JSON.parse(event.data);
			handle(object.type, object.data);
		} catch (error) {
			console.error('eChat: Unable to handle invalid message', event.data, 'because of error', error);
		}
	};
	
	function send(type, data) {
		var json = JSON.stringify({
			type: type,
			data: data
		});
		// console.log('sending', json);
		bullet.send(json);
	}
	
	Connection.register = function (username) {
		send('register', username);
	};
	
	Connection.join = function (room) {
		send('join', room);
	};
	
	Connection.leave = function (room) {
		send('leave', room);
	};
	
	Connection.sendMessage = function (room, content) {
		send('message', {
			room: room,
			content: content
		});
	};
	
	Connection.loadMessagesBefore = function (room, timestamp, limit) {
		send('messages_before', {
			room: room,
			timestamp: timestamp,
			limit: limit
		});
	};
	
	Connection.loadMessagesBetween = function (room, startTimestamp, endTimestamp) {
		send('messages_between', {
			room: room,
			startTimestamp: startTimestamp,
			endTimestamp: endTimestamp
		});
	};
	
}

/*window.chat = login;

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