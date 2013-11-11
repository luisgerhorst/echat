// tests

function testRooms(id) {
	id = (id === undefined) ? '' : ' ' + id;
	sendRegister('Luis' + id);
	setTimeout(function () {
		var r = 'Lobby #';
		sendJoin(r);
		setTimeout(function () {
			sendMessage(r, 'Hi');
			setTimeout(function () {
				sendLeave(r);
				setTimeout(function () {
					sendJoin(r);
				}, 200);
			}, 200);
		}, 200);
	}, 200);
}

function testFastReconnect() {
	console.log('register & join');
	sendRegister('Luis');
	sendJoin('lobby');
}

// end tests



$(document).ready(start);

function start() {
	
	window.bullet = $.bullet('ws://localhost:8080/bullet');
	
	// reactions
	
	window.bullet.onopen = function () {
		console.log('bullet opened');
		if (window.chat.reconnect) window.chat.reconnect();
		testFastReconnect();
	};
	
	window.bullet.onmessage = function (event) {
		try {
			console.log('received', event.data);
			var object = JSON.parse(event.data);
			handle(object.type, object.data);
		} catch (error) {
			console.log('Unable to handle invalid message', event.data, 'error', error);
		}
	};
	
}

// actions

function send(type, data) {
	var json = JSON.stringify({
		type: type,
		data: data
	});
	//console.log('sending', json);
	window.bullet.send(json);
}

function sendRegister(username) {
	send('register', username);
}

function sendJoin(room) {
	send('join', room);
}

function sendLeave(room) {
	send('leave', room);
}

function sendMessage(room, content) {
	send('message', {
		room: room,
		content: content
	});
}

function sendLoadBefore(room, timestamp, limit) {
	send('messages_before', {
		room: room,
		timestamp: timestamp,
		limit: limit
	});
}

function sendLoadBetween(room, startTimestamp, endTimestamp) {
	send('messages_between', {
		room: room,
		startTimestamp: startTimestamp,
		endTimestamp: endTimestamp
	});
}

// reactions

function handle(type, data) {
	switch (type) {
		case 'register_res':
			// { username, accepted }
			// tell user if username accepted
			// if accepted start chat
			break;
		case 'users':
			// { room, users: [username], action: 'join'|'leave', username} }
			// pass to handler for user changes
			break;
		case 'messages':
			// { room, messages: [{ username, content, timestamp }] }
			// pass messages to special handler that inserts on right position
			break;
		case 'message':
			// { room, message: { username, content, timestamp } }
			// pass message to hanlder that appends (maybe also sorts)
			break;
		default:
			throw 'unexpected type';
	}
}

// data

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
	
}