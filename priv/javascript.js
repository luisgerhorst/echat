$(document).ready(start);

function start() {
	
	window.bullet = $.bullet('ws://localhost:8080/bullet');
	
	// reactions
	
	window.bullet.onopen = function () {
		console.log('bullet: opened');
		if (window.user) window.user.reconnect();
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
	
	bullet.ondisconnect = function(){
		console.log('bullet: disconnected');
	};
	
	bullet.onclose = function(){
		console.log('bullet: closed');
	};
	
}

// actions

function send(type, data) {
	var json = JSON.stringify({
		type: type,
		data: data
	});
	console.log('sending', json);
	window.bullet.send(json);
}

function sendUser(id, nickname, rooms) {
	send('user', {
		id: id,
		nickname: nickname,
		rooms: rooms
	});
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

// reactions

function handle(type, data) {
	switch (type) {
		case 'old_messages':
			for (var i = data.length; i--;) {
				var name = data[i].room;
				var messages = data[i].messages;
				window.user.rooms[name].appendMessages(messages.reverse());
			}
			break;
		case 'new_message':
			// data is single extended message object
			window.user.rooms[data.room].appendMessages([data.message]);
			break;
		default:
			throw 'Unexpected type';
	}
}

// data

function enter(nickname) {
	window.user = new User(nickname);
	sendUser(window.user.id, window.user.nickname, []);
}

function User(nickname) {
	
	this.id = (new Date()).getTime();
	this.nickname = nickname;
	
	this.rooms = {};
	
	this.join = function (roomName) {
		sendJoin(roomName);
		this.rooms[roomName] = new Room(roomName);
	}
	
	this.leave = function (roomName) {
		sendLeave(roomName);
		this.rooms[roomName] = null;
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
	
}

function Room(name) {
	
	this.name = name;
	
	this.messages = [];
	
	this.latestMessageTimestamp = function () {
		return (this.messages.length > 0) ? this.messages[this.messages.length-1].timestamp : -1;
	};
	
	this.appendMessages = function (messages) { // insert messages ordered
		this.messages.push.apply(this.messages, messages);
		console.log('room', this.name, 'received messages', messages);
	};
	
	this.sendMessage = function (content) {
		sendMessage(this.name, content);
	};
	
}