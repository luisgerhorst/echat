$(document).ready(start);

function start() {
	
	window.bullet = $.bullet('ws://localhost:8080/bullet');
	
	// reactions
	
	window.bullet.onopen = function () {
		var latestMessageTimestamp = (window.messages.length > 0) ? window.messages[window.messages.length-1].timestamp : -1;
		send('messages?', latestMessageTimestamp); // ask for new messages since disconnect
	};
	
	window.bullet.ondisconnect = function () {
		console.log('bullet: disconnected');
	};
	
	window.bullet.onclose = function () {
		console.log('bullet: closed');
	};
	
	window.bullet.onmessage = function (event) {
		try {
			// console.log('received', event.data);
			var object = JSON.parse(event.data);
			handle(object.type, object.data);
		} catch (error) {
			console.log('Invalid message', event.data, 'caused error', error);
		}
	};
	
	window.bullet.onheartbeat = function () {
		
	};
	
}

function test() {
	sendMessage('jo');
}

// data

window.messages = [];

window.userID = (new Date()).getTime();

function setUsername(username) {
	window.username = username;
}

function appendMessages(messages) {
	window.messages.push.apply(window.messages, messages);
	console.log('received messages', messages);
}

// actions

function send(type, data) { // type: string
	var json = JSON.stringify({
		type: type,
		data: data
	});
	// console.log('sending', json);
	window.bullet.send(json);
}

// actions - shorthands

function sendMessage(content) {
	send('message', {
		content: content,
		userID: window.userID,
		username: (window.username) ? window.username : 'Unknown'
	});
}

// reactions

function handle(type, data) { // type: string
	switch (type) {
		case 'messages':
			// data is array of extended message objects, newest first
			appendMessages(data.reverse());
			break;
		case 'message':
			// data is single extended message object
			appendMessages([data]);
			break;
		default:
			throw 'Unexpected type';
	}
}