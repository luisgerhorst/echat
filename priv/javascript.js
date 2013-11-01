$(document).ready(start);

function start() {
	
	window.bullet = $.bullet('ws://localhost:8080/bullet');
	
	// reactions
	
	window.bullet.onopen = function () {
		console.log('bullet: opened');
		test(); // disable for production
	};
	
	window.bullet.ondisconnect = function () {
		console.log('bullet: disconnected');
	};
	
	window.bullet.onclose = function () {
		console.log('bullet: closed');
	};
	
	window.bullet.onmessage = function (event) {
		try {
			console.log(event.data);
			var array = JSON.parse(event.data);
			var type = array.shift();
			var data = array;
			handle(type, data);
		} catch (error) {
			console.log('Error', error, 'handling message event', event);
		}
	};
	
	window.bullet.onheartbeat = function () {
		try {
			console.log('i have', window.messages);
			var latestMessageTimestamp = window.messages[window.messages.length-1][0];
		} catch (error) {
			var latestMessageTimestamp = -1;
		}
		send('update?', [latestMessageTimestamp]);
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
	console.log('appended', messages, 'now i have', window.messages);
}

// actions

function send(type, data) { // type: string, data: array
	data.unshift(type);
	var string = JSON.stringify(data);
	console.log('sending', data);
	window.bullet.send(string);
}

// actions - shorthands

function sendMessage(content) {
	send('message', [
		content,
		window.userID,
		window.username ? window.username : 'Unknown'
	]);
}

// reactions

function handle(type, data) {
	switch (type) {
		case 'messages':
			appendMessages(data.reverse());
			break;
		default:
			throw 'Unexpected type';
	}
}