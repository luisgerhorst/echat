$(document).ready(start);

function start() {
	
	window.bullet = $.bullet('ws://localhost:8080/bullet');
	
	// reactions
	
	window.bullet.onopen = function () {
		var latestMessageTimestamp = (window.messages.length > 0) ? window.messages[window.messages.length-1].timestamp : -1;
		send('get_messages_since', latestMessageTimestamp); // ask for new messages since disconnect
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
	
}

// data

window.messages = [];

window.userID = (new Date()).getTime();

function setNickname(nickname) {
	window.nickname = nickname;
}

function appendMessages(messages) {
	window.messages.push.apply(window.messages, messages);
	console.log('received messages', messages);
}

// actions

function send(type, data) {
	var json = JSON.stringify({
		type: type,
		data: data
	});
	// console.log('sending', json);
	window.bullet.send(json);
}

// actions - shorthands

function sendMessage(content) {
	if (!window.nickname) throw 'no_nickname_set';
	send('save_message', {
		content: content,
		userID: window.userID,
		nickname: window.nickname
	});
}

// reactions

function handle(type, data) {
	switch (type) {
		case 'old_messages':
			// data is array of extended message objects, newest first
			appendMessages(data.reverse());
			break;
		case 'new_message':
			// data is single extended message object
			appendMessages([data]);
			break;
		default:
			throw 'Unexpected type';
	}
}