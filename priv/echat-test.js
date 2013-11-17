/*

API:

start test mode:
_startTest();

test api:
test.method();

*/

$(document).ready(function () {

	window._startTest = function () {
		
		window.testChat = window.echat(function () { // on ready
			// allow user to register/show interface
	
			console.log('Connected.');
	
			window.test = { // call this object's methods
	
				username: function () {
	
					function id() {
						return Math.floor( Math.random() * Math.pow(2, 16) );
					};
	
					var username = 'user#'+id();
					testChat.username(username, function (accepted) { // username, on res
						// accepted?
						console.log('Username', username, 'accepted:', accepted);
					});
					return username;
				},
	
				join: function (room, toLoad) {
					testChat.join(room, toLoad, function (users, messages) { // room name, number of old messages to load, on messages & users received
						// joined, here are the users
						console.log('Joined', room, 'received users', users, 'and messages', messages);
					}, function (action, username, allUsers) { // on users change
						// each time users change
						// go threw interface html and remove user with matching username
						console.log('User', username, 'performs', action, 'in', room, 'All users are now', allUsers);
					}, function (username, content, timestamp, allMessages) { // on new message
						// new message
						console.log('Message in', room, 'with data', username, content, timestamp, 'All messages now', allMessages);
					});
				},
	
				send: function (room, content) {
					testChat.room(room).send(content, function (timestamp) { // content, on success and res from server
						// sent & received, show in interface
						console.log('Own message was received by server at', timestamp);
					});
				},
	
				load: function (room, limit) {
					testChat.room(room).load(limit, function (messages, allMessages) { // number of old messages to load, on receive
						// messages requested
						console.log('Messages in room', room, 'loaded:', messages, 'all', allMessages);
					});
				},
	
				leave: function (room) {
					testChat.leave('lobby');
				}
	
			}
	
		});
		
	};

});