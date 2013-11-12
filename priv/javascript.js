(function () {
	
$(document).ready(function () {
	
	var chat = new Chat(function () { // on ready
		// allow user to register/show interface
	});
	
	chat.username('mark', function (accepted) { // username, on res
		// accepted?
	});
	
	chat.join('lobby', 5, function (users, messages) { // room name, number of old messages to load, on messages & users received
		// joined, here are the users
	}, function (action, username) { // on users change
		// each time users change
		// go threw interface html and remove user with matching username
	}, function (username, content, timestamp) { // on new message
		// new message
	});
	
	chat.room('lobby').send('Hi', function (timestamp) { // content, on success and res from server
		// sent & received, show in interface
	});
	
	chat.room('lobby').load(10, function (messages) { // number of old messages to load, on receive
		// messages requested
	});
	
	chat.leave('lobby');
	
});

})();