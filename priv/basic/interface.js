(function () {

window.chat = null;

$(document).ready(function () {
	window.chat = window.echat(register);
});

function register() {
	
	$('#connecting').hide();
	$('#register').show();
	
	$('#register button.submit').click(function () {
		
		$('#register p.message').text('Submitting ...');
		
		var username = $('input#username').val();
		
		function valid(username) {
			if (username) return true;
		}
		
		if (!valid(username)) {
			$('#username').addClass('error');
			$('#register p.message').text('Please enter a valid username');
		} else {
			window.chat.username(username, function (accepted) {
				if (!accepted) {
					$('#username').addClass('error');
					$('#register p.message').text('Username is already taken');
				} else {
					$('#setup').hide();
					window.username = username;
					join();
					send();
					load();
					time();
				}
			});
		}
		
	});
	
}

window.activeRoom = null;

function join() {
	
	$('#room-name').keydown(function (event) {
		var keypressed = event.keyCode || event.which;
		if (keypressed === 13) submit();
	});
	
	function submit() {
		
		var room = $('#room-name').val();
		
		if (!true) void(0); // #todo validate room name
		else {
			
			$('#room-name').val('');
			
			function deactivateActive() {
				$('#rooms .rooms .active').removeClass('active');
				$('#messages .rooms .active').removeClass('active');
				$('#users .active').removeClass('active');
			}
			
			// add
			
			deactivateActive();
			
			$('#rooms .rooms').append('<li data-room-name="' + room + '" class="id-' + encodeURIComponent(room) + ' active"><span class="name">' + room + '</span><button class="remove">leave</button></li>');
			$('#messages .rooms').append('<ol class="id-' + encodeURIComponent(room) + ' active"></ol>');
			$('#users').append('<ul class="id-' + encodeURIComponent(room) + ' active"></ul>');
			
			window.activeRoom = room;
			    
			$('#rooms .rooms .id-' + encodeURIComponent(room)).click(function () { // activate
				
				deactivateActive();
				
				$('#rooms .rooms .id-' + encodeURIComponent(room)).addClass('active');
				$('#messages .rooms .id-' + encodeURIComponent(room)).addClass('active');
				$('#users .id-' + encodeURIComponent(room)).addClass('active');
				
				window.activeRoom = room;
				calcActiveTime();
				
			});
			
			$('#rooms .rooms .id-' + encodeURIComponent(room) + ' .remove').click(function () { // remove
				
				// choose new active if was active, remove self
				
				chat.leave(room);
				
				$('#rooms .rooms .id-' + encodeURIComponent(room)).remove();
				$('#messages .rooms .id-' + encodeURIComponent(room)).remove();
				$('#users .id-' + encodeURIComponent(room)).remove();
				
				if (window.activeRoom === room) { // was active
					$('#rooms .rooms li').first().addClass('active');
					$('#messages .rooms ol').first().addClass('active');
					$('#users ul').first().addClass('active');
					window.activeRoom = $('#rooms .rooms li').first().data('room-name');
					calcActiveTime();
				}
				
			});
			
			window.chat.join(room, 20, function (users, messages) {
				
				(function (usernames) {
					
					// fill users with initial users html
					
					var usersListHTML = '';
					
					for (var i = usernames.length; i--;) {
						var username = usernames[i];
						usersListHTML = '<li class="id-' + encodeURIComponent(username) + '">' + username + '</li>' + usersListHTML;
					}
					
					$('#users .id-' + encodeURIComponent(room)).html(usersListHTML);
					
				})(users);
				
				(function (messages) {
					
					// fill messages with initial messages
					
					console.log("Join: Initial messages", messages);
					
					var messagesListHTML = '';
					
					for (var i = messages.length; i--;) {
						var message = messages[i],
						    username = message.username,
						    content = message.content,
						    timestamp = message.timestamp;
						messagesListHTML = '<li><span class="name">' + username + '</span><span class="timestamp" data-timestamp="' + timestamp + '"></span><span class="message">' + content + '</span></li>' + messagesListHTML;
					}
					
					$('#messages .rooms .id-' + encodeURIComponent(room)).html(messagesListHTML);
					calcActiveTime();
					
				})(messages);
								
			}, function (action, username) {
				
				console.log('Room:', action, 'of', username);
				
				switch (action) {
					case 'join':
						$('#users ul.id-' + encodeURIComponent(room)).append('<li class="id-' + encodeURIComponent(username) + '">' + username + '</li>');
						break;
					case 'leave':
						$('li.id-' + encodeURIComponent(username), '#users ul.id-' + encodeURIComponent(room)).remove();
						break;
				}
				
			}, function (username, content, timestamp) {
				
				$('#messages .rooms .id-' + encodeURIComponent(room)).append('<li><span class="name">' + username + '</span><span class="timestamp" data-timestamp="' + timestamp + '"></span><span class="message">' + content + '</span></li>');
				calcActiveTime();
				
			}); // window.chat.join
			
		}
		
	}
	
}

function send() {
	
	console.log('Send: Function called.')
	
	$('#send button.submit').click(function () {
		
		console.log('Send: Handler called.');
		
		var content = $('#new-message').val();
		
		if (!true) void(0); // #todo validate message
		else {
			
			var room = window.activeRoom,
			    username = window.username;
			    
			var unconfirmedID = Date.now();
			
			$('#new-message').val('');
			$('#messages .rooms .id-' + encodeURIComponent(room)).append('<li class="sent unconfirmed unconfirmed-id-' + unconfirmedID + '"><span class="name">' + username + '</span><span class="timestamp" data-timestamp="' + Date.now() + '"></span><span class="message">' + content + '</span></li>');
			calcActiveTime();
			
			window.chat.room(room).send(content, function (timestamp) {
				
				$('#messages .rooms .id-' + encodeURIComponent(room) + ' .unconfirmed-id-' + unconfirmedID + ' span.timestamp').attr('data-timestamp', timestamp);
				$('#messages .rooms .id-' + encodeURIComponent(room) + ' .unconfirmed-id-' + unconfirmedID).removeClass('unconfirmed-id-' + unconfirmedID).removeClass('unconfirmed');
				calcActiveTime();
				
			});
			
		}
		
	});
	
}

function load() {
	
	$('#load').click(function () {
		
		var room = window.activeRoom;
		
		console.log('Load: Hanlder called.');
		
		window.chat.room(room).load(50, function (messages) {
			
			console.log('Load: Messages', messages);
			
			var messagesListHTML = '';
			
			for (var i = messages.length; i--;) {
				var message = messages[i],
					username = message.username,
					content = message.content,
					timestamp = message.timestamp;
				messagesListHTML = '<li><span class="name">' + username + '</span><span class="timestamp" data-timestamp="' + timestamp + '"></span><span class="message">' + content + '</span></li>' + messagesListHTML;
			}
			
			$('#messages .rooms .id-' + encodeURIComponent(room)).prepend(messagesListHTML);
			calcActiveTime();
			
		});
		
	});
	
}

function time() {
	
	setInterval(calcActiveTime, 30*1000);
	
}

function calcActiveTime() {
	
	console.log('Calc Active Time: called.');
	
	$('#messages .rooms ol.active li span.timestamp').each(function () {
	
		var timestamp = Math.round(parseFloat($(this).attr('data-timestamp')));
		var relative = moment(timestamp).fromNow();
		$(this).html(relative);
	
	});
	
}

})();