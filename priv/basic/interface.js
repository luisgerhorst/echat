(function () {

var chat = null;

$(document).ready(function () {
	chat = window.echat(register);
});

function register() {
	
	$('#connecting').hide();
	$('#register').show();
	
	$('#register button.submit').click(function () {
		
		$('#register p.message').text('Logging in ...');
		
		var username = $('input#username').val();
		
		function valid(username) {
			if (username) return true;
		}
		
		if (!valid(username)) $('#register p.message').text('Please enter a username.');
		else {
			chat.username(username, function (accepted) {
				if (!accepted) $('#register p.message').text('Username is already taken, please choose another one.');
				else {
					$('#register').hide();
					join();
				}
			});
		}
		
	});
	
}

function join() {
	
	$('#join button.submit').click(function () {
		
		var room = $('#room-name').val();
		
		if (false) void(0); // #todo validate room name
		else {
			
			$('#rooms .rooms .active').removeClass('active');
			$('#rooms .rooms').append('<li class="id-' + encodeURIComponent(room) + ' active">' + room + '</li>');
			
			$('#messages .rooms .active').removeClass('active');
			$('#messages .rooms').append('<ol class="id-' + encodeURIComponent(room) + ' active"></ol>');
			
			$('#users .active').removeClass('active');
			$('#users').append('<ul class="id-' + encodeURIComponent(room) + ' active"></ul>');
			
			var roomItem = $('#rooms .rooms .id-' + encodeURIComponent(room)),
			    messagesList = $('#messages .rooms .id-' + encodeURIComponent(room)),
			    usersList = $('#users .id-' + encodeURIComponent(room));
			
			chat.join(room, 15, function (users, messages) {
				
				(function (usernames) {
					
					var usersListHTML = '';
					
					for (var i = usernames.length; i--;) {
						var username = usernames[i];
						usersListHTML += '<li class="id-' + encodeURIComponent(username) + '">' + username + '</li>';
					}
					
					usersList.html(usersListHTML);
					
				})(users);
				
				(function (messages) {
					
					var messagesListHTML = '';
					
					for (var i = messages.length; i--;) {
						var message = messages[i];
						messagesListHTML = '<li data-timestamp="' + message.timestamp + '"><span class="name">' + message.username + '</span><span class="message">' + message.content + '</span></li>' + messagesListHTML;
					}
					
					messagesList.html(messagesListHTML);
					
				})(messages);
				
				// #todo here i left off
								
			}, function (action, username, allUsers) {
				
				
				
			}, function (username, content, timestamp, allMessages) {
				
				
				
			});
			
		}
		
	});
	
}

})();