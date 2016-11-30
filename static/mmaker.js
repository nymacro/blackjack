var usernameInput  = document.getElementById('username');
var content        = document.getElementById('content');
var usernameSubmit = document.getElementById('usernameSubmit');
var messageInput   = document.getElementById('message');
var messageSubmit  = document.getElementById('messageSubmit');

var socket;
//var socket         = new WebSocket('ws://localhost:3000/');

function addContent(msg) {
    var div = document.createElement('div');
    div.className = 'message';
    div.innerHTML = msg;

    content.appendChild(div);
}

function messageEnabled(b) {
    messageInput.disabled = !b;
    messageSubmit.disabled = !b;
}
function usernameEnabled(b) {
    usernameInput.disabled = !b;
    usernameSubmit.disabled = !b;
}
function connectEnabled(b) {
    messageEnabled(!b);
    usernameEnabled(b);
}
connectEnabled(true);

function connectSocket() {
    socket = new WebSocket('ws://' + document.location.host + '/');

    socket.onopen = function (event) {
        addContent("Connected");
        socket.send(usernameInput.value);
    };
    socket.onmessage = function (event) {
        console.log(event.data);
        addContent(event.data);
    };
    socket.onclose = function (event) {
        addContent("Disconnected");
        connectEnabled(true);
    };
}

usernameSubmit.onclick = function () {
    connectSocket();
    // socket.send(usernameInput.value);
    // addContent("Joined");

    connectEnabled(false);
};

messageSubmit.onclick = function () {
    socket.send(messageInput.value);
};
