var wsUri = "ws://" + document.location.host + "/match";

var websocket;

var messages = document.getElementById("messages");

var nameInput = document.getElementById("inputName");
var connectButton = document.getElementById("connectButton");
connectButton.onclick = function() { connect(); };

var output = document.getElementById("output");
var input  = document.getElementById("inputText");
var button = document.getElementById("sendButton");
button.onclick = function() {
    if (input.value == "") {
        return;
    }
    sendText(input.value);
    input.value = "";
};

input.disabled = true;
button.disabled = true;

function updateConnected() {
    nameInput.disabled = true;
    connectButton.disabled = true;

    input.disabled = false;
    button.disabled = false;
}

function updateDisconnected() {
    nameInput.disabled = false;
    connectButton.disabled = false;

    input.disabled = true;
    button.disabled = true;
}

function connect() {
    websocket = new WebSocket(wsUri);
    websocket.onopen    = function(evt) { onOpen(evt); };
    websocket.onmessage = function(evt) { onMessage(evt); };
    websocket.onerror   = function(evt) { onError(evt); };
    websocket.onclose   = function(evt) { onClose(evt); };

    updateConnected();
}

function myName() {
    return nameInput.value;
}

function writeToScreen(message) {
    output.innerHTML += emojify.replace(message) + "<br/>";
    //output.innerHTML = message;
}

function onOpen(evt) {
    //writeToScreen("Connected to " + wsUri);
    messages.innerHTML = "Connected to " + wsUri;

    // send username
    websocket.send(myName());
}

function onClose(evt) {
    //writeToScreen("Disconnected...");
    messages.className = "alert alert-warning";
    messages.innerHTML = "Disconnected";

    updateDisconnected();
}

function onError(evt) {
    messages.className = "alert alert-danger";
    console.log("Error: " + evt);
    //writeToScreen("Error: " + evt.data);
    messages.innerHTML = "Error: " + evt.data;
}

function onMessage(evt) {
    console.log("Receieved: " + evt.data);
    //writeToScreen("Receieved message: " + evt.data);
    writeToScreen(evt.data);
}

function sendText(json) {
    console.log("sending text: " + json);
    //writeToScreen("Sent: " + json);
    websocket.send(json);
}

