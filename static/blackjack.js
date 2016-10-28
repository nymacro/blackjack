var wsUri = "ws://" + document.location.host + "/blackjack";

var websocket;

var messages = document.getElementById("messages");
var hand     = document.getElementById("cards");

var nameInput = document.getElementById("inputName");
var connectButton = document.getElementById("connectButton");
connectButton.onclick = function() { connect(); };

function cardImageUrl(card) {
    var pat   = /Card (.*) (.*)/i;
    var match = pat.exec(card);
    var rank  = { "Two"   : "2",
                  "Three" : "3",
                  "Four"  : "4",
                  "Five"  : "5",
                  "Six"   : "6",
                  "Seven" : "7",
                  "Eight" : "8",
                  "Nine"  : "9",
                  "Ten"   : "10",
                  "Jack"  : "J",
                  "Queen" : "Q",
                  "King"  : "K",
                  "Ace"   : "A" };
    var suit  = { "Diamonds" : "D",
                  "Clubs"    : "C",
                  "Hearts"   : "H",
                  "Spades"   : "S" };
    console.log(match);
    var name = rank[match[1]] + suit[match[2]];
    var baseUrl = "/img/";
    return baseUrl + name + ".png";
}

var output = document.getElementById("output");
// var input  = document.getElementById("inputText");
// var button = document.getElementById("sendButton");
// button.onclick = function() {
//     if (input.value == "") {
//         return;
//     }
//     sendText(input.value);
//     input.value = "";
// };

var tap    = document.getElementById("tapButton");
var sit    = document.getElementById("sitButton");
tap.onclick = function() { sendText("tap"); };
sit.onclick = function() { sendText("sit"); };

// input.disabled = true;
// button.disabled = true;

function updateConnected() {
    nameInput.disabled = true;
    connectButton.disabled = true;

    // input.disabled = false;
    // button.disabled = false;
}

function updateDisconnected() {
    nameInput.disabled = false;
    connectButton.disabled = false;

    // input.disabled = true;
    // button.disabled = true;
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
    // output.innerHTML += emojify.replace(message) + "<br/>";
    output.innerHTML += message + "<br />";
}

function addCard(card) {
    if (/^Card/.test(card)) {
        var img = document.createElement("img");
        img.setAttribute("src", cardImageUrl(card));
        hand.appendChild(img);
    }
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
    addCard(evt.data);
}

function sendText(json) {
    console.log("sending text: " + json);
    //writeToScreen("Sent: " + json);
    websocket.send(json);
}

