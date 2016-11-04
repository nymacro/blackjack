var wsUri = "ws://" + document.location.host + "/blackjack";

var websocket;

var messages = document.getElementById("messages");
var hand     = document.getElementById("cards");

var nameInput = document.getElementById("inputName");
var connectButton = document.getElementById("connectButton");
connectButton.onclick = function() { connect(); clearCards(); };

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
                  "Ten"   : "0",
                  "Jack"  : "J",
                  "Queen" : "Q",
                  "King"  : "K",
                  "Ace"   : "ace" };
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

var tap    = document.getElementById("tapButton");
var sit    = document.getElementById("sitButton");
tap.onclick = function() { sendText("tap"); };
sit.onclick = function() { sendText("sit"); };

function clearCards() {
    while (hand.firstChild) {
        hand.removeChild(hand.firstChild);
    }
}

function updateConnected() {
    nameInput.disabled = true;
    connectButton.disabled = true;
}

function updateDisconnected() {
    nameInput.disabled = false;
    connectButton.disabled = false;
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
    var msg = document.createElement("div");
    msg.appendChild(document.createTextNode(message));
    output.appendChild(msg);
}

function addCard(card) {
    var match = /^(.*) Card/.exec(card);
    if (match) {
        var name = match[1];
        var toHand = null;
        if (name == "YOU") {
            toHand = hand;
        } else {
            // create if it doesn't exist
            var others = document.getElementById("others");
            toHand = document.getElementById("player_" + name);
            if (!toHand) {
                toHand = document.createElement("div");
                toHand.id = "player_" + name;
                var nameHeader = document.createElement("h3");
                nameHeader.appendChild(document.createTextNode(name));
                toHand.appendChild(nameHeader);
                others.appendChild(toHand);
            }
        }
        var img = document.createElement("img");
        img.setAttribute("src", cardImageUrl(card));
        if (name != "YOU") {
            img.width = "96";
        } else {
            img.width = 128;
        }
        toHand.appendChild(img);
    }
}

function onOpen(evt) {
    messages.innerHTML = "Connected to " + wsUri;

    // send username
    websocket.send(myName());
}

function onClose(evt) {
    messages.className = "alert alert-warning";
    messages.innerHTML = "Disconnected";

    updateDisconnected();
}

function onError(evt) {
    messages.className = "alert alert-danger";
    console.log("Error: " + evt);
    messages.innerHTML = "Error: " + evt.data;
}

function onMessage(evt) {
    console.log("Receieved: " + evt.data);
    writeToScreen(evt.data);
    addCard(evt.data);
}

function sendText(json) {
    console.log("sending text: " + json);
    websocket.send(json);
}

