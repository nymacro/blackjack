var wsUri = "ws://" + document.location.host + "/blackjack";

var websocket;

var alerts   = document.getElementById("alerts");
var hand     = document.getElementById("cards");
var others   = document.getElementById("others");

var nameInput = document.getElementById("inputName");
var nameText  = document.getElementById("player-name");
var connectButton = document.getElementById("connectButton");
connectButton.onclick = function() { connect(); clearGame(); };

var messages = document.getElementById("messages");
var instruments = document.getElementById("instruments");
var contents = document.getElementById("game-content");

var tap    = document.getElementById("tapButton");
var sit    = document.getElementById("sitButton");
tap.onclick = function() { sendText("tap"); };
sit.onclick = function() { sendText("sit"); };

function cardImageUrl(card) {
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
    var name = rank[card.rank] + suit[card.suit];
    var baseUrl = "/img/";
    return baseUrl + name + ".png";
}

function clearGame() {
    dealVue.reset();
    // while (hand.firstChild) {
    //     hand.removeChild(hand.firstChild);
    // }
    while (others.firstChild) {
        others.removeChild(others.firstChild);
    }
    while (messages.firstChild) {
        messages.removeChild(messages.firstChild);
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

function config() {
    return { "name": nameInput.value,
             "opts": { "players" : 1 } };
}

function writeToScreen(message) {
    var msg = document.createElement("div");
    msg.appendChild(document.createTextNode(message));
    messages.appendChild(msg);
}

function addCard(card) {
    var name = card.user.name;
    var toHand = null;
    if (name == nameInput.value) {
        toHand = hand;

        dealVue.addCard(card.card);
        return;
    } else {
        // create if it doesn't exist
        toHand = document.getElementById("player_" + name);
        if (!toHand) {
            toHand = document.createElement("div");
            toHand.id = "player_" + name;
            var nameHeader = document.createElement("div");
            nameHeader.className = "label label-default";
            nameHeader.style = "font-size: 200%";
            nameHeader.appendChild(document.createTextNode(name));
            toHand.appendChild(nameHeader);
            others.appendChild(toHand);
        }
    }
    var img = document.createElement("img");
    img.setAttribute("src", cardImageUrl(card.card));
    if (toHand != hand) {
        img.className = "card-small";
    } else {
        img.className = "card";
    }
    toHand.appendChild(img);
}

function onOpen(evt) {
    alerts.innerHTML = "Connected to " + wsUri;
    websocket.send(JSON.stringify(config()));

    instruments.style.display = 'none';
    contents.style.display = 'block';

    var node = document.createElement("span");
    node.appendChild(document.createTextNode("Playing as "));
    var b = document.createElement("b");
    b.appendChild(document.createTextNode(nameInput.value));
    node.appendChild(b);
    nameText.appendChild(node);
}

function onClose(evt) {
    instruments.style.display = 'block';
    contents.style.display = 'none';

    updateDisconnected();
    alerts.className = "alert alert-warning";
    alerts.innerHTML = "Disconnected";

    while (nameText.firstChild) {
        nameText.removeChild(nameText.firstChild);
    }
}

function onError(evt) {
    console.log("Error: " + evt);
    alerts.className = "alert alert-danger";
    alerts.innerHTML = "Error: " + evt.data;
}

function onMessage(evt) {
    console.log("Receieved: " + evt.data);
    writeToScreen(evt.data);
    var msg = JSON.parse(evt.data);
    if (msg.type == "deal") {
        addCard(msg.data);
    } else if (msg.type == "done") {
        websocket.close();
        // TODO show winner[s] in a nice way
    }
}

function sendText(msg) {
    console.log("sending text: " + msg);
    websocket.send(msg);
}

var dealVue = new Vue({
    el: "#cards",
    data: {
        items: []
    },
    methods: {
        reset: function() {
            this.items = [];
        },
        addCard: function(card) {
            this.items.push("<img class=\"card\" src=\"" + cardImageUrl(card) + "\" />");
        }
    }
});
