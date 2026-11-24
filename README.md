# Blackjack

Browser websocket Blackjack

## Instructions

    $ stack build
    $ stack exec mmaker

Browse to `localhost:3000/blackjack.html`

Run the test suite with:

    $ stack test

The project builds with GHC 9.10.3 via Stack snapshot `lts-24.68`.

## Container

Build and run the server in a container (Podman picks up `Containerfile`
automatically):

    $ podman build -t mmaker .
    $ podman run --rm -p 3000:3000 mmaker

With Docker, specify the Containerfile explicitly:

    $ docker build -f Containerfile -t mmaker .
    $ docker run --rm -p 3000:3000 mmaker

Then browse to `localhost:3000/blackjack.html`.

## TODO

- [ ] Make beter use of concurrency in Matchmake.hs
- [ ] Make code more beautiful
- [ ] Better testing...
