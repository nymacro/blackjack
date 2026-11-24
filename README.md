# Blackjack

Browser websocket Blackjack

## Instructions

    $ stack build
    $ stack exec mmaker

Browse to `localhost:3000/blackjack.html`

Run the test suite with:

    $ stack test

The project builds with GHC 9.10.3 via Stack snapshot `lts-24.68`.

## TODO

- [ ] Make beter use of concurrency in Matchmake.hs
- [ ] Make code more beautiful
- [ ] Better testing...
