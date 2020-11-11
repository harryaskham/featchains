# (feat. Chains)

Perform a BFS of the Spotify API where nodes are artists, and edges are collaborations on songs, in order to build the graph of collaborating artists. Scrapes about 20 artists per second.

Written up in Haskell.

## Requirements

- postgresql set up with local DB "featchains"
- API secret stored in file in root dir called "secret"

## Current Status

- Rate-limited scraping working
- Simple query examples done
- Data is dominated by huge choirs / orchestras

## Next Steps

- Messy one-script code, to be cleaned up
- persist info about which artists have been scraped
- enable resuming from database
- start writing graph algos
