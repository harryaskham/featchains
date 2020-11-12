# (feat. Chains)

Perform a BFS of the Spotify API where nodes are artists, and edges are collaborations on songs, in order to build the graph of collaborating artists. Scrapes about 20 artists per second.

Written up in Haskell. Currently in exploratory, single-file scripting phase. Supports scraping and finding chains of artists in reasonable time.

Typical output:

```haskell
"Is there a path from Eminem to Emma Bunton: True"
["Eminem","Snoop Dogg","Olly Murs","Robbie Williams","Emma Bunton"]
```

## Requirements

- postgresql set up with local DB "featchains"
- API secret stored in file in root dir called "secret"

## Next Steps

- Track names to augment the path
- More scraping, larger dataset
- Need to store which artists we actually scraped versus just saw (first ID versus second ID as a proxy?)
- Resume scraping state
