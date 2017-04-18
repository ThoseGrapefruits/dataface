# Dataface
## README
Requirements
------------
- [Haskell Platform](https://www.haskell.org/downloads#platform)
- [neo4j](https://neo4j.com/download/community-edition/)
    + Run locally (will run on [`localhost:7474`](http://localhost:7474))
    + Set up user with username & password of `dataface`, or change `Main.hs`
      to match your existing username & password

Usage
-----
```sh
git clone https://github.com/ThoseGrapefruits/dataface.git
cd dataface
stack build
stack exec dataface-exe
```

This will take a while to compile all required libraries.

Technical Specifications
------------------------


Goals
-----
- Use a NoSQL database (neo4j due to graph representation of faces).
- Use a programming language we haven't used before (Haskell).
- Make a product that requires some complexity above simple database interactions.

Authors
-------
- [Jack Davis](https://github.com/dackJavies)
- [Logan Moore](https://github.com/ThoseGrapefruits)
