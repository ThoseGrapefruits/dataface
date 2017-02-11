# dataface
Web-based human-evolved facial structure data, taking the concept of
[picbreeder](http://picbreeder.org/) and applying it to graphs of facial structure.

> Final project for CS 3200, Spring 2017 @ Northeastern University

Requirements
------------
- [Haskell Platform](https://www.haskell.org/downloads#platform)
- [neo4j](https://neo4j.com/download/community-edition/)
    + Run locally (will run on [`localhost:7474`](http://localhost:7474))
    + Set up user with username & password of `dataface`, or change [Main.hs](app/Main.hs)
      to match your existing username & password

Build
-----
```
git clone https://github.com/ThoseGrapefruits/dataface.git
cd dataface
stack build
```

Usage
-----
###### UNIX / GNU+LINUX
```
PORT=8080 stack exec dataface-exe
```

###### WINDOWS (PowerShell)
```
$env:PORT = 8080; stack exec dataface-exe
```

Concept
-------
- A web game à la [picbreeder](http://picbreeder.org), in which users select from a random human
  face structure (stored as a series of points in 2 or 3 dimensions) and slight modifications to
  each new set of provided structures allows enormous changes to happen over time.

Goals
-----
- Use a NoSQL database (MongoDB due to project constraints) in a pure manner.
- Use a functional language we haven't used before (Haskell).
- Make a product that requires some complexity above simple database interactions.

Authors
-------
- [Jack Davis](https://github.com/dackJavies)
- [Logan Moore](https://github.com/ThoseGrapefruits)