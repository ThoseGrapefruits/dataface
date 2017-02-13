CREATE (testUser:User {username: 'test'})

CREATE (ogFace:Face {name:'The Root Face'})
CREATE (newFace:Face {name:'Sample Duplicate Face'})
CREATE
  (newFace)-[:FORKS]->(ogFace),
  (testUser)-[:CREATED]->(newFace)

CREATE
    (p0_og:Point),
    (p1_og:Point),
    (p2_og:Point)
CREATE (p0_og)-[:STARTS]->(ogFace)

CREATE (p0_new:Point)
CREATE (p0_new)-[:STARTS]->(newFace)

