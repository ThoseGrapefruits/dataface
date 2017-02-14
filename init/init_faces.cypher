CREATE (testUser:User {username: 'test'})

CREATE (ogFace:Face {name:'The Root Face'})
CREATE (newFace:Face {name:'Sample Duplicate Face'})
CREATE
  (newFace)-[:FORKS]->(ogFace),
  (testUser)-[:CREATED]->(newFace)

CREATE
    // Outline, top to bottom
    (p0_og:Point {x:0.0, y:0.0}), // top middle
    (p1_og:Point {x:0.275, y:0.5}), // hairline
    (p2_og:Point {x:0.4, y:0.275}), // temple
    (p3_og:Point {x:0.35, y:0.55}), // cheek
    (p4_og:Point {x:0.2, y:0.8}), // jowl
    (p5_og:Point {x:0.0, y:1.0}), // bottom middle

    // Vertical interior centre points, from bottom
    (p6_og:Point {x:0.0, y:0.75}), // brow bridge
    (p7_og:Point {x:0.0, y:0.7}), // nose bridge
    (p8_og:Point {x:0.0, y:0.625}), // nose tip
    (p9_og:Point {x:0.0, y:0.4}), // upper lip centre
    (p10_og:Point {x:0.0, y:0.25}), // lower lip centre

    // Eyebrow
    (p11_og:Point {x:0.03, y:0.2}), // top right
    (p12_og:Point {x:0.225, y:0.21}), // top left
    (p13_og:Point {x:0.225, y:0.23}), // bottom left
    (p14_og:Point {x:0.03, y:0.22}), // bottom right

    // Eye
    (p15_og:Point {x:0.14, y:0.275}), // top
    (p16_og:Point {x:0.24, y:0.325}), // left
    (p17_og:Point {x:0.14, y:0.375}), // bottom
    (p18_og:Point {x:0.04, y:0.325}), // right

    // Cheekbone
    (p19_og:Point {x:0.2, y:0.5}),

    // Nose (other 2 points covered by vertical interior centre points)
    (p20_og:Point {x:0.06, y:0.6}), // outside nostril

    // Lips (other 2 points covered by vertical interior centre points)
    (p21_og:Point {x:0.02, y:0.69}), // cupid's bow peak
    (p22_og:Point {x:0.15, y:0.73})  // corner

CREATE (ogFace)-[:STARTS_AT]->(p0_og)

CREATE (p0_new:Point)
CREATE (p0_new)-[:STARTS]->(newFace)

