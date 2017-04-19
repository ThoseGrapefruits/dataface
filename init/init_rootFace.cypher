CREATE (rootUser:User {username: 'root', created_at:TIMESTAMP()})
CREATE (rootFace:Face {name:'Root Face', created_at:TIMESTAMP()})
CREATE (rootUser) -[:CREATED]-> (rootFace)

// Create the points of the Root Face
CREATE
    // Outline, top to bottom
    (outline_hairLine:Point {x:0.275, y:0.05}),
    (outline_temple:Point {x:0.4, y:0.275}),
    (outline_cheek:Point {x:0.35, y:0.55}),
    (outline_jowl:Point {x:0.2, y:0.8}),

    // Vertical interior centre points, from bottom
    (inline_bottomMiddle:Point {x:0.0, y:1.0}),
    (inline_lowerLipMiddle:Point {x:0.0, y:0.75}),
    (inline_upperLipMiddle:Point {x:0.0, y:0.7}),
    (inline_noseTip:Point {x:0.0, y:0.625}),
    (inline_noseBridge:Point {x:0.0, y:0.4}),
    (inline_browBridge:Point {x:0.0, y:0.25}),
    (inline_topMiddle:Point {x:0.0, y:0.0}),

    // Eyebrow
    (eyebrow_topRight:Point {x:0.03, y:0.2}),
    (eyebrow_topLeft:Point {x:0.225, y:0.21}),
    (eyebrow_bottomLeft:Point {x:0.225, y:0.23}),
    (eyebrow_bottomRight:Point {x:0.03, y:0.22}),

    // Eye
    (eye_top:Point {x:0.14, y:0.275}),
    (eye_left:Point {x:0.24, y:0.325}),
    (eye_bottom:Point {x:0.14, y:0.375}),
    (eye_right:Point {x:0.04, y:0.325}),

    // Cheekbone
    (cheek_bone:Point {x:0.2, y:0.5}),

    // Nose (other 2 points covered by vertical interior centre points)
    (nose_outsideNostril:Point {x:0.06, y:0.6}),

    // Lips (other 2 points covered by vertical interior centre points)
    (lips_cupidBowPeak:Point {x:0.03, y:0.69}),
    (lips_corner:Point {x:0.15, y:0.73})

// Connect start of Root Face
CREATE (rootFace) -[:STARTS_AT]-> (inline_topMiddle)

/*
 * All edges, feature-by-feature, top-to-bottom, (in/out)line first.
 *
 * Connections are always represented as (feature) -[:LINK]-> (outline) (feature first)
 * where a connection exists between a feature and the outline.
 *
 * If a connection could be considered both a support and part of the feature,
 * (e.g. the nose), those connections are under the feature's section,
 * not feature's supports section. For example, both lines making up the nose
 * connect to the inline structure, but are listed under "Nose".
 */

// Create all edges in the Root Face
CREATE
    // Outline, top to bottom
    (inline_topMiddle) -[:LINK]-> (outline_hairLine),
    (outline_hairLine) -[:LINK]-> (outline_temple),
    (outline_temple) -[:LINK]-> (outline_cheek),
    (outline_cheek) -[:LINK]-> (outline_jowl),
    (outline_jowl) -[:LINK]-> (inline_bottomMiddle),

    // Vertical centre line (not sure if these connections should actually exist)
    (inline_bottomMiddle) -[:LINK]-> (inline_lowerLipMiddle),
    (inline_lowerLipMiddle) -[:LINK]-> (inline_upperLipMiddle),
    (inline_upperLipMiddle) -[:LINK]-> (inline_noseTip),
    (inline_noseTip) -[:LINK]-> (inline_noseBridge),
    (inline_noseBridge) -[:LINK]-> (inline_topMiddle),

    // Eyebrow
    (eyebrow_topRight) -[:LINK]-> (eyebrow_topLeft),
    (eyebrow_topLeft) -[:LINK]-> (eyebrow_bottomLeft),
    (eyebrow_bottomLeft) -[:LINK]-> (eyebrow_bottomRight),
    (eyebrow_bottomRight) -[:LINK]-> (eyebrow_topRight),

    // Eyebrow supports
    (eyebrow_topRight) -[:LINK]-> (inline_topMiddle),
    (eyebrow_topLeft) -[:LINK]-> (outline_hairLine),
    (eyebrow_bottomLeft) -[:LINK]-> (outline_temple),
    (eyebrow_bottomLeft) -[:LINK]-> (eye_left),
    (eyebrow_bottomRight) -[:LINK]-> (inline_browBridge),

    // Eye
    (eye_top) -[:LINK]-> (eye_left),
    (eye_left) -[:LINK]-> (eye_bottom),
    (eye_bottom) -[:LINK]-> (eye_right),
    (eye_right) -[:LINK]-> (eye_top),

    // Eye supports
    (eye_left) -[:LINK]-> (outline_temple),
    (eye_left) -[:LINK]-> (outline_cheek),
    (eye_left) -[:LINK]-> (cheek_bone),
    (eye_right) -[:LINK]-> (inline_browBridge),
    (eye_right) -[:LINK]-> (inline_noseBridge),

    // Cheekbone supports
    (cheek_bone) -[:LINK]-> (inline_noseBridge),
    (cheek_bone) -[:LINK]-> (outline_cheek),
    (cheek_bone) -[:LINK]-> (nose_outsideNostril),
    (cheek_bone) -[:LINK]-> (lips_corner),
    (cheek_bone) -[:LINK]-> (outline_jowl),

    // Nose
    (nose_outsideNostril) -[:LINK]-> (inline_noseBridge),
    (nose_outsideNostril) -[:LINK]-> (inline_noseTip),

    // Nose supports
    (nose_outsideNostril) -[:LINK]-> (lips_cupidBowPeak),
    (nose_outsideNostril) -[:LINK]-> (lips_corner),

    // Lips
    (lips_corner) -[:LINK]-> (lips_cupidBowPeak),
    (lips_cupidBowPeak) -[:LINK]-> (inline_upperLipMiddle),
    (lips_corner) -[:LINK]-> (inline_lowerLipMiddle),

    // Lips supports
    (lips_corner) -[:LINK]-> (outline_jowl),
    (lips_corner) -[:LINK]-> (inline_bottomMiddle)

// Face duplication
// MATCH (u:User {username: "root" })-[created:CREATED]->(f0:Face)-[:STARTS_AT]->(p0:Point)<-[:LINK*0..10]-(end:Point)-[l:LINK]-(start:Point)
// WITH COLLECT(l) AS links, f0 AS f0, p0 AS p0, created AS created, u AS u
// CREATE (copy:Face)
// SET copy=f0
// SET copy.name = 'Copy2'
// 
// CREATE (u)-[:CREATED]->(copy)
// 
// CREATE (copyP0:Point)
// SET copyP0=p0
// 
// CREATE (copy)-[:STARTS_AT]->(copyP0)
// 
// FOREACH (link in links |
  // CREATE (copyStart:Point)
  // SET copyStart=startNode(link)
  // CREATE (copyEnd:Point)
  // SET copyEnd=endNode(link)
  // CREATE (copyStart)-[copyLink:LINK]->(copyEnd)
// )
// 
// RETURN copy, copyP0
