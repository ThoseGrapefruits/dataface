CREATE (rootFace:Face {name:'Root Face', created_at:TIMESTAMP()})

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
 * Connections are always represented as (feature) -[:LINE]-> (outline) (feature first)
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
    (inline_topMiddle) -[:LINE]-> (outline_hairLine),
    (outline_hairLine) -[:LINE]-> (outline_temple),
    (outline_temple) -[:LINE]-> (outline_cheek),
    (outline_cheek) -[:LINE]-> (outline_jowl),
    (outline_jowl) -[:LINE]-> (inline_bottomMiddle),

    // Vertical centre line (not sure if these connections should actually exist)
    (inline_bottomMiddle) -[:LINE]-> (inline_lowerLipMiddle),
    (inline_lowerLipMiddle) -[:LINE]-> (inline_upperLipMiddle),
    (inline_upperLipMiddle) -[:LINE]-> (inline_noseTip),
    (inline_noseTip) -[:LINE]-> (inline_noseBridge),
    (inline_noseBridge) -[:LINE]-> (inline_topMiddle),

    // Eyebrow
    (eyebrow_topRight) -[:LINE]-> (eyebrow_topLeft),
    (eyebrow_topLeft) -[:LINE]-> (eyebrow_bottomLeft),
    (eyebrow_bottomLeft) -[:LINE]-> (eyebrow_bottomRight),
    (eyebrow_bottomRight) -[:LINE]-> (eyebrow_topRight),

    // Eyebrow supports
    (eyebrow_topRight) -[:LINE]-> (inline_topMiddle),
    (eyebrow_topLeft) -[:LINE]-> (outline_hairLine),
    (eyebrow_bottomLeft) -[:LINE]-> (outline_temple),
    (eyebrow_bottomLeft) -[:LINE]-> (eye_left),
    (eyebrow_bottomRight) -[:LINE]-> (inline_browBridge),

    // Eye
    (eye_top) -[:LINE]-> (eye_left),
    (eye_left) -[:LINE]-> (eye_bottom),
    (eye_bottom) -[:LINE]-> (eye_right),
    (eye_right) -[:LINE]-> (eye_top),

    // Eye supports
    (eye_left) -[:LINE]-> (outline_temple),
    (eye_left) -[:LINE]-> (outline_cheek),
    (eye_left) -[:LINE]-> (cheek_bone),
    (eye_right) -[:LINE]-> (inline_browBridge),
    (eye_right) -[:LINE]-> (inline_noseBridge),

    // Cheekbone supports
    (cheek_bone) -[:LINE]-> (inline_noseBridge),
    (cheek_bone) -[:LINE]-> (outline_cheek),
    (cheek_bone) -[:LINE]-> (nose_outsideNostril),
    (cheek_bone) -[:LINE]-> (lips_corner),
    (cheek_bone) -[:LINE]-> (outline_jowl),

    // Nose
    (nose_outsideNostril) -[:LINE]-> (inline_noseBridge),
    (nose_outsideNostril) -[:LINE]-> (inline_noseTip),

    // Nose supports
    (nose_outsideNostril) -[:LINE]-> (lips_cupidBowPeak),
    (nose_outsideNostril) -[:LINE]-> (lips_corner),

    // Lips
    (lips_corner) -[:LINE]-> (lips_cupidBowPeak),
    (lips_cupidBowPeak) -[:LINE]-> (inline_upperLipMiddle),
    (lips_corner) -[:LINE]-> (inline_lowerLipMiddle),

    // Lips supports
    (lips_corner) -[:LINE]-> (outline_jowl),
    (lips_corner) -[:LINE]-> (inline_bottomMiddle)
