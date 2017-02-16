/* FUTURE CHANGES TO CONSIDER
 * - Create point along centre vertical between bottom lip and bottom middle
 * - Add eye supports to the top and bottom of the eye?
 *     + Seems like it would help show the structure around the eye better,
 *       but may allow for some funky transformations. Probably should.
 */


// Example users
CREATE (voidUser:User {username: 'void', created_at:TIMESTAMP()}),
       (nullUser:User {username: 'null', created_at:TIMESTAMP()})

// Example faces
CREATE (rootFace:Face {name:'rootFace', created_at:TIMESTAMP()}),
       (newFace:Face {name:'newFace', created_at:TIMESTAMP()}),
       (nextNewFace:Face {name:'nextNewFace', created_at:TIMESTAMP()}),
       (modNewFace:Face {name:'modNewFace', created_at:TIMESTAMP()}),
       (modNewFaceByNullUser:Face {name:'modNewFaceByNullUser', created_at:TIMESTAMP()})

// Edges between example faces
CREATE
  (rootFace) -[:CHILD {isFork:true}]-> (newFace),
  (newFace) -[:CHILD {isFork:false}]-> (nextNewFace), // direct successor, no forking
  (nextNewFace) -[:CHILD {isFork:true}]-> (modNewFace), // forked, unowned
  (nextNewFace) -[:CHILD {isFork:true}]-> (modNewFaceByNullUser), // forked, owned by nullUser
  (voidUser) -[:CREATED]-> (newFace),
  (nullUser) -[:CREATED]-> (modNewFaceByNullUser),
  (voidUser) -[:PUBLISHED]-> (modNewFace)

// Give newFace a dummy starting point
CREATE (p0_new:Point)
CREATE (newFace) -[:STARTS_AT]-> (p0_new)

// Network Control Plane
// - RIP
// - OSPF
//
// Manchester Coding
// Async serial
