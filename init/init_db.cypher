// !!! WARNING: RUNNING THIS WILL CLEAR THE DATABASE !!!

// Clear existing relations.
MATCH () -[c:STARTS_AT|LINK|CHILD|CREATED|PUBLISHED]-> () DELETE c;

// Clear existing nodes
MATCH (n) WHERE n:Face OR n:Point OR n:User DELETE n;

/*
CREATE CONSTRAINT ON (u:User)
    ASSERT exists(u.username)  // exists(...) assertion requires enterprise edition
*/

CREATE CONSTRAINT ON (u:User)
    ASSERT u.username IS UNIQUE
