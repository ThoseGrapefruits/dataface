// !!! WARNING: RUNNING THIS WILL CLEAR THE DATABASE !!!

// Clear existing relations.
MATCH () -[c:STARTS_AT|LINE|CHILD|CREATED|PUBLISHED]-> () DELETE c;

// Clear existing nodes
MATCH (n) WHERE n:Face OR n:Point OR n:User DELETE n;

// TODO
