# Static game data

Data in this directory represent common server state used at server
startup. The data are stored as s-expressions of the form

    (type key1: val1 ...)

where *type* is the name of an entity type (that is an abstract
game-data type) and each key/value pair initializes a field of the
data object.

In the current version of the Fabric, all entities are represented as
tagged property lists; the *type* element is the tag; the tail of the
list is the property list that defines the object's fields.

In this way we can store entities as s-expressions that the server
reads on startup, and we can store the in-memory versions of objects
directly in an embedded object database as lists.

