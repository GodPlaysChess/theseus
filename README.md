Theseus is the simple purely functional graph library. (currently in development)

The idea is
- to make effective traversals and lazy graphs based on scalaz.zio
- for the simplest non-io graphs - make simple functional API.



IGraph - stands for IO graph. Minimum amount of node identifiers is given,
content of the node is IO and the edges are IO.

GGraph - stands for Geometry graph. I called it that way, since the geometry of such graph is known, but
the content of the Nodes can contain expensive computation (hence IO)

Graph - normal graph (so far is small-graph), with known contents and edges