### Unit tests for rowDist method = cosine in ChemoSpecUtils

# cosine similarity is bounded on c(-1, 1) so cosine distance is 1 - c(-1, 1) = c(2, 0)
# From Wikipedia:
# "It is thus a judgment of orientation and not magnitude: two vectors with the same orientation
# have a cosine similarity of 1, two vectors oriented at 90° relative to each other have a
# similarity of 0, and two vectors diametrically opposed have a similarity of -1, independent
# of their magnitude."

# N.B. Function returns distance, not similarity!

M1 <- matrix(c(1.0, 1.0, 2.0, 2.0), nrow = 2, byrow = TRUE) # parallel/colinear vectors
expect_equal(c(rowDist(M1, "cosine")), 0.0)

M2 <- matrix(c(0.0, 1.0, 0.0, -1.0), nrow = 2, byrow = TRUE) # anti-parallel vectors
expect_equal(c(rowDist(M2, "cosine")), 2.0)

M3 <- matrix(c(0.0, 1.0, 1.0, 0.0), nrow = 2, byrow = TRUE) # orthogonal vectors
expect_equal(c(rowDist(M3, "cosine")), 1.0)
