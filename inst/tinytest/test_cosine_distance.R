### Unit tests for rowDist method = cosine in ChemoSpecUtils

# cosine similarity is bounded on c(-1, 1) so cosine distance is 1 - c(-1, 1)

M1 <- matrix(c(rep(0.5, 20)), nrow = 2, byrow = TRUE) # test matrix

M2 <- M1
M2[2,] <- 2 * M2[2,]
expect_equal(c(rowDist(M2, "cosine")), 0.0)

M3 <- M1
M3[2,] <- -1 * M3[2,]
expect_equal(c(rowDist(M3, "cosine")), 2.0)
