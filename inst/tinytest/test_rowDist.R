# File created by roxut; edit the function definition file, not this file
 
M1 <- matrix(c(0.0, 1.0, 0.0, 1.0), nrow = 2, byrow = TRUE) # parallel/colinear vectors
M2 <- matrix(c(0.0, 1.0, 0.0, -1.0), nrow = 2, byrow = TRUE) # anti-parallel vectors
M3 <- matrix(c(0.0, 1.0, 1.0, 0.0), nrow = 2, byrow = TRUE) # orthogonal vectors

possMeth <- c("cosine", "pearson", "abspearson", "correlation", "abscorrelation", "spearman", "kendall",
                "euclidean", "maximum", "manhattan", "canberra", "binary")

boundMeth1 <- c("abspearson", "abscorrelation")
boundMeth2 <- c("pearson", "correlation", "cosine")

# Check that all distances by any method are positive
for (i in 1:length(possMeth)) {
  expect_true(all(rowDist(M1, possMeth[i]) >= 0.0))
  expect_true(all(rowDist(M2, possMeth[i]) >= 0.0))
  expect_true(all(rowDist(M3, possMeth[i]) >= 0.0))
}

# Check that distances bounded on [0...1] are actually bounded on [0...1]
for (i in 1:length(boundMeth1)) {
  expect_true((all(rowDist(M1, boundMeth1[i]) >= 0.0)) & (all(rowDist(M1, boundMeth1[i]) <= 1.0)))
  expect_true((all(rowDist(M2, boundMeth1[i]) >= 0.0)) & (all(rowDist(M2, boundMeth1[i]) <= 1.0)))
  expect_true((all(rowDist(M3, boundMeth1[i]) >= 0.0)) & (all(rowDist(M3, boundMeth1[i]) <= 1.0)))
}

# Check that distances bounded on [0...2] are actually bounded on [0...2]
for (i in 1:length(boundMeth2)) {
  expect_true((all(rowDist(M1, boundMeth2[i]) >= 0.0)) & (all(rowDist(M1, boundMeth2[i]) <= 2.0)))
  expect_true((all(rowDist(M2, boundMeth2[i]) >= 0.0)) & (all(rowDist(M2, boundMeth2[i]) <= 2.0)))
  expect_true((all(rowDist(M3, boundMeth2[i]) >= 0.0)) & (all(rowDist(M3, boundMeth2[i]) <= 2.0)))
}
