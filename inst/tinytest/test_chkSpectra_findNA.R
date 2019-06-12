
### Unit tests for chkSpectra + findNA in ChemoSpecUtils

load("tiny2D.RData")

tiny2D_NAc <- tiny2D
tiny2D_NAc$data[[1]][,4] <- NA # single spectrum with col of NAs

# chkSpectra detects mismatched col NAs
expect_error(chkSpectra(tiny2D_NAc), "NAs are present in the data but differ between samples")

tiny2D_NAr <- tiny2D
tiny2D_NAr$data[[1]][3,] <- NA # single spectrum with row of NAs

# chkSpectra detects mismatched row NAs
expect_error(chkSpectra(tiny2D_NAr), "NAs are present in the data but differ between samples")

tiny2D_NAc$data[[2]][,4] <- NA 
tiny2D_NAc$data[[3]][,4] <- NA 

# .findNA reports col NAs correctly
expect_equal(.findNA(tiny2D_NAc)$colNA, 4)

tiny2D_NAr$data[[2]][3,] <- NA 
tiny2D_NAr$data[[3]][3,] <- NA 

# .findNA reports row NAs correctly"
expect_equal(.findNA(tiny2D_NAr)$rowNA, 3)

