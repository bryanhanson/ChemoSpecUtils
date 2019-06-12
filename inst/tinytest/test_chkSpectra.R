
### Unit tests for chkSpectra in ChemoSpecUtils

## Selected Spectra2D checks

load("tiny2D.RData")

tiny2D_NAmr <- tiny2D
M <- tiny2D_NAmr$data[[1]]
M <- M[-1,]
tiny2D_NAmr$data[[1]] <- M

# chkSpectra detects matrices with differing no. rows
expect_error(chkSpectra(tiny2D_NAmr), "Data matrices do not have the same dimensions")

tiny2D_NAmc <- tiny2D
M <- tiny2D_NAmc$data[[1]]
M <- M[,-1]
tiny2D_NAmc$data[[1]] <- M

# chkSpectra detects matrices with differing no. cols
expect_error(chkSpectra(tiny2D_NAmc), "Data matrices do not have the same dimensions")

tiny2D_NAmm <- tiny2D
tiny2D_NAmm$data[[1]][,6] <- NA
tiny2D_NAmm$data[[2]][,5] <- NA

# chkSpectra detects matrices with NAs in different positions
expect_error(chkSpectra(tiny2D_NAmm), "NAs are present in the data but differ between samples")




