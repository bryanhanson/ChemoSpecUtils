# File created by roxut; edit the function definition file, not this file

# Test found in chkSpectra.Spectra2D.R:8 (file:line)
  
### Unit tests for chkSpectra.Spectra2D
load("tiny2D.RData")
tiny2D_NAc <- tiny2D
tiny2D_NAc$data[[1]][,4] <- NA # single spectrum with col of NAs
tiny2D_NAc$data[[2]][,3] <- NA 

tiny2D_NAr <- tiny2D
tiny2D_NAr$data[[1]][3,] <- NA # single spectrum with row of NAs
# chkSpectra detects mismatched col NAs
expect_error(chkSpectra(tiny2D_NAc), "NAs are present in the data but differ between samples")

# chkSpectra detects mismatched row NAs
expect_error(chkSpectra(tiny2D_NAr), "NAs are present in the data but differ between samples")

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
