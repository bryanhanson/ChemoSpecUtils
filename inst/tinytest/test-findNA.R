# File created by roxut; edit the function definition file, not this file

# Test found in findNA.R:20 (file:line)
  
### Unit tests for .findNA
load("tiny2D.RData")

tiny2D_NAc <- tiny2D
tiny2D_NAc$data[[1]][,4] <- NA # single spectrum with col of NAs
tiny2D_NAc$data[[2]][,4] <- NA # more spectra with all NAs in col 4
tiny2D_NAc$data[[3]][,4] <- NA 
tiny2D_NAr <- tiny2D
tiny2D_NAr$data[[1]][3,] <- NA # single spectrum with row of NAs
tiny2D_NAr$data[[2]][3,] <- NA # more spectra with all NAs in row 3
tiny2D_NAr$data[[3]][3,] <- NA 

# .findNA reports col NAs correctly
expect_equal(.findNA(tiny2D_NAc)$colNA, 4)

# .findNA reports row NAs correctly"
expect_equal(.findNA(tiny2D_NAr)$rowNA, 3)
