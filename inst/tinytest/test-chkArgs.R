# File created by roxut; edit the function definition file, not this file

# Test found in chkArgs.R:13 (file:line)
  
# Get some pca data for testing
pca1 <- prcomp(USArrests) # class prcomp
pca2 <- princomp(USArrests) # class princomp

# Simple test function; ALWAYS call with ALL arguments
tf <- function(spectra, pca, mode) {
  .chkArgs(mode)	
}

## ChemoSpec Instances

if (requireNamespace("ChemoSpec", quietly = TRUE)) {
  library("ChemoSpec")
	 data(metMUD1)

  # Mode 0
  # 1st arg is garbage
  expect_error(tf(12, 12, 0), "did not have class Spectra or Spectra2D")

  # Mode 11
  # 1st arg is garbage
  expect_error(tf(12, 12, 11), "was not found or not a Spectra object")
  # Wrong arg in 1st position
  expect_error(tf(pca1, 12, 11), "was not found or not a Spectra object")

  # Mode 12
  # 1st arg is garbage
  expect_error(tf(12, 12, 12), "was not found or not a Spectra object")
  # 2nd arg is garbage
  expect_error(tf(metMUD1, 12, 12), "was not found or did not have class prcomp")
  # 1st and 2nd arg reversed
  expect_error(tf(pca1, metMUD1, 12), "was not found or not a Spectra object")
  # 2nd arg wrong class
  expect_error(tf(metMUD1, pca2, 12), "was not found or did not have class prcomp")
} # end of ChemoSpec chkArgs tests

## ChemoSpec2D Instances

if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
  library("ChemoSpec2D")
  data(MUD1)
  set.seed(123)
  pfac <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 1)
  mia <- miaSpectra2D(MUD1)
  pop <- popSpectra2D(MUD1)

  # Mode 0 (same as mode 0 above, no need to check)

  # Mode 21
  # 1st arg is garbage
  expect_error(tf(12, 12, 21), "was not found or not a Spectra2D object")
  # Wrong arg in 1st position
  expect_error(tf(pca1, 12, 21), "was not found or not a Spectra2D object")

  # Mode 22
  # 2nd arg is garbage
  expect_error(tf(MUD1, 12, 22), "was not found or did not have class mia/pfac/pop")
  # 1st and 2nd arg reversed
  expect_error(tf(pca1, MUD1, 22), "was not found or not a Spectra2D object")
  # 2nd arg wrong class
  expect_error(tf(MUD1, pca1, 22), "was not found or did not have class mia/pfac/pop")
} # end of ChemoSpec2D chkArgs tests

## Crossover checks

if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
  if (requireNamespace("ChemoSpec", quietly = TRUE)) {
    library("ChemoSpec2D")
    data(MUD1)
    library("ChemoSpec")
    data(metMUD1)
    # Spectra object passed to ChemoSpec2D
    expect_error(tf(metMUD1, 12, 21), "was not found or not a Spectra2D object")
    # Spectra2D object passed to ChemoSpec
    expect_error(tf(MUD1, 12, 11), "was not found or not a Spectra object")
  }
} # end of crossover checks
