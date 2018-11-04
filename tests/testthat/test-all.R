
### Unit tests for ChemoSpecUtils

context("chkArgs") ####################

# Get some data to test with
pca <- prcomp(USArrests, scale = TRUE)

# Simple test function ALWAYS call with ALL arguments
tf <- function(spectra, pca, mode) {
	.chkArgs(mode)	
}

if (requireNamespace("ChemoSpec", quietly = TRUE)) {
	library("ChemoSpec")
	data(metMUD1)
	
	test_that("chkArgs detects missing spectra object mode 11", {
	  expect_error(tf(12, 12, 11))
	})

	test_that("chkArgs detects wrong class of spectra argument mode 11", {
	  expect_error(tf(pca, 12, 11))
	})

	test_that("chkArgs detects missing spectra object mode 12", {
	  expect_error(tf(12, 12, 12))
	})

	test_that("chkArgs detects missing pca object mode 12", {
	  expect_error(tf(metMUD1, 12, 12))
	})

	test_that("chkArgs detects args in wrong order mode 12", {
	  expect_error(tf(pca, metMUD1, mode = 12))
	})	
	
} # end of ChemoSpec chkArgs tests

if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
	library("ChemoSpec2D")
	data(MUD1)
	
	test_that("chkArgs detects missing spectra object mode 21", {
	  expect_error(tf(12, 12, 21))
	})

	test_that("chkArgs detects wrong class of spectra argument mode 21", {
	  expect_error(tf(pca, 12, 21))
	})

	test_that("chkArgs detects missing spectra object mode 22", {
	  expect_error(tf(12, 12, 22))
	})

	test_that("chkArgs detects missing pca object mode 22", {
	  expect_error(tf(MUD1, 12, 22))
	})

	test_that("chkArgs detects args in wrong order mode 22", {
	  expect_error(tf(pca, MUD1, mode = 22))
	})	
	
} # end of ChemoSpec2D chkArgs tests

####################

# A tiny Spectra2D object for testing
tiny <- vector("list")
tiny$F2 <- as.numeric(1:10)
tiny$F1 <- as.numeric(1:5)
tiny$data <- list( # frontal slabs of all 1's, all 2's, all 3's
	Sample_1 = matrix(6.0, nrow = 5, ncol = 10),
	Sample_2 = matrix(4.0, nrow = 5, ncol = 10),
	Sample_3 = matrix(2.0, nrow = 5, ncol = 10)
	)
tiny$names <- paste("Sample", 1:3, sep = "_")
tiny$groups <- factor(rep("A", 3))
tiny$colors <- rep("black", 3)
tiny$units <- c("ppm", "ppm", "intensity")
tiny$desc <- "Tiny data set"
class(tiny) <- "Spectra2D"
chkSpectra(tiny)

context("chkSpectra + .findNA") ####################

tiny_NAc <- tiny
tiny_NAc$data[[1]][,4] <- NA # single spectrum with col of NAs

test_that("chkSpectra detects mismatched col NAs", {
  expect_error(chkSpectra(tiny_NAc))
})

tiny_NAr <- tiny
tiny_NAr$data[[1]][3,] <- NA # single spectrum with row of NAs

test_that("chkSpectra detects mismatched row NAs", {
  expect_error(chkSpectra(tiny_NAr))
})

tiny_NAc$data[[2]][,4] <- NA 
tiny_NAc$data[[3]][,4] <- NA 

test_that(".findNA reports col NAs correctly", {
  expect_equal(.findNA(tiny_NAc)$colNA, 4)
})

tiny_NAr$data[[2]][3,] <- NA 
tiny_NAr$data[[3]][3,] <- NA 

test_that(".findNA reports row NAs correctly", {
  expect_equal(.findNA(tiny_NAr)$rowNA, 3)
})

context("chkSpectra") ####################

tiny_NAmr <- tiny
M <- tiny_NAmr$data[[1]]
M <- M[-1,]
tiny_NAmr$data[[1]] <- M

test_that("chkSpectra detects matrices with differing no. rows", {
  expect_error(chkSpectra(tiny_NAmr))
})

tiny_NAmc <- tiny
M <- tiny_NAmc$data[[1]]
M <- M[,-1]
tiny_NAmc$data[[1]] <- M

test_that("chkSpectra detects matrices with differing no. cols", {
  expect_error(chkSpectra(tiny_NAmc))
})

tiny_NAmm <- tiny
tiny_NAmm$data[[1]][,6] <- NA
tiny_NAmm$data[[2]][,5] <- NA

test_that("chkSpectra detects matrices with NAs in different positions", {
  expect_error(chkSpectra(tiny_NAmm))
})




