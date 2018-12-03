
### Unit tests for ChemoSpecUtils

context("chkArgs") ####################

# Get some pca data for testing
pca1 <- prcomp(USArrests)
pca2 <- princomp(USArrests)
pca2 <- ChemoSpec:::.r2qPCA(pca2) # add "conPCA" to the class vector

# Simple test function; ALWAYS call with ALL arguments
tf <- function(spectra, pca, mode) {
	.chkArgs(mode)	
}

if (requireNamespace("ChemoSpec", quietly = TRUE)) {
	library("ChemoSpec")
	data(metMUD1)

# Mode 0

	test_that("chkArgs detects missing Spectra or Spectra2D object mode 0", {
	  expect_error(tf(12, 12, 0))
	})

	test_that("chkArgs recognizes Spectra object mode 0", {
	  expect_silent(tf(metMUD1, 12, 0))
	})

# Mode 11

	test_that("chkArgs detects missing spectra object mode 11", {
	  expect_error(tf(12, 12, 11))
	})

	test_that("chkArgs detects wrong class of spectra argument mode 11", {
	  expect_error(tf(pca1, 12, 11))
	})

	test_that("chkArgs recognizes Spectra object mode 11", {
	  expect_silent(tf(metMUD1, 12, 11))
	})

# Mode 12

	test_that("chkArgs detects missing spectra object mode 12", {
	  expect_error(tf(12, 12, 12))
	})

	test_that("chkArgs detects missing pca object mode 12", {
	  expect_error(tf(metMUD1, 12, 12))
	})

	test_that("chkArgs detects args in wrong order mode 12", {
	  expect_error(tf(pca1, metMUD1, mode = 12))
	})	

	test_that("chkArgs accepts classical pca input", {
	  expect_silent(tf(metMUD1, pca1, mode = 12))
	})	

	test_that("chkArgs accepts robust pca input", {
	  expect_silent(tf(metMUD1, pca2, mode = 12))
	})	

# Mode 13

	test_that("chkArgs detects missing spectra object mode 13", {
	  expect_error(tf(12, 12, 13))
	})

	test_that("chkArgs detects missing pca object mode 13", {
	  expect_error(tf(metMUD1, 12, 13))
	})

	test_that("chkArgs detects wrong order of args mode 13", {
	  expect_error(tf(pca1, metMUD1, 13))
	})

	test_that("chkArgs recognizes Spectra object mode 13", {
	  expect_silent(tf(metMUD1, 12, 11))
	})
	
} # end of ChemoSpec chkArgs tests

if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
	library("ChemoSpec2D")
	data(MUD1)
	set.seed(123)
	pfac <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 1)
	mia <- miaSpectra2D(MUD1)
	
# Mode 0

	test_that("chkArgs recognizes Spectra2D object mode 0", {
	  expect_silent(tf(MUD1, 12, 0))
	})

# Mode 21
	
	test_that("chkArgs detects missing spectra object mode 21", {
	  expect_error(tf(12, 12, 21))
	})

	test_that("chkArgs detects wrong class of spectra argument mode 21", {
	  expect_error(tf(pca1, 12, 21))
	})

	test_that("chkArgs accepts Spectra2D object mode 21", {
	  expect_silent(tf(MUD1, 12, 21))
	})

# Mode 22

	test_that("chkArgs detects missing spectra object mode 22", {
	  expect_error(tf(MUD1, 12, 22))
	})

	test_that("chkArgs detects missing pca object mode 22", {
	  expect_error(tf(MUD1, 12, 22))
	})

	test_that("chkArgs detects args in wrong order mode 22", {
	  expect_error(tf(pca1, MUD1, mode = 22))
	})	

	# test_that("chkArgs accepts Spectra2D object & mia results mode 22", {
	  # expect_silent(tf(MUD1, mia, 22))
	# })

	# test_that("chkArgs accepts Spectra2D object & parafac results mode 22", {
	  # expect_silent(tf(MUD1, pfac, 22))
	# })
	
	test_that("chkArgs rejects classical pca mode 22", {
	  expect_error(tf(MUD1, pca1, 22))
	})

# Mode 23

	test_that("chkArgs detects missing Spectra2D object mode 23", {
	  expect_error(tf(12, 12, 23))
	})

	test_that("chkArgs detects missing pca object mode 23", {
	  expect_error(tf(MUD1, 12, 23))
	})

	test_that("chkArgs detects args in wrong order mode 23", {
	  expect_error(tf(pca1, MUD1, mode = 23))
	})	

	# test_that("chkArgs accepts Spectra2D object & mia results mode 23", {
	  # expect_silent(tf(MUD1, mia, 23))
	# })

	# test_that("chkArgs accepts Spectra2D object & parafac results mode 23", {
	  # expect_silent(tf(MUD1, pfac, 23))
	# })

	test_that("chkArgs rejects classical pca mode 23", {
	  expect_error(tf(MUD1, pca1, 23))
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




