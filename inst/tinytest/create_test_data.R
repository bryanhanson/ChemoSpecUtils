
# Script to create "tiny2D", a tiny Spectra2D object for testing
# This is for the record; it will not be run during testing

tiny2D <- vector("list")
tiny2D$F2 <- as.numeric(1:10)
tiny2D$F1 <- as.numeric(1:5)
tiny2D$data <- list( # frontal slabs of all 1's, all 2's, all 3's
	Sample_1 = matrix(6.0, nrow = 5, ncol = 10),
	Sample_2 = matrix(4.0, nrow = 5, ncol = 10),
	Sample_3 = matrix(2.0, nrow = 5, ncol = 10)
	)
tiny2D$names <- paste("Sample", 1:3, sep = "_")
tiny2D$groups <- factor(rep("A", 3))
tiny2D$colors <- rep("black", 3)
tiny2D$units <- c("ppm", "ppm", "intensity")
tiny2D$desc <- "tiny2D data set"
class(tiny2D) <- "Spectra2D"
chkSpectra(tiny2D)
