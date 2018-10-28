#'
#' Check Arguments of Calls Involving Spectra or Spectra2D Objects
#' 
#' This function checks that the first argument passed is a \code{\link[ChemoSpec]{Spectra}
#' or a \code{Spectra2D} object as appropriate.  It can also check that the second argument
#' comes from an appropriate corresponding data reduction function.
#'
#' You must always call this function with argument mode specified!
#'
#' @param args A list of args passed to the function, captured typically via
#' \code{args <- as.list(match.call()[-1])} right before calling this function.
#'
#' @param mode Integer. One of:
#' \itemize{
#'   \item 11 Check if the first argument is a \code{Spectra} object.
#'   \item 12 Check if the first argument is a \code{Spectra} object and the second argument
#'            is of class \code{prcomp} or \code{princomp}.
#'   \item 21 Check if the first argument is a \code{Spectra2D} object.
#'   \item 22 Check if the first argument is a \code{Spectra2D} object and the second argument
#'            is of class \code{mia} or \code{parafac}.
#' }
#'
#' @return Nothing is returned.  Problems with arguments give warnings and then
#' a stop/error message.
#'
#' @export
#' @noRd
#'

.chkArgs <- function(args, mode) {
	
	mode <- as.integer(mode)
	if (!(mode %in% c(11, 12, 21, 22))) stop("Invalid mode")

##### Mode 11	
	if (mode == 11) {
		
		# See if R found the arguments (this is match.call's job, this does NOT
		# mean that the arguments have the correct type/class)
		
		specFound <- "spectra" %in% names(args)
		if (!specFound) stop("Could not find 'spectra' (1st) argument")
		
		# Now check the classes of the arguments
		# Must contend with get failing to find the object identified as the argument
		
		arg1 <- tryCatch(
			{get(as.character(args[["spectra"]]))},
			error = function(cond) {
				message("\nError message from R: ", cond$message, "\n")
				message("Could not find the object corresponding to argument 'spectra'")
				return(NA)
			}
		) # end of tryCatch arg1
		
		if (class(arg1) != "Spectra") stop("Argument 'spectra' (1st argument) was not a Spectra object")
				
	} # end of mode == 11

##### Mode 12	
	if (mode == 12) {
		
		trouble <- FALSE
		stopmsg <- "Be sure the spectra argument is first and the pca argument second"
		
		# See if R found the arguments (this is match.call's job, this does NOT
		# mean that the arguments have the correct type/class)
		
		specFound <- "spectra" %in% names(args)
		if (!specFound) {
			trouble <- TRUE
			warning("Could not find 'spectra' (1st) argument")
		}
		
		pcaFound <- "pca" %in% names(args)
		if (!pcaFound) {
			trouble <- TRUE
			warning("Could not find 'pca' (2nd) argument")
		}
		
		if (trouble) stop(stopmsg)
		
		# Now check the classes of the arguments
		# Must contend with get failing to find the object identified as the argument
		
		arg1 <- tryCatch(
			{get(as.character(args[["spectra"]]))},
			error = function(cond) {
				message("\nError message from R: ", cond$message, "\n")
				message("Could not find the object corresponding to argument 'spectra'")
				return(NA)
			}
		) # end of tryCatch arg1
		
		if (class(arg1) != "Spectra") {
			trouble <- TRUE
			warning("Argument 'spectra' (1st argument) was not a Spectra object")
		}
		if (!trouble) chkSpectra(arg1)
				
		if (!trouble) {
			arg2 <- tryCatch(
				{get(as.character(args[["pca"]]))},
				error = function(cond) {
					message("\nError message from R: ", cond$message, "\n")
					message("Could not find the object corresponding to argument 'pca'")
					return(NA)
				}
			) # end of tryCatch arg2
			pcaOK <- FALSE
			if ("prcomp" %in% class(arg2)) pcaOK <- TRUE
			if ("princomp" %in% class(arg2)) pcaOK <- TRUE
			if (!pcaOK) {
				warning("Argument 'pca' (2nd argument) must be a prcomp or princomp object")
				stop(stopmsg)
			}
		}
		
		if (trouble) stop(stopmsg) # just in case we fall through to here
		
	} # end of mode == 12

##### Mode 21	
	if (mode == 21) {
		
		# See if R found the arguments (this is match.call's job, this does NOT
		# mean that the arguments have the correct type/class)
		
		specFound <- "spectra" %in% names(args)
		if (!specFound) stop("Could not find 'spectra' (1st) argument")
		
		# Now check the classes of the arguments
		# Must contend with get failing to find the object identified as the argument
		
		arg1 <- tryCatch(
			{get(as.character(args[["spectra"]]))},
			error = function(cond) {
				message("\nError message from R: ", cond$message, "\n")
				message("Could not find the object corresponding to argument 'spectra'")
				return(NA)
			}
		) # end of tryCatch arg1
		
		if (class(arg1) != "Spectra2D") stop("Argument 'spectra' (1st argument) was not a Spectra2D object")
				
	} # end of mode == 21

##### Mode 22	
	if (mode == 22) {
		
		trouble <- FALSE
		stopmsg <- "Be sure the spectra argument is first and the pca argument second"
		
		# See if R found the arguments (this is match.call's job, this does NOT
		# mean that the arguments have the correct type/class)
		
		specFound <- "spectra" %in% names(args)
		if (!specFound) {
			trouble <- TRUE
			warning("Could not find 'spectra' (1st) argument")
		}
		
		pcaFound <- "pca" %in% names(args)
		if (!pcaFound) {
			trouble <- TRUE
			warning("Could not find 'pca' (2nd) argument")
		}
		
		if (trouble) stop(stopmsg)
		
		# Now check the classes of the arguments
		# Must contend with get failing to find the object identified as the argument
		
		arg1 <- tryCatch(
			{get(as.character(args[["spectra"]]))},
			error = function(cond) {
				message("\nError message from R: ", cond$message, "\n")
				message("Could not find the object corresponding to argument 'spectra'")
				return(NA)
			}
		) # end of tryCatch arg1
		
		if (class(arg1) != "Spectra2D") {
			trouble <- TRUE
			warning("Argument 'spectra' (1st argument) was not a Spectra2D object")
		}
		if (!trouble) chkSpectra(arg1)
				
		if (!trouble) {
			arg2 <- tryCatch(
				{get(as.character(args[["pca"]]))},
				error = function(cond) {
					message("\nError message from R: ", cond$message, "\n")
					message("Could not find the object corresponding to argument 'pca'")
					return(NA)
				}
			) # end of tryCatch arg2
			pcaOK <- FALSE
			if ("mia" %in% class(arg2)) pcaOK <- TRUE
			if ("parafac" %in% class(arg2)) pcaOK <- TRUE
			if (!pcaOK) {
				warning("Argument 'pca' (2nd argument) must be a mia or parafac object")
				stop(stopmsg)
			}
		}
		
		if (trouble) stop(stopmsg) # just in case we fall through to here
		
	} # end of mode == 22


} # end of chkArgs

