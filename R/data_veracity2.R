.is.int <- function(x, tolerance = .Machine$double.eps){
	#####################################################################################################
	# - Logical function that checks if a number is an integer within machine precision
	# - x         > input number
	# - tolerance > tolerance threshold for determining integer quality
	#####################################################################################################

	abs(x - round(x)) < tolerance
}

##---------------------------------------------------------------------------------------------------------
## 
## Functions for Nested Model F-testing
##
##---------------------------------------------------------------------------------------------------------

#' Function that computes F-test for nested (ANOVA) models
#'
#' @param meanVec numeric vector containing means for each factor level
#' @param sdVec numeric vector containing standard deviations for each factor level
#' @param n.fl (numeric) integer indicating cell size/nr. of replicates per factor level
#' @return A list with delta F and associated p-value.
#' @details
#' \itemize{
#' 	\item Assumes only summary/descriptive statistics are available (from publication under investigation)
#'	\item Assumes ANOVA-type setting with one-way factorial design
#'	\item Assumes the ANOVA is balanced (nr. of replicates same for each factor level)
#'	\item Assumes the input vectors are ordered according to linearity of effect, e.g., 
#'	  meanVec[1] < meanVec[2] < ... < meanVec[k]
#'	\item Compares reduced linear model with more complex model bearing k - 1 regression parameters
#'	\item Null hypothesis: perfect linearity in underlying means
#'	\item Function based on whistleblower report in JF case
#'  \item Unmodified function from UvA report by Peeters, Klaasen, and van de Wiel (2015)
#' }
#' @references
#' Peeters, C. F. W., Klaasen, C. A. J., and van de Wiel, M. A. (2015).  \emph{Evaluating the Scientific Veracity of Publications by dr. Jens Förster.}  Retrieved from \href{http://retractionwatch.files.wordpress.com/2014/04/report_foerster.pdf}{Retraction Watch website}.
#' @export
deltaF.linear <- function(meanVec, sdVec, n.fl){
	if (class(meanVec) != "numeric"){
		stop("Input (meanVec) is of wrong class")
	}
	else if (class(sdVec) != "numeric"){
		stop("Input (sdVec) is of wrong class")
	}
	else if (any(sdVec < 0)){
		stop("Input (sdVec) cannot contain negative entries")
	}
	else if (length(meanVec) != length(sdVec)){
		stop("Length input (sdVec) should equal length input (meanVec)")
	}
	else if (class(n.fl) != "numeric" & class(n.fl) != "integer"){
		stop("Input (n.fl) is of wrong class")
	} 
	else if (length(n.fl) != 1){
		stop("Input (n.fl) should be a scalar")
	} 
	else {
		# Obtain needed quantities
		k      <- length(meanVec)
		grandM <- mean(meanVec)
		SS.reg <- (n.fl * (meanVec[k] - meanVec[1])^2)/2
		SSb    <- n.fl * sum((meanVec - grandM)^2)
		MSw    <- sum((sdVec)^2)/k

		# Calculate deltaF and its p-value
		deltaF   <- (SSb - SS.reg)/(k - 2) * 1/MSw
		deltaF.p <- 1 - pf(deltaF, (k - 2), k*(n.fl - 1))	
	
		# Return
		return(list(deltaF = deltaF, deltaF.p = deltaF.p))
	}
}

#' Function that performs Fisher's combined probability test
#'
#' @param pvalVec a vector containing p-values (ideally stemming from 'deltaF.linear')
#' @param plot boolean specifying whether to produce a plot
#' @param x.label character specifying the label for the x-axis, default = "p-values"; only when plot = TRUE
#' @param main character specifying the main title, default = ""; only when plot = TRUE
#' @return single scalar value
#' @details Only use to combine results from independent tests that
#' regard the same (null) hypothesis. The blue line in the plot
#' represents expectation under uniform behavior.  Function was
#' included without modification from UvA report by Peeters, Klaasen,
#' and van de Wiel (2015).
#' @references
#' Peeters, C. F. W., Klaasen, C. A. J., and van de Wiel, M. A. (2015).  \emph{Evaluating the Scientific Veracity of Publications by dr. Jens Förster.}  Retrieved from \href{http://retractionwatch.files.wordpress.com/2014/04/report_foerster.pdf}{Retraction Watch website}.
#' @export
Fisher.method <- function(pvalVec, plot = FALSE, x.label = "p-values", main = ""){
	# Dependencies
	# require("base")
	# require("stats")
	# require("graphics")
	if (class(pvalVec) != "numeric"){
		stop("Input (pvalVec) is of wrong class")
	}
	else if (class(plot) != "logical"){
		stop("Input (plot) is of wrong class")
	} 
	else {
		# Fisher's Method
		FM <- pchisq(-2 * sum(log(pvalVec)), df = 2 * length(pvalVec)) 

		# Plot
		if (plot){
			if (class(x.label) != "character"){
				stop("Input (x.label) is of wrong class")
			} else if (class(main) != "character"){
				stop("Input (main) is of wrong class")
			} else {
				hist(pvalVec, xlim = c(0,1), breaks = seq(0,1, l = 6), xlab = x.label, main = main)
				abline(h = length(pvalVec)/5, xlim = c(0,1), col = "blue", lwd = 2)
			}
		}

		# Return
		return(FM)
	}
}


##---------------------------------------------------------------------------------------------------------
## 
## Functions for the Evidential Value
##
##---------------------------------------------------------------------------------------------------------

#' Function that computes an evidential value in favor of dependent observations versus independence
#' 
#' @param meanVec numeric vector containing means for each factor level
#' @param sdVec   numeric vector containing standard deviations for each factor level
#' @param n.fl    (numeric) integer indicating cell size/nr. of replicates per factor level
#' @return single-element vector with V statistic, or a list with upper and lower limits for V
#' @details
#' Function was included without modification from UvA report by Peeters, Klaasen,
#' and van de Wiel (2015).
#' \itemize{
#' \item Assumes only summary/descriptive statistics are available (from publication under investigation)
#' \item Assumes ANOVA-type setting with one-way factorial design
#' \item Assumes there are 3 factor levels
#' \item Assumes the ANOVA is balanced (nr. of replicates same for each factor level)
#' \item Assumes following restriction on group means: mu1 \item 2*mu2 + mu3 = 0
#' \item Assumes the input vectors are ordered according to linearity of effect, e.g., meanVec[1] < meanVec[2]  < meanVec[3]
#' \item Assumes dependence between the measurement errors undermines (the assumption of) veracity
#' \item Computes the evidential value in favor of the hypothesis of a dependence-structure in the underlying
#' data (i.e., correlated measurement errors, indicating incorrect data collection: H_f) versus the
#' hypothesis of independence (the standard ANOVA assumption: H_i)
#' \item The evidential value as calculated is defined in Klaassen (2015).
#' }
#' @references
#' Peeters, C. F. W., Klaasen, C. A. J., and van de Wiel, M. A. (2015).  \emph{Evaluating the Scientific Veracity of Publications by dr. Jens Förster.}  Retrieved from \href{http://retractionwatch.files.wordpress.com/2014/04/report_foerster.pdf}{Retraction Watch website}.
#'
#' Klaassen, C.A.J. (2015). \emph{Evidential Value in ANOVA-Regression Results in Scientific Integrity Studies}. \href{http://arxiv.org/pdf/1405.4540v2}{Arxiv: 1405.4540v2}.
#' @export
evi.val <- function(meanVec, sdVec, n.fl){
	# Dependencies
	# require("base")

	if (class(meanVec) != "numeric"){
		stop("Input (meanVec) is of wrong class")
	}
	else if (length(meanVec) != 3){
		stop("Length input (meanVec) is expected to be 3")
	}
	else if (class(sdVec) != "numeric"){
		stop("Input (sdVec) is of wrong class")
	}
	else if (any(sdVec < 0)){
		stop("Input (sdVec) cannot contain negative entries")
	}
	else if (length(meanVec) != length(sdVec)){
		stop("Length input (sdVec) should equal length input (meanVec)")
	}
	else if (class(n.fl) != "numeric" & class(n.fl) != "integer"){
		stop("Input (n.fl) is of wrong class")
	} 
	else if (length(n.fl) != 1){
		stop("Input (n.fl) should be a scalar")
	} 
	else {
		# Obtain needed quantities
		vars <- sdVec^2
		z    <- meanVec[1] - 2 * meanVec[2] + meanVec[3]
    el1  <- (2 * sdVec[2] - (sdVec[1] + sdVec[3]))^2
    el2  <- (2 * sdVec[2] - sqrt(vars[1] + vars[3]))^2
		S0   <- vars[1] + 4 * vars[2] + vars[3]
		SL   <- min(el1, el2)
		nz2  <- n.fl * z^2

		# Compute evidential value
		if (SL <= nz2 & nz2 <= S0){
			EV <- sqrt(S0/nz2) * exp((-nz2/2) * ((1/nz2) - (1/S0)))
			return(EV)
		}
		if (SL >= nz2){
		  EV.l <- sqrt(S0/SL) * exp((-nz2/2) * ((1/SL) - (1/S0)))
		  EV.u <- sqrt(S0/nz2) * exp((-nz2/2) * ((1/nz2) - (1/S0)))
		  return(list(EV.lower = EV.l, EV.upper = EV.u))
		}
		if (S0 <= nz2){
			EV <- 1
			return(EV)
		}
	}
}

#' Function that computes overall evidential value
#' @param EVvals a vector containing multiple evidential values (ideally stemming from 'evi.val')
#' @return Product of all elements in \code{EVvals}
#' @details Computes overall evidential value by simple product of
#' evidential values (of individual studies).  When, from 'evi.val' we
#' have that SL >= nz2, the evidential value has a range, it is
#' recommended to use the minimum value for computation of the overall
#' EV.
#' 
#' Function was included without modification from UvA report by Peeters, Klaasen,
#' and van de Wiel (2015).
#' @references
#' Peeters, C. F. W., Klaasen, C. A. J., and van de Wiel, M. A. (2015).  \emph{Evaluating the Scientific Veracity of Publications by dr. Jens Förster.}  Retrieved from \href{http://retractionwatch.files.wordpress.com/2014/04/report_foerster.pdf}{Retraction Watch website}.
#'
#' Klaassen, C.A.J. (2015). \emph{Evidential Value in ANOVA-Regression Results in Scientific Integrity Studies}. \href{http://arxiv.org/pdf/1405.4540v2}{Arxiv: 1405.4540v2}.
#' @export
evi.valMult <- function(EVvals){
	# Dependencies
	# require("base")

	if (class(EVvals) != "numeric"){
		stop("Input (EVvals) is of wrong class")
	}
	else {
		# Compute overall evidential value
		return(prod(EVvals))
	}
}
