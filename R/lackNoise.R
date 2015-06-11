##---------------------------------------------------------------------------------------------------------
## 
## Simulation function
##
##---------------------------------------------------------------------------------------------------------

lackNoise <- function(meanMat, sdMat, n.flVec, F.vec, seed = sample(1:1000000, 1), nrRuns, cat.data = FALSE,
                      lower.cat, upper.cat, dir = getwd(), nameExt = "", verbose = TRUE){
	#####################################################################################################
	# - Function that assesses, by simulation, excessiveness of linearity and similarity
	# - meanMat    > a matrix containing means. Rows assumed to represent individual studies while columns
	#                contain factor level means
	# - sdMat      > a matrix containing standard deviations. Rows assumed to represent individual studies 
	#                while columns contain factor level standard deviations
	# - n.flVec    > a vector representing the nr. of replicates for each factor level in each study
	# - F.vec      > a vector representing the reported omnibus F tests
	# - seed       > an (random) integer used to initialize simulations at a particular seed
	# - nrRuns     > a numeric (integer) indicating the number of simulations runs desired
	# - cat.data   > a logical indicating if original data are of the categorical (Likert) type, default = FALSE
	# - lower.cat  > a vector representing lowest reponse categories. Only if cat.data = TRUE
	# - upper.cat  > a vector representing highest response categories. Only if cat.data = TRUE
	# - dir        > specifies the directory in which the visual output is stored
	# - nameExt    > character giving extension of default output names.
	#                Circumvents overwriting of output when working in single directory
	# - verbose    > logical indicating if progress should be printed on-screen
	# 
	# - Notes:
	# - Assumes ANOVA-type setting with one-way factorial design
	# - Assumes there are 3 factor levels
	# - Assumes the ANOVA is balanced (nr. of replicates same for each factor level)
	# - Assumes the row-vectors of meanMat are ordered according to linearity of effect, e.g., 
	#   meanMat[1,1] < meanMat[1,2]  < meanMat[1,3]
	# - Simulations aim to recreate entire paper with all included studies
	# - Simulations adhere to exact linearity (of the control condition) in the population
	# - Simulations truncate the data draws to Likert-type data when cat.data = TRUE
	# - When n.flVec contains decimal numbers only the integer part is retained
	# - When publication reports averages of Likert-type variables, the function simulates data further
	#   from the normality assumption than needed
	# - Simulations then assess (un)likeliness of reported linearity across conditions 
	#   Linearity measure: average absolute distance between control condition and the midpoint between the 
	#   high and low conditions
	# - Simulations also assess (un)likeliness of stability of reported omnibus F-values
	#   Stability measure: standard deviation of omnibus F tests
	# - Result (assessment unlikeliness published results) dependent on the number of simulations runs
	# - Function based on ideas and code by Uri Simonsohn. See:
	#   + http://datacolada.org/2014/05/08/21-fake-data-colada/
	#   + http://opim.wharton.upenn.edu/~uws/BlogAppendix/Colada21.FakeDataColada.R
	#   + Simonsohn, U. (2013). Just Post it: The Lesson from Two Cases of Fabricated Data Detected by 
	#     Statistics Alone. Psychological Science, 24:1875-1888
	#####################################################################################################

	# Dependencies
	# require("base")
	# require("stats")

	if (class(meanMat) != "matrix"){
		stop("Input (meanMat) is of wrong class")
	}
	else if (dim(meanMat)[2] != 3){
		stop("Column dimension input (meanMat) is expected to be 3")
	}
	else if (class(sdMat) != "matrix"){
		stop("Input (sdMat) is of wrong class")
	}
	else if (any(sdMat < 0)){
		stop("Input (sdMat) cannot contain negative entries")
	}
	else if (!all(dim(meanMat) == dim(sdMat))){
		stop("Input (sdMat) should be of equal dimension to input (meanMat)")
	}
	else if (class(n.flVec) != "numeric" & class(n.flVec) != "integer"){
		stop("Input (n.flVec) is of wrong class")
	}
	else if (length(n.flVec) != dim(meanMat)[1]){
		stop("Length input (n.flVec) should equal row dimension of input (meanMat)")
	}
	else if (class(F.vec) != "numeric"){
		stop("Input (F.vec) is of wrong class")
	}
	else if (length(F.vec) != dim(meanMat)[1]){
		stop("Length input (F.vec) should equal row dimension of input (meanMat)")
	}
	else if (!.is.int(seed)){
		stop("Input (seed) is expected to be a (numeric) integer")
	}
	else if (length(seed) != 1){
		stop("Input (seed) is expected to be a scalar")
	}
	else if (!.is.int(nrRuns)){
		stop("Input (nrRuns) is expected to be a (numeric) integer")
	}
	else if (length(nrRuns) != 1){
		stop("Input (nrRuns) is expected to be a scalar")
	}
	else if (class(cat.data) != "logical"){
		stop("Input (cat.data) is of wrong class")
	} 
	else if (class(dir) != "character"){
		stop("Input (dir) is of wrong class")
	}
	else if (class(nameExt) != "character"){
		stop("Input (nameExt) is of wrong class")
	}
	else if (class(verbose) != "logical"){
		stop("Input (verbose) is of wrong class")
	}
	else {
		# Set and check basics
		set.seed(seed)
		sdF   <- numeric()
		MDist <- numeric()
		M.med <- (meanMat[,1] + meanMat[,3])/2

		if (cat.data){
			if (length(lower.cat) != dim(meanMat)[1]){
				stop("Length input (lower.cat) should equal row dimension of input (meanMat)")
			} else if (any(!.is.int(lower.cat))){
				stop("Input (lower.cat) is expected to consist of (numeric) integers")
			} else if (length(upper.cat) != dim(meanMat)[1]){
				stop("Length input (upper.cat) should equal row dimension of input (meanMat)")
			} else if (any(!.is.int(upper.cat))){
				stop("Input (upper.cat) is expected to consist of (numeric) integers")
			}
		}

		# Simulate
		for (i in 1:nrRuns){
		
			# Set temporary vectors
			Fobs  <- numeric()
			mdist <- numeric()

			# Run publication loop
			for (j in 1:dim(meanMat)[1]){

				# Draw data
				y.high <- rnorm(n.flVec[j], meanMat[j,3], sdMat[j,3])       
				y.low  <- rnorm(n.flVec[j], meanMat[j,1], sdMat[j,1])        
				y.med  <- rnorm(n.flVec[j], M.med[j], sdMat[j,2])        

				if (cat.data){
					# Bound data
					y.high <- pmin(y.high, upper.cat[j])
					y.high <- pmax(y.high, lower.cat[j])
					y.med  <- pmin(y.med, upper.cat[j])
					y.med  <- pmax(y.med, lower.cat[j])
					y.low  <- pmin(y.low, upper.cat[j])
					y.low  <- pmax(y.low, lower.cat[j])
    
					# Round data
					y.high <- round(y.high, 0)
					y.med  <- round(y.med, 0)
					y.low  <- round(y.low, 0)
				}

				# Data
				y = c(y.low, y.med, y.high)  

				# Run ANOVA
				dumL <- c(rep(1,n.flVec[j]), rep(0,2*n.flVec[j]))
				dumH <- c(rep(0,2*n.flVec[j]), rep(1,n.flVec[j]))
				Fc   <- lm(y ~ dumL + dumH)

				# Measures
				Fobs[j]  <- summary(Fc)$fstatistic[1]  
				mdist[j] <- abs((mean(y.high) + mean(y.low))/2 - mean(y.med))
			}

			# Collect summarized measures per simulated publication
  			MDist <- c(MDist, mean(mdist))
			sdF   <- c(sdF, sd(Fobs))

			# Verbose
			if (verbose){cat(paste(i, " of ", nrRuns, " simulations done", sep = ""), "\n")}
		}

		# Evaluation excessiveness linearity
		meanL <- mean(meanMat[,1])
		meanM <- mean(meanMat[,2])
		meanH <- mean(meanMat[,3])
		mDist.pub <- abs((meanH + meanL)/2 - meanM)

		pdf(paste(dir, "Deviance_from_linearity.", nameExt, ".pdf"))
		hist(MDist, xlim = c(0, max(MDist)), main = "", xlab = "Absolute distance from linearity", ylab = "Frequency")
		abline(v = mDist.pub, col = "red", lwd = 2)
		dev.off()

		MDextreme <- sum(MDist <= mDist.pub) 
		MDmin     <- min(MDist) 
      
		# Evaluation excessiveness stability F values
		sdF.pub <- sd(F.vec)

		pdf(paste(dir, "Similarity_F_values.", nameExt, ".pdf"))
		hist(sdF, xlim = c(0, max(sdF)), main = "", xlab = "Standard deviation of omnibus F-values", ylab = "Frequency")
		abline(v = sdF.pub, col = "red", lwd = 2)
		dev.off()

		sdFextreme <- sum(sdF <= sdF.pub)
		sdFmin     <- min(sdF)

		# Return
		return(list(seed.used = seed, nr.Sims.MDist.EQS.reported = MDextreme, min.MDist.Sims = MDmin, 
		       nr.Sims.sdF.EQS.reported = sdFextreme, min.sdF.Sims = sdFmin, cor.MDist.sdF = cor(MDist, sdF, method = "spearman")))
	}
}






##---------------------------------------------------------------------------------------------------------
## 
## NOTES
##
##---------------------------------------------------------------------------------------------------------

#- Focus is on one-way ANOVA-type studies 
#- As such, these methods befit the evaluation of data veracity for fields/studies that employ ANOVA-type analyses
#- Other options for the evaluation of data veracity might lie with the employment of Benford's law (see package 'BenfordTests')
#- Note, however, that fields such as behavioral and social sciences often have Likert-type data
#- Of interest might also be the Ioannidis and Trikalinos Test for Excess Significance










