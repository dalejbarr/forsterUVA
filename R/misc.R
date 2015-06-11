#' Convert continuous to interval data
#'
#' @param x the continuous data
#' @param boundaries boundaries defining how continuous variable is
#' mapped to scale levels
#' @return vector with discrete ratings
#' @details Splits up a continuous scale into a discrete scale.  The
#' scale will have a number of levels equal to one more than the
#' number of elements in \code{boundaries}.  The default behavior is
#' to create an interval scale with upper and lower boundaries at -3
#' and 3 respectively.  This function is a fairly simple wrapper for
#' \code{\link{cut}}.
#' @aliases discretise
#' @seealso \code{link{cut}}
#' @export
discretize <- function(x,
                       boundaries = seq(-3, 3, length.out = 4)) {
    cut(x, c(-Inf, boundaries, +Inf), labels = FALSE)
}

discretise <- discretize

#' Wrapper function for calculating delta F
#'
#' @param x A numeric vector (or row from a data frame) containing means, standard deviations, and sample size per group as named elements \code{m1}, \code{m2}, \code{m3} and \code{s1}, \code{s2}, and \code{s3}, and \code{N}, respectively.
#' @param df_format Should results be returned as a data frame? (Useful when calling the function from \code{\link[dplyr]{do}}).
#' @return p-value for delta F (single numeric value)
#' @seealso \code{\link{deltaF.linear}}
#' @details This function wraps \code{\link{deltaF.linear}}, returning the associated p-value.
#' @export
deltaFp <- function(x, df_format = FALSE) {
    x2 <- as.numeric(x[c("m1", "m2", "m3", "s1", "s2", "s3")])
    N <- x[["N"]]
    if (df_format) {
        data.frame(`pdF`=deltaF.linear(x2[1:3], x2[4:6], N)[["deltaF.p"]])
    } else {
        deltaF.linear(x2[1:3], x2[4:6], N)[["deltaF.p"]]
    }
}

#' Wrapper function for calculating Klaassen's V (evidential value)
#'
#' @param x A numeric vector (or row from a data frame) containing means, standard deviations, and sample size per group as named elements \code{m1}, \code{m2}, \code{m3} and \code{s1}, \code{s2}, and \code{s3}, and \code{N}, respectively.
#' @param df_format Should results be returned as a data frame? (Useful when calling the function from \code{\link[dplyr]{do}}).
#' @return V, or the lower bound of the range for V
#' @seealso \code{\link{evi.val}}
#' @details This function wraps \code{\link{evi.val}}, returning the associated V statistic or lower bound if a range is returned.
#' @export
eviVal <- function(x, df_format = FALSE) {
    x2 <- as.numeric(x[c("m1", "m2", "m3", "s1", "s2", "s3")])
    N <- x[["N"]]
    res <- evi.val(x2[1:3], x2[4:6], N)
    ## if it comes back as a list, return lower bound
    if (is.list(res)) {
        estat <- res[["EV.lower"]]
    } else {
        estat <- res
    }
    if (df_format) {
        data.frame(V = estat)
    } else {
        c(V = estat)
    }
}

#' Combine multiple delta-F p values using Fisher method
#'
#' @param x a data frame with at least one variable containing p-values from \code{deltaF.linear}
#' @param varname name of variable containing p-values
#' @param df_format Should results be returned as a data frame?
#' @return either a single p-value or data.frame containing p-value
#' @details Just a simple wrapper of \code{Fisher.method}
#' @seealso \code{\link{Fisher.method}}
#' @export
combine_pdF <- function(x, df_format = FALSE, varname = "pdF") {
    res <- Fisher.method(x[[varname]])
    if (df_format) {
        return(data.frame(p = res))
    } else {}
    return(res)
}

#' Combine multiple V statistics by taking the product
#'
#' @param x a data frame with at least one variable containing V statistics from \code{deltaF.linear}
#' @param varname name of variable containing V statistics
#' @param df_format Should results be returned as a data frame?
#' @return either a single V stat or data.frame containing V stat
#' @details Just a simple wrapper of \code{evi.valMult}
#' @seealso \code{\link{evi.valMult}}
#' @export
combine_V <- function(x, df_format = FALSE, varname = "V") {
    res <- evi.valMult(x[[varname]])
    if (df_format) {
        return(data.frame(V_prod = res))
    } else {}
    return(res)
}
