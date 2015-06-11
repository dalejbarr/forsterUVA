#' List of publications investigated in UvA Förster report
#'
#' A dataset listing the publications and their classification.
#'
#' @format A data frame with 24 rows and 2 columns: \describe{
#' \item{pub}{Unique identifying code for the publication}
#' \item{section}{Classifcation of the publication in the report}
#' }
"pubs"

#' List of studies investigated in UvA Förster report
#'
#' A dataset listing the studies (experiments) within the publications
#'
#' @format A data frame with 209 rows and 3 columns:
#' \describe{
#' \item{pub}{Unique identifying code for the publication}
#' \item{study}{Identifying code for the study (unique within the publication)}
#' \item{N}{Total number of observations}
#' }
"studies"

#' Statistics of studies investigated in UvA Förster report
#'
#' A dataset with the statistics for the studies investigated
#'
#' @format A data frame with 209 rows and 8 columns:
#' \describe{
#' \item{pub}{Unique identifying code for the publication}
#' \item{study}{Identifying code for the study (unique within the publication)}
#' \item{m1}{Mean for group 1 (lowest mean)}
#' \item{m2}{Mean for group 2 (middle mean)}
#' \item{m3}{Mean for group 3 (highest mean)}
#' \item{s1}{Standard deviation for group 1}
#' \item{s2}{Standard deviation for group 2}
#' \item{s3}{Standard deviation for group 3}
#' }
"study_stats"

#' Findings for studies investigated in UvA Förster report
#'
#' A dataset with the findings for each of the studies in the report
#'
#' \describe{
#' \item{pub}{Unique identifying code for the publication}
#' \item{study}{Identifying code for the study (unique within the publication)}
#' \item{dF}{Delta F}
#' \item{p(dF)}{p-value from the delta-F test}
#' \item{EV}{Evidential Value}
#' \item{EV.up}{Evidential Value upper limit}
#' }
"findings"
