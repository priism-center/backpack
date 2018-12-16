#' Seeds Dataset
#'
#' Measurements of geometrical properties of kernels belonging to three 
#' different varieties of wheat. Adapted from the dataset taken from the 
#' UCI Machine Learning Repository.
#'
#' @docType data
#'
#' @usage data(seeds)
#'
#' @format A dataframe of 210 observations of 8 variables.
#' #' \describe{
#'   \item{area}{area of kernel}
#'   \item{perimeter}{perimeter of kernel}
#'   \item{compact}{compactness of kernel given by C = 4*pi*A/P^2}
#'   \item{length}{length of kernel}
#'   \item{width}{width of kernel}
#'   \item{asymm}{asymmetry coefficient}
#'   \item{groove}{length of kernel groove}
#'   \item{variety}{variety of wheat: 1 = Kama, 2 = Rosa and 3 = Canadian}
#'
#' @keywords datasets
#'
#' @source \href{http://archive.ics.uci.edu/ml/datasets/seeds?ref=datanews.io}
#' {UCI Machine Learning Repository}
#' 
"seeds"