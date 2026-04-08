#' @title Outlier Detection Using Tukey's IQR Method
#'
#' @description
#' This function detects outliers in a numeric vector using Tukey's
#' Interquartile Range (IQR) method. It calculates the first quartile (Q1),
#' third quartile (Q3), and the IQR to determine lower and upper bounds.
#' Observations outside these bounds are classified as outliers and their
#' indices are returned.
#'
#' @param x A numeric vector. The data on which outlier detection is performed.
#' @param k A numeric scalar. The threshold multiplier used to define the
#' lower and upper bounds. Default is \code{1.5}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{alt_sinir}: Lower bound.
#'   \item \code{ust_sinir}: Upper bound.
#'   \item \code{aykiri_degerler}: Detected outlier values.
#'   \item \code{indisler}: Indices of the outliers.
#' }
#'
#' @seealso
#' \code{\link[stats:quantile]{quantile}},
#' \code{\link[stats:IQR]{IQR}}
#'
#' @examples
#' # Example 1: Normally distributed data
#' set.seed(123)
#' veri1 <- rnorm(100, mean = 50, sd = 10)
#' tespit_et_outlier(veri1)
#'
#' # Example 2: Data with a clear outlier
#' veri2 <- c(10, 12, 11, 13, 12, 14, 15, 11, 12, 200)
#' tespit_et_outlier(veri2)
#'
#' # Example 3: More sensitive threshold
#' tespit_et_outlier(veri2, k = 1)
#'
#' @importFrom stats quantile IQR
#' @export
#' 
tespit_et_outlier<-function(x,k=1.5){
  if (!is.numeric(x)){
    stop ("x sayisal bir vektor olmalidir.")
  }
  if(!is.numeric(k)||length(k)!=1||is.na(k)||k<=0){
    stop("k pozitif, tek bir sayisal deger olmalidir.")
  }
  Q1=as.numeric(quantile(x, probs=0.25,na.rm=TRUE,names=FALSE))
  Q3=as.numeric(quantile(x, probs=0.75,na.rm=TRUE,names=FALSE))
  IQR_deger=IQR(x,na.rm=TRUE)
  alt_sinir=Q1-k*IQR_deger
  ust_sinir=Q3+k*IQR_deger
  
  indisler=which(!is.na(x)& (x<alt_sinir|x>ust_sinir))
  aykiri_degerler=x[indisler]
  return(list(
    alt_sinir=alt_sinir,
    ust_sinir=ust_sinir,
    aykiri_degerler=aykiri_degerler,
    indisler=indisler
  ))
}