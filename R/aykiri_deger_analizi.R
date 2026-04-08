#' @title Outlier Analysis
#' @description
#' This function performs outlier analysis on a numeric vector using Tukey's method.
#' It can also produce a boxplot if requested.
#'
#' @param veri A numeric vector.
#' @param grafik A logical value. If TRUE, a boxplot is drawn.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{temizlenmis_veri}: The data after removing outliers.
#'   \item \code{aykiri_degerler}: The detected outlier values.
#'   \item \code{sinirlar}: A list containing the lower and upper bounds.
#'   \item \code{ozet}: A list containing Q1, Q3, and IQR summary values.
#' }
#'
#' @importFrom graphics boxplot abline
#' @importFrom stats quantile
#' @export

aykiri_deger_analizi<-function(veri,grafik=FALSE){
  #1) Giris Kontrolu
  if (!is.numeric(veri)||!is.vector(veri)){
    stop("Error: Please provide a numeric vector.")
  }
  #NA Kontrolu
  if(any(is.na(veri))) {
    warning("Warning: Missing values were found and removed from the analysis.")
    veri=veri[!is.na(veri)]
  }
  #2) Hesaplama (Tukey Method)
  Q1=as.numeric(quantile(veri,0.25,names=FALSE,type=7))
  Q3=as.numeric(quantile(veri,0.75,names=FALSE,type=7))
  IQR_deger=Q3-Q1
  alt_sinir<-Q1-1.5*IQR_deger
  ust_sinir<-Q3+1.5*IQR_deger
  
  #3) Islem
  aykiri=veri[veri<alt_sinir|veri>ust_sinir]
  temiz_veri=veri[veri>=alt_sinir&veri<=ust_sinir]
  
  #4) Raporlama
  cat("******************************************************\n")
  cat("\t\tOutlier Analysis\n")
  cat("******************************************************\n")
  cat("Total observations (excluding NA):", length(veri),"\n", sep = "")
  cat("Q1:",Q1,"\t||Q3:",Q3,"\t||IQR:",IQR_deger,"\n", sep = "")
  cat("Lower Limit\t\t:",alt_sinir,"\n", sep = "")
  cat("Upper Limit\t\t:",ust_sinir,"\n", sep = "")
  cat("Number of Outliers\t:", length(aykiri),"\n", sep = "")
  if(length(aykiri)>0) {
    cat ("Outliers\t\t:",paste(aykiri, collapse=","),"\n", sep = "")
  }else{
    cat("Outliers\t\t:Yok\n")
  }
  cat("******************************************************\n")
  
  #5)Grafik
  if(isTRUE(grafik)){
    boxplot(veri,main="Boxplot(Tukey Outliers Analysis)",ylab="Value")
    abline(h=c(alt_sinir,ust_sinir),lty=2)
  }
  
  #6)Return List
  return(invisible(list(
    temizlenmis_veri=temiz_veri,
    aykiri_degerler=aykiri,
    sinirlar=list(alt=alt_sinir,ust=ust_sinir),
    ozet=list(Q1=Q1,Q3=Q3,IQR=IQR_deger)
    )))
  
  
}