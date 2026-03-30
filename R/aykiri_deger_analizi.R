#' @importFrom graphics boxplot abline
aykiri_deger_analizi<-function(veri,grafik=FALSE){
  #1) Giris Kontrolu
  if (!is.numeric(veri)||!is.vector(veri)){
    stop("Hata: Lutfen sayisal bir veri giriniz!")
  }
  #NA Kontrolu
  if(any(is.na(veri))) {
    warning("Uyari: Veride eksik degerler bulundu, bunlar analiz disi birakildi.")
    veri=veri[!is.na(veri)]
  }
  #2) Hesaplama (Tukey Yöntemi)
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
  cat("\t\tAykırı Değer Analizi\n")
  cat("******************************************************\n")
  cat("Toplam gözlem (NA haric):", length(veri),"\n", sep = "")
  cat("Q1:",Q1,"\t||Q3:",Q3,"\t||IQR:",IQR_deger,"\n", sep = "")
  cat("Alt sınır\t\t:",alt_sinir,"\n", sep = "")
  cat("Üst sınır\t\t:",ust_sinir,"\n", sep = "")
  cat("Aykırı değer sayısı\t:", length(aykiri),"\n", sep = "")
  if(length(aykiri)>0) {
    cat ("Aykırı değerler\t\t:",paste(aykiri, collapse=","),"\n", sep = "")
  }else{
    cat("Aykırı değerler\t\t:Yok\n")
  }
  cat("******************************************************\n")
  
  #5)Grafik
  if(isTRUE(grafik)){
    boxplot(veri,main="Boxplot(Tukey Aykırı Değer Analizi)",ylab="Değer")
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

#' @title Tukey IQR Yöntemi ile Aykırı Değer Tespiti
#'
#' @description
#' Bu fonksiyon, bir sayısal veri vektörü üzerinde Tukey'nin çeyrekler arası
#' açıklık (Interquartile Range, IQR) yöntemini kullanarak aykırı değerleri
#' tespit eder. İlk çeyrek (Q1), üçüncü çeyrek (Q3) ve çeyrekler arası açıklık
#' (IQR) hesaplanarak alt ve üst sınırlar belirlenir. Bu sınırların dışında
#' kalan gözlemler aykırı değer olarak sınıflandırılır ve indeksleri ile birlikte
#' raporlanır.
#'
#' @param x A numeric vector. Aykırı değer analizi uygulanacak sayısal veri vektörüdür.
#' @param k A numeric scalar. Alt ve üst sınırların belirlenmesinde kullanılan
#' eşik katsayısıdır. Varsayılan değer \code{1.5}'tir.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{alt_sinir}: Hesaplanan alt sınır değeri.
#'   \item \code{ust_sinir}: Hesaplanan üst sınır değeri.
#'   \item \code{aykiri_degerler}: Vektörde tespit edilen aykırı değerler.
#'   \item \code{indisler}: Aykırı değerlerin vektördeki indeksleri.
#' }
#'
#' @seealso
#' \code{\link[stats:quantile]{quantile}},
#' \code{\link[grDevices:boxplot.stats]{boxplot.stats}}
#'
#' @examples
#' # Örnek 1: Normal dağılıma yakın veri
#' set.seed(123)
#' veri1 <- rnorm(100, mean = 50, sd = 10)
#' tespit_et_outlier(veri1)
#'
#' # Örnek 2: Kesin aykırı değer içeren veri
#' veri2 <- c(10, 12, 11, 13, 12, 14, 15, 11, 12, 200)
#' tespit_et_outlier(veri2)
#'
#' # Örnek 3: Daha hassas eşik
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