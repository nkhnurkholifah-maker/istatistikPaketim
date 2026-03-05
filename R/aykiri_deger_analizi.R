aykiri_deger_analizi<-function(veri,grafik=FALSE){
  #1) Giris Kontrolu
  if (!is.numeric(veri)||!is.vector(veri)){
    stop("Hata: Lütfen sayısal bir veri giriniz!")
  }
  #NA Kontrolu
  if(any(is.na(veri))) {
    warning("Uyarı: Veride eksik değerler bulundu, bunlar analiz dıșı bırakıldı.")
    veri=veri[!is.na(veri)]
  }
  #2) Hesaplama (Tukey Yöntemi)
  Q1=as.numeric(quantile(veri,0.25,name=FALSE,type=7))
  Q3=as.numeric(quantile(veri,0.75,name=FALSE,type=7))
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
#test1
x<-c(10,12,13,15,18,17,14,16)
aykiri_deger_analizi(x,grafik=TRUE)
#test2
x<-c(10,12,13,15,18,17,14,16,200)
aykiri_deger_analizi(x,grafik=TRUE)
#test3
x<-c(10,12,13,15,NA,17,14,16)
aykiri_deger_analizi(x,grafik=TRUE)
#test4
x<-c(10,12,13,15,c,17,14,16)
aykiri_deger_analizi(x,grafik=TRUE)