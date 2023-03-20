#Tugas Teknik Simulasi
#Multiplicative_RNG dan Bernoulli_1
#NIM GENAP
#KELAS B
#Kamalina ROsyida Supriyanti

multiplicative_RNG<-function(a,z0,m,n) {
  xb<-matrix(NA,n,3)
  colnames(xb)<-c("aZ","Xb","Ub")
  for(b in 1:n)
  {
    xb[b,1]<-(a*z0)
    xb[b,2]<-xb[b,1]%%m
    xb[b,3]<-xb[b,2]/m
    z0<-xb[b,2]
  }
  hist(xb[,3])
  View(xb)
}
multiplicative_RNG(45,21139,417,150)

Bernoulli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}

Bernoulli_1(150,0.83)