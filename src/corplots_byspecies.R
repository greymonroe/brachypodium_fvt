traitlist<-traits[-which(traits=="Relative_WC")]
traitnames<-real_trait_names[-which(real_trait_names=="RWC")]

panel.cor.function <- function(x, y){
  trait1<-alldata[,grep(x, colnames(alldata))]
  trait2<-alldata[,grep(y, colnames(alldata))]
  corlist<-c()
  for (i in 1:20){
    co<-cor(trait1[,i],trait2[,i])
    corlist<-c(corlist, co)
  }
  #plot(seq(0.3, 1, length.out = 20), corlist, xlab="Final soil moisture content\n(% gravimetric capacity", ylab="Pearson's Correlation Coefficient (r)", main=paste(x,y, sep=" vs. "))
  #plot(seq(0.3, 1, length.out = 20), corlist, xlab="", ylab="", ylim=c(-1, 1), cex=1.5, pch=20)
  #lines(seq(0.3, 1, length.out = 20), corlist, type="l")
  #abline(h=0, lty=2, col="gray")
  plot.new()
}

panel.cor.function.species <- function(x, y){
  trait1<-alldata[1:5,grep(x, colnames(alldata))]
  trait2<-alldata[1:5,grep(y, colnames(alldata))]
  corlist<-c()
  for (i in 1:20){
    co<-cor.test(trait1[,i],trait2[,i])$estimate
    corlist<-c(corlist, co)
  }
  #plot(seq(0.3, 1, length.out = 20), corlist, xlab="Final soil moisture content\n(% gravimetric capacity", ylab="Pearson's Correlation Coefficient (r)", main=paste(x,y, sep=" vs. "))
  plot(seq(0.3, 1, length.out = 20)*100, corlist, xlab="", ylab="", ylim=c(-1, 1), cex=.5, pch=20, col="dodgerblue")
  lines(seq(0.3, 1, length.out = 20)*100, corlist, type="l", col="dodgerblue", lwd=.5)
  abline(h=0, lty=2, col="gray",lwd=.5)
  
  trait1<-alldata[6:10,grep(x, colnames(alldata))]
  trait2<-alldata[6:10,grep(y, colnames(alldata))]
  corlist<-c()
  for (i in 1:20){
    co<-cor(trait1[,i],trait2[,i])
    corlist<-c(corlist, co)
  }
  #plot(seq(0.3, 1, length.out = 20), corlist, xlab="Final soil moisture content\n(% gravimetric capacity", ylab="Pearson's Correlation Coefficient (r)", main=paste(x,y, sep=" vs. "))
  lines(seq(0.3, 1, length.out = 20)*100,cex=.5, corlist, type="p",pch=20, col="orange2")
  lines(seq(0.3, 1, length.out = 20)*100, corlist, type="l", col="orange2",lwd=.5)
  abline(h=0, lty=2, col="gray",lwd=.5)
}

greypairs<-function(x){
  par(mfrow=c(length(x),length(x)), mai=c(0.10,0.10,0.05,0.05), mgp=c(0.5,0.5,0), oma=c(2,2,0,0))
  for (i in 1:length(x)){
    if(i>1) for (j in (1):(i-1)) {panel.cor.function.species(x[i], x[j])}
    plot(1, type="n", axes=F, xlab="", ylab="")
    text(1,1,traitnames[i], cex=1)
    if(i<length(x)) for (j in (i+1):length(x)){ panel.cor.function(x[i], x[j])}
  }
}

pdf("../Figures/corsplots_byspecies.pdf", height=7, width=7, pointsize = 8)
greypairs(traitlist)
mtext(expression('Soil moisture (%'[final]*')'), side=1, line=1, outer=TRUE)
mtext("Pearson's r", side=2, line=.8, outer=TRUE)

dev.off()
