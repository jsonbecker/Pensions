compound<- function(start,rate,timespan){
  x <-vector(mode = 'numeric', length = timespan)
  for(i in 1:timespan){
    if(i == 1){
      x[i]<-start
    }
    else{
      x[i]<-x[i-1]*(1+rate)
    }
  }
  return(x)
}
inflate<-function(start, inflation){
  x<-vector(mode='numeric', length=dim(inflation)[1])
  for(i in 1:dim(inflation)[1]){
    if(i==1){
      x[i]<-start
    }
    else{
      x[i]<-x[i-1]*(1+(1.25*(inflation[i,2]/100)))
    }
  }
  return(x)
}
cpiu<-cbind(seq(from=1992,to=2011),c(0.0,2.8,2.4,2.6,2.8,2.4,1.4,2.1,3.4,2.8,2.1,2.8,3.5,3.6,3.6,2.6,4.0,0.0,2.0,3.0))
inflation<-data.frame(cbind(cpiu[,1],inflate(25000,cpiu),compound(25000,.05,20),compound(25000,.06,20)))
names(inflation)<-c('year','NECPI.U','FivePercent','SixPercent')
png(filename="inflation.png", height=480, width=640, bg="white")
par(mar=c(6,5,5,3))
plot(inflation$NECPI.U, type='o', col=rgb(0,0.5,0), ylim=c(20000,80000), axes=FALSE, ann=FALSE, lwd=1.5)
axis(1, at=1:20, lab=inflation$year)
axis(2, las=1, at=seq(from=20000, to=80000, by=10000))
lines(inflation$FivePercent, type="o", pch=22, lty=2, col=rgb(0,0,0.5), lwd=1.5)
lines(inflation$SixPercent, type="o", pch=23, lty=2, col='red', lwd=1.5)
title(main="COLA or Raise?\n CPI-U v. Pension COLAs in Providence", col.main="black")
title(xlab="Year")
title(ylab="Annual Pension in Dollars\n")
legend(1, 80000, c('CPI-U NE + 25%', 'Five Percent', 'Six Percent'), col=c('green', 'blue', 'red'), pch=21:23, lty=1:3)
text(1,25000, 25000, pos=3, col='black')
text(20, max(inflation$SixPercent), round(max(inflation$SixPercent), 0), pos=3, col='red')
text(20, max(inflation$FivePercent), round(max(inflation$FivePercent), 0),pos=3, col=rgb(0,0,0.5))
text(20, max(inflation$NECPI.U), round(max(inflation$NECPI.U), 0), pos=3, col=rgb(0,0.5,0))
dev.off()
