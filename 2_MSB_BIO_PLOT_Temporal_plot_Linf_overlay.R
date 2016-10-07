# rm(list=ls())
library(colorRamps)
linf <- read.csv('summary_par_VB_walkG.csv')
head(linf)

numyears <- length(min(dd.age$YR):max(dd.age$YR))


linf2 <- linf[-c(1:5),]
head(linf2)

# numyears * 12

linf3 <- linf2[c(1:(numyears * 14)),]
head(linf3)
tail(linf3)


# remove population ave estiamtes
# linf2 <- linf[-c(1:5),]
# head(linf2)

# numyears * 14

# linf3 <- linf2[497:980,]
# head(linf3)
# tail(linf3)

################# PLOT ############################

year <- min(dd.age$YR):max(dd.age$YR)


res <- 6
name_figure <- "Lake_temporal_kBCI.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1
axissize <- 1
x.label = 'Year'
y.label = expression(paste(L[infinity]))


n.panel=12 #num of pannels per plot


nf <- layout(matrix(c(1:12),nrow=3,ncol=4,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.0,0.05,0.05,0) )

# lake-specific plot
# (1-1)*numyears+1
# (2-1)*numyears+1
# (3-1)*numyears+1
# 
# (1-1)*numyears+1 + numyears-1
# (2-1)*numyears+1+ numyears-1
# (3-1)*numyears+1+ numyears-1

# linf3[((2-1)*numyears+1) : ((2-1)*numyears+1 + numyears-1),2]

# linf3[((1-1)*numyears+1) : ((1-1)*numyears+1 + numyears-1),2]

for(i in 1:n.panel){
  
  plot(year,linf3[1:length(year),1], ylim=c(min(linf3$lower),max(linf3$upper)),
       xlim=c(min(year),max(year)), axes=F, ylab='', xlab='', type='n')
  
  points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col='black',type='o', lwd=2 )
  points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),3], col='black',type='l', lwd=2 )
  points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),4], col='black',type='l',lwd=2 )
  
  
  if( i <=7){
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F ) 
  } else {
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
  }	
  
  if( i ==1 | i==5 | i==9 ){
    axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1)
  } else {
    axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
  }	
  
  
  box()
  
}

mtext(y.label, line = 1.5, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()


 ######## NO CIS
res <- 6
name_figure <- "Lake_temporal_k.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1
axissize <- 1
x.label = 'Year'
y.label = expression(paste('K (',year^-1,')'))


n.panel=12 #num of pannels per plot


nf <- layout(matrix(c(1:8),nrow=4,ncol=2,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(3,4,0,1),mai=c(0.0,0.05,0.05,0) )

# lake-specific plot
# (1-1)*numyears+1
# (2-1)*numyears+1
# (3-1)*numyears+1
# 
# (1-1)*numyears+1 + numyears-1
# (2-1)*numyears+1+ numyears-1
# (3-1)*numyears+1+ numyears-1

# linf3[((2-1)*numyears+1) : ((2-1)*numyears+1 + numyears-1),2]

# linf3[((13-1)*numyears+1) : ((13-1)*numyears+1 + numyears-1),2]

for(i in c(1,2,5,6,7,9,13,14)){
  
  plot(year,linf3[1:length(year),1], ylim=c(min(linf3$HPD),0.7),
       xlim=c(min(year),max(year)), axes=F, ylab='', xlab='', type='n')
  
  points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col='black',type='o', lwd=2 )
  # points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),3], col='black',type='l', lwd=2 )
  # points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),4], col='black',type='l',lwd=2 )
  # 
  
  if( i <=9){
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F ) 
  } else {
    axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
  }	
  
  if( i ==1 |i==3| i==5 | i==7 |i==13 ){
    axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1)
  } else {
    axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
  }	
  
  
  box()
  
}

mtext(y.label, line = 2.2, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()




library(colorRamps)
######## NO CIS - OVERLAY
res <- 6
name_figure <- "temporal_k_overlay.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1
axissize <- 1
x.label = 'Year'
y.label = expression(paste('K (',year^-1,')'))


gcols <- blue2red(8)

# n.panel=12 #num of pannels per plot

lwd1 <-1.5

nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(2.0,3,0,0.5),mai=c(0.0,0.05,0.05,0) )


# for(i in c(1,2,5,6,7,9,13,14)){
  
  plot(year,linf3[1:length(year),1], ylim=c(min(linf3$HPD),0.7),
       xlim=c(min(year),max(year)), axes=F, ylab='', xlab='', type='n')
  
  points(year, linf3[((1-1)*numyears+1) : ((1-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[1],type='l', lwd=lwd1 )
  points(year, linf3[((2-1)*numyears+1) : ((2-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[2],type='l', lwd=lwd1 )
  points(year, linf3[((5-1)*numyears+1) : ((5-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[3],type='l', lwd=lwd1 )
  points(year, linf3[((6-1)*numyears+1) : ((6-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[4],type='l', lwd=lwd1 )
  points(year, linf3[((7-1)*numyears+1) : ((7-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[5],type='l', lwd=lwd1 )
  points(year, linf3[((9-1)*numyears+1) : ((9-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[6],type='l', lwd=lwd1)
  points(year, linf3[((13-1)*numyears+1) : ((13-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[7],type='l', lwd=lwd1)
  points(year, linf3[((14-1)*numyears+1) : ((14-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[8],type='l', lwd=lwd1)
  # points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),3], col='black',type='l', lwd=2 )
  # points(year, linf3[((i-1)*numyears+1) : ((i-1)*numyears+1 + numyears-1),4], col='black',type='l',lwd=2 )
  # 
  
#   if( i <=9){
#     axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F ) 
#   } else {
#     axis(side=1,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01)
#   }	
#   
#   if( i ==1 |i==3| i==5 | i==7 |i==13 ){
#     axis(side=2,cex.axis=axissize , mgp=c(0,0.3,0),tck= -0.01, las=1)
#   } else {
#     axis(side=2,cex.axis=axissize , mgp=c(1,0,0),tck= -0.01, labels=F)
#   }	
  axis(side=1,cex.axis=axissize , mgp=c(1,0.4,0),tck= -0.01)
  axis(side=2,cex.axis=axissize , mgp=c(0,0.35,0),tck= -0.01, las=1)
  
  box()
  
# }
  
  legend(1978, 0.70, c("Lake 1" ,
                    "Lake 2" ,
                    "Lake 3" ,
                    "Lake 4" ,
                    "Lake 5" ,
                    "Lake 6",
                    "Lake 7",
                    "Lake 8"),
         lty=c(1,1,1,1,1,1,1,1 ), col=c(gcols[1],gcols[2],gcols[3],gcols[4],gcols[5],gcols[6],gcols[7],gcols[8]),
         lwd=c(lwd1,lwd1,lwd1,lwd1,lwd1,lwd1))

mtext(y.label, line = 1.5, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()



########### PNG - SMALLER
######## NO CIS - OVERLAY
png(
  "Spatial_MAS.png",
  width     = 2,
  height    = 1.5,
  units     = "in",
  res       = 1200,
  pointsize = 4
)
def.par <- par(no.readonly = TRUE)

size.labels = 1.2
size.text = 1.4
axissize <- 1.2
x.label = 'Year'
y.label = expression(paste("MAS (",L[infinity], '; mm)'))


gcols <- blue2red(8)

# n.panel=12 #num of pannels per plot

lwd1 <-0.5

nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.0,0.1,0.1,0.1),oma=c(2.5,3.5,0.4,0.5),mai=c(0.0,0.05,0.0,0) )


# for(i in c(1,2,5,6,7,9,13,14)){

plot(year,linf3[1:length(year),1], ylim=c(min(linf3$HPD),1500),
     xlim=c(min(year),max(year)), axes=F, ylab='', xlab='', type='n')

points(year, linf3[((1-1)*numyears+1) : ((1-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[1],type='l', lwd=lwd1 )
points(year, linf3[((2-1)*numyears+1) : ((2-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[5],type='l', lwd=lwd1 )
points(year, linf3[((5-1)*numyears+1) : ((5-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[3],type='l', lwd=lwd1 )
points(year, linf3[((6-1)*numyears+1) : ((6-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[4],type='l', lwd=lwd1 )
# points(year, linf3[((7-1)*numyears+1) : ((7-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[5],type='l', lwd=lwd1 )
# points(year, linf3[((9-1)*numyears+1) : ((9-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[6],type='l', lwd=lwd1)
points(year, linf3[((13-1)*numyears+1) : ((13-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[7],type='l', lwd=lwd1)
points(year, linf3[((14-1)*numyears+1) : ((14-1)*numyears+1 + numyears-1),2], cex=0.8, pch=16,col=gcols[8],type='l', lwd=lwd1)

axis(side=1,cex.axis=axissize , mgp=c(1,0.4,0),tck= -0.01)
axis(side=2,cex.axis=axissize , mgp=c(0,0.35,0),tck= -0.01, las=1)

box()

# }

legend(1977, 1535, c("Lake 1" ,
                     "Lake 2" ,
                     "Lake 3" ,
                     "Lake 4" ,
                     "Lake 5"),
       lty=c(1,1,1,1,1,1 ), col=c(gcols[1],gcols[5],gcols[3],gcols[4],gcols[7],gcols[8]),
       lwd=c(lwd1,lwd1,lwd1,lwd1), cex=0.9)

mtext(y.label, line = 1.7, side = 2, cex = size.text,outer=T)
mtext(x.label, line = 1.5, side = 1, cex = size.text, outer=T)


par(def.par)
dev.off()

# 5, 6, 15

range(linf3[((15-1)*numyears+1) : ((15-1)*numyears+1 + numyears-1),2])


