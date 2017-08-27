####################################################
#IMPORT DATA####################################################
####################################################

setwd('~/Documents/phd/code')
options(max.print=10000000)
whiteref <- read.table('White Standard.txt')
setwd("~/Documents/phd/code/fertiliser2")
#pdf("PLS_Model_Fit.pdf") 
AllData <- data.frame() #set up data frame of data
DataNames <- list() #list the data
topFolders <- list.files() #list the files
catagories <- data.frame()
# Cycle through Folders 
for(iFo in 1:length(topFolders)) {
  # Cycle through files in each folder
  folderFiles <- list.files(path = paste("./", topFolders[iFo], sep = ""))
  for(iFi in 1:length(folderFiles)) {
    dat <- read.table(paste("./", topFolders[iFo], "/", folderFiles[iFi], sep = ""), sep = "", header = F, skip = 15, skipNul = TRUE)
    # dat <- apply (dat, 2, function(x) x*whiteref[,2]) #apply white reference
    AllData <- rbind(AllData, t(dat[,2]))
    DataNames <- rbind(DataNames, paste("", iFo, "", iFi, sep = ""))
    catagories <- rbind(catagories, iFo)
  }
}

rownames(AllData) <- DataNames

require(graphics)

#######################################################
######Set a color Palette
#######################################################

mycols <- adjustcolor(palette(rainbow(30)), alpha.f = 0.3)

palette(c(rgb(185,130,62, maxColorValue=255),
          rgb(137,93,181, maxColorValue=255),
          #rgb(100,157,97, maxColorValue=255),
          rgb(180,78,94, maxColorValue=255)))
palette()
pal<-palette()

maxy <- max(AllData[1:3,])
miny <- min(AllData[1:3,])


##########################################################################################
#INDICIES#################################################################################
##

rownames(AllData) <- DataNames
eighthundred <- AllData[,1444]
sixeighty <- AllData[,1142]
a <- eighthundred - sixeighty
b <- eighthundred+sixeighty
NDVI <- a/b
NDVI
plot(NDVI, col = unlist(catagories))
sNDVI <- split(NDVI, catagories)
boxplot(sNDVI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)


fourfifty <- AllData[,595]
c <- 6*sixeighty
d <- 7.5*fourfifty
e <- c+eighthundred
f <- d+1
g <- e-f
EVI <- a/g
plot(EVI, col = unlist(catagories))
plot(EVI, col = unlist(catagories))
sEVI <- split(EVI, catagories)
boxplot(sEVI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)


h <- sixeighty - fourfifty
i <- h*2
l <- eighthundred-i
m <- eighthundred+i
ARVI <- l/m
plot(ARVI, col = unlist(catagories))
sARVI <- split(ARVI, catagories)
boxplot(sARVI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

sevenfifty <- AllData[,1316]
seven05 <- AllData[,1203]
n <- sevenfifty - seven05
o <- sevenfifty + seven05
rededgeNDVI <- n/o
plot(rededgeNDVI, col = unlist(catagories))
srededgeNDVI <- split(rededgeNDVI, catagories)
boxplot(srededgeNDVI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

four45 <- AllData[,584]
p <- 2*four45
q <- o-p
modifiedrededgeNDVI <- n/q
plot(modifiedrededgeNDVI, col = unlist(catagories))
smodifiedrededgeNDVI <- split(modifiedrededgeNDVI, catagories)
boxplot(smodifiedrededgeNDVI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

r <- sevenfifty - four45
s <- seven05 - four45
modifiedrededgeSRI <- r/s
modifiedrededgeSRI
plot(modifiedrededgeSRI, col = unlist(catagories))
smodifiedrededgeSRI <- split(modifiedrededgeSRI, catagories)
boxplot(smodifiedrededgeSRI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

seven40 <- AllData[,1290]
seven20 <- AllData[,1240]
VOGREI <- seven40/seven20
VOGREI
plot(VOGREI, col = unlist(catagories))
sVOGREI <- split(VOGREI, catagories)
boxplot(sVOGREI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

seven34 <- AllData[,1275]
seven47 <- AllData[,1308]
t <- seven34-seven47
seven15 <- AllData[,1454]
seven26 <- AllData[,1255]
u <- seven15+seven26
VOGREI2 <- t/u
VOGREI2
plot(VOGREI2, col = unlist(catagories))
sVOGREI2 <- split(VOGREI2, catagories)
boxplot(sVOGREI2, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

v <- seven15 + seven20
VOGREI3 <- t/v
VOGREI3
plot(VOGREI3, col = unlist(catagories))
sVOGREI3 <- split(VOGREI3, catagories)
boxplot(sVOGREI3, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

five31 <- AllData[,527]
five70 <- AllData[,875]
w <- five31 - five70
x <- five31 + five70
PRI <- w/x
plot(PRI, col = unlist(catagories))
sPRI <- split(PRI, catagories)
boxplot(sPRI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)


five00 <- AllData [,711]
seven50 <- AllData [,1316]
y <- sixeighty - five00
PSRI <- y/seven50
PSRI
plot(PSRI, col = unlist(catagories))
sPSRI <- split(PSRI, catagories)
boxplot(sPSRI, outline=FALSE, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)




####################################################################
#spectra plotting
#################################################################
#############################################
##############################################

plot(1:2047, AllData[1,], type = "l", col = pal, ylim = c(100, -100))
for(i in 1:3) {
  lines(1:2047, AllData[i+1,], col = pal[i+1])
}
legend('bottomleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)

plot(1:2047, AllData[1,1:2047], type = "l", col = pal[1], ylim = c(-250, 250))
par(mfrow=c(2,2))
for(ip in 1:3) {
  plot(1:2047, AllData[1,1:2047], type = "l", col = pal[ip], ylim = c(-250, 250))
  for(i in 1:15) {
    lines(1:2047, AllData[i*ip,1:2047], col = pal[ip])
  }
}
#legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
par(mfrow=c(1,1))

##################################################################################################################################

plot(1:15, AllData[1, 1:15], type = "l", col = pal)
for(i in 1:3) {
  lines(1:15, AllData[i+1, 1:15], col = pal[i+1])
}
legend('bottomleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)

plot(1:15, AllData[1,1:15], type = "l", col = pal[1], ylim = c(0, 5))
par(mfrow=c(2,2))
for(ip in 1:3) {
  plot(1:15, AllData[1,1:15], type = "l", col = pal[ip], ylim = c(0, 5))
  for(i in 1:15) {
    lines(1:15, AllData[i*ip,1:15], col = pal[ip], ylim = c(0, 5))
  }
}

par(mfrow=c(1,1))


plot(1:459, AllData[1, 1:459], type = "l", col = pal)
for(i in 1:3) {
  lines(1:459, AllData[i+1, 1:459], col = pal[i+1])
}
legend('bottomleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)

plot(1:459, AllData[1,1:459], type = "l", col = pal[1], ylim = c(-100, 100))
par(mfrow=c(2,2))
for(ip in 1:3) {
  plot(1:459, AllData[1,1:459], type = "l", col = pal[ip], ylim = c(-100, 100))
  for(i in 1:15) {
    lines(1:459, AllData[i*ip,1:459], col = pal[ip], ylim = c(-100, 100))
  }
}
par(mfrow=c(1,1))

##################################################################################################################################

plot(460:1190, AllData[1, 460:1190], type = "l", col = pal[1])
par(mfrow=c(2,2))
for(ip in 1:3) {
  plot(460:1190, AllData[1, 460:1190], type = "l", col = pal[ip], ylim = c(-200, 100))
  for(i in 1:15) {
    lines(460:1190, AllData[i*ip, 460:1190], col = pal[ip], ylim = c(-200, 100))
  }
}
plot(460:1190, AllData[1, 460:1190], type = "l", col = pal[1])
par(mfrow=c(2,2))
for(ip in 1:3) {
  plot(460:1190, AllData[1, 460:1190], type = "l", col = pal[ip], ylim = c(-200, 100))
  for(i in 1:15) {
    lines(460:1190, AllData[i*ip, 460:1190], col = pal[ip], ylim = c(-200, 100))
  }
}

plot(1191:2047, AllData[1, 1191:2047], type = "l", col = pal[1])
par(mfrow=c(2,2))
for(ip in 1:3) {
  plot(1191:2047, AllData[1, 1191:2047], type = "l", col = pal[ip], ylim = c(-150, 150))
  for(i in 1:15) {
    lines(1191:2047, AllData[i*ip, 1191:2047], col = pal[ip], ylim = c(-150, 150))
  }
}

plot(1191:2047, AllData[1, 1191:2047], type = "l", col = pal[1])
par(mfrow=c(2,2))
for(ip in 1:4) {
  plot(1191:2047, AllData[1, 1191:2047], type = "l", col = pal[ip], ylim = c(-2000, 200))
  for(i in 1:30) {
    lines(1191:2047, AllData[i*ip, 1191:2047], col = pal[ip], ylim = c(-2000, 200))
  }
}

#legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
par(mfrow=c(1,1))

#################################################################


plot(1:2047, a[1,], type = "l", col = pal, ylim = c(-100, 100))
for(i in 1:30) {
  lines(1:2047, a[i+1,], col = pal[i+1])
}
plot(1:459, a[1,1:459], type = "l", col = pal)
for(i in 1:30) {
  lines(1:459, a[i+1,1:459], col = pal[i+1])
}
plot(460:1190, a[1,460:1190], type = "l", col = mycols[1], ylim = c(-50, 100))
for(i in 1:1190) {
  lines(460:1190, a[i+1,460:1190], col = pal[i+1])
}
plot(1191:2047, a[1,1191:2047], type = "l", col = mycols[1], ylim = c(-200, 70))
for(i in 1:30) {
  lines(1191:2047, a[i+1,1191:2047], col = pal[i+1])
}

####################################################################

plot(1:2047, b[1,], type = "l", col = pal, ylim = c(-200, 100))
for(i in 1:30) {
  lines(1:2047, b[i+1,], col = mycols[i+1])
}
plot(1:459, b[1,1:459], type = "l", col = pal)
for(i in 1:30) {
  lines(1:459, b[i+1,1:459], col = mycols[i+1])
}
plot(460:1190, b[1,460:1190], type = "l", col = mycols[1], ylim = c(-150, 50))
for(i in 1:30) {
  lines(460:1190, b[i+1,460:1190], col = mycols[i+1])
}
plot(1191:2047, b[1,1191:2047], type = "l", col = mycols[1], ylim = c(-500, 70))
for(i in 1:30) {
  lines(1191:2047, b[i+1,1191:2047], col = mycols[i+1])
}

####################################################################
plot(1:2047, c[1,], type = "l", col = pal, ylim = c(-200, 100))
for(i in 1:30) {
  lines(1:2047, c[i+1,], col = mycols[i+1])
}
plot(1:459, c[1,1:459], type = "l", col = pal)
for(i in 1:30) {
  lines(1:459, c[i+1,1:459], col = mycols[i+1])
}
plot(460:1190, c[1,460:1190], type = "l", col = mycols[1], ylim = c(-200, 50))
for(i in 1:30) {
  lines(460:1190, c[i+1,460:1190], col = mycols[i+1])
}
plot(1191:2047, c[1,1191:2047], type = "l", col = mycols[1], ylim = c(-500, 70))
for(i in 1:30) {
  lines(1191:2047, c[i+1,1191:2047], col = mycols[i+1])
}

####################################################################

plot(1:2047, d[1,], type = "l", col = pal, ylim = c(-100, 100))
for(i in 1:30) {
  lines(1:2047, d[i+1,], col = mycols[i+1])
}
plot(1:459, d[1,1:459], type = "l", col = pal, ylim = c(-1000, 500))
for(i in 1:30) {
  lines(1:482, d[i+1,1:482], col = mycols[i+1])
}
plot(460:1190, d[1,460:1190], type = "l", col = mycols[1], ylim = c(-100, 50))
for(i in 1:30) {
  lines(460:1190, d[i+1,460:1190], col = mycols[i+1])
}
plot(1191:2047, d[1,1191:2047], type = "l", col = mycols[1], ylim = c(-500, 70))
for(i in 1:30) {
  lines(1191:2047, d[i+1,1191:2047], col = mycols[i+1])
}

####################################################################
#PRINCIPAL COMPONENT ANALYSIS
#################################################################

fit <- prcomp(AllData[,-3], scale = TRUE) #pca
#####save(fit, file = ('fit.Rdata'))

plot(fit, type = "bar") #plot pca as bar

biplot(fit, arrow.len = 0, cex = 0.1)

sdata<-split(AllData, catagories)
a <- sdata$`1` 
b <- sdata$`2` 
c <- sdata$`3` 
d <- sdata$`4` 


maxy <- max(AllData[1:4,])
miny <- min(AllData[1:4,])
##################################################################
#Extracts for PC1 and PC2##############
#######################################################

pcs <- fit$x[,1:2]
plot(pcs, col = unlist(catagories), pch=10)
legend('topleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)
pcs2 <- cbind(pcs,catagories)

pca<- fit$x[,1:1]
plot(pca, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)
pca<- fit$x[,1]

pcb<- fit$x[,2:2]
plot(pcb, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcb<- fit$x[,2]

pcc<- fit$x[,3:3]
plot(pcc, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcc<- fit$x[,3]

pcd<- fit$x[,4:4]
plot(pcd, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcd<- fit$x[,4]

pce<- fit$x[,5:5]
plot(pce, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pce<- fit$x[,5]

pcf<- fit$x[,6:6]
plot(pcf, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcf<- fit$x[,6]

pcg<- fit$x[,7:7]
plot(pcg, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcg<- fit$x[,7]

pch<- fit$x[,8:8]
plot(pch, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pch<- fit$x[,8]

pci<- fit$x[,9:9]
plot(pci, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pci<- fit$x[,9]

pcj<- fit$x[,10:10]
plot(pcj, col = unlist(catagories), pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcj<- fit$x[,10]

########################################################################################
#ANOVA and TUKEY#
#############################################################################
####Split PC into columns
pcas <- split (pca, unlist(catagories))
pcas
a <- pcas$`1` 
b <- pcas$`2` 
c <- pcas$`3` 
d <- pcas$`4` 


length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)

####box plot the PC
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

####create catagory factor
catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))

####create list of PC
ja <- c(a,b,c)

####Determine if homogenity of variances, is the qchisq>than barret K score then accept HO
## Anova wants variances to be equal if not there is less power but still can be used to strigent significance values

bartlett.test(ja, catagory)
qchisq(0.950, 3)
fligner.test(ja, catagory)

####run anova
ja
afit <- aov(ja~catagory)
summary.aov(afit)

##run tukey test
tafit <- TukeyHSD(afit)
tafit

##any that do not cross 0 are significant
plot(tafit)

##check for outliers or errors in model
plot(afit)
a
b
c
d

kruskal.test(ja ~ catagory)

#############################################################################

pcbs <- split (pcb, unlist(catagories))

a <- pcbs$`1` 
b <- pcbs$`2` 
c <- pcbs$`3` 
d <- pcbs$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
jb <- c(a,b,c)
bartlett.test(jb, catagory)
fligner.test(jb, catagory)
afit <- aov(jb~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jb ~ catagory)

#############################################################################

pccs <- split (pcc, unlist(catagories))

a <- pccs$`1` 
b <- pccs$`2` 
c <- pccs$`3` 
d <- pccs$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('C','D', 'DI')))
jc <- c(a,b,c)
bartlett.test(jc, catagory)
fligner.test(jc, catagory)
afit <- aov(jc~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jc ~ catagory)

#############################################################################

pcds <- split (pcd, unlist(catagories))

a <- pcds$`1` 
b <- pcds$`2` 
c <- pcds$`3` 
d <- pcds$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c, d)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('B', 'CC','CN'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
jd <- c(a,b,c)
bartlett.test(jd, catagory)
fligner.test(jd, catagory)
afit <- aov(jd~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jd ~ catagory)


#############################################################################
pces <- split (pce, unlist(catagories))

a <- pces$`1` 
b <- pces$`2` 
c <- pces$`3` 
d <- pces$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
je <- c(a,b,c)
bartlett.test(je, catagory)
fligner.test(je, catagory)
afit <- aov(je~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)

a
b
c
d
kruskal.test(je ~ catagory)

#############################################################################

pcfs <- split (pcf, unlist(catagories))

a <- pcfs$`1` 
b <- pcfs$`2` 
c <- pcfs$`3` 
d <- pcfs$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
jf <- c(a,b,c)
bartlett.test(jf, catagory)
fligner.test(jf, catagory)
afit <- aov(jf~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jf ~ catagory)

#############################################################################
pcgs <- split (pcg, unlist(catagories))

a <- pcgs$`1` 
b <- pcgs$`2` 
c <- pcgs$`3` 
d <- pcgs$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
jg <- c(a,b,c)
bartlett.test(jg, catagory)
fligner.test(jg, catagory)
afit <- aov(jg~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jg ~ catagory)

#############################################################################
pchs <- split (pch, unlist(catagories))

a <- pchs$`1` 
b <- pchs$`2` 
c <- pchs$`3` 
d <- pchs$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
jh <- c(a,b,c)
bartlett.test(jh, catagory)
fligner.test(jh, catagory)
afit <- aov(jh~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jh ~ catagory)


#############################################################################
pcis <- split (pci, unlist(catagories))

a <- pcis$`1` 
b <- pcis$`2` 
c <- pcis$`3` 
d <- pcis$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
ji <- c(a,b,c)
bartlett.test(ji, catagory)
fligner.test(ji, catagory)
afit <- aov(ji~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(ji ~ catagory)

#############################################################################
pcjs <- split (pcj, unlist(catagories))

a <- pcjs$`1` 
b <- pcjs$`2` 
c <- pcjs$`3` 
d <- pcjs$`4` 

length(a) <- 15
length(b) <- 15
length(c) <- 15
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)

catagory<-factor(gl(3,15,45 ,labels=c('B','CC', 'CN')))
jj <- c(a,b,c)
bartlett.test(jj, catagory)
fligner.test(jj, catagory)
afit <- aov(jj~catagory)
afit <- aov(afit)
summary.aov(afit)
tafit <- TukeyHSD(afit)
tafit
plot(tafit)
plot(afit)
a
b
c
d
kruskal.test(jh ~ catagory)


#############################################################################


########################################################################################
####MANOVA
########################################################################################

Y <- cbind(ja, jb, jc, jd, je, jf, jg, jh, ji, jj)
manY <- manova(Y~catagory)
manY
summary(manY, test="Pillai")
summary.aov(manY)

########################################################################################

pcp <- fit$x[,1:10]
head (pcp)
catagory<-factor(gl(4,30,120 ,labels=c('C','D', 'DI', 'I')))
catagory


manPC <- manova(pcp ~ as.factor(catagory))
summary.aov(manPC)

xt <- manova(cbind(PC1, PC2) ~ as.factor(X1L), data = pcs2)

z <- cbind(pcp,catagory)
colnames(z) <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'catagory')
head(z)
z <- as.data.frame(z)
xt <- manova(cbind(PC1, PC2) ~ catagory, data = z)
summary(xt)
summary.aov(xt)

xt1_2 <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) ~ catagory, data = z, subset = catagory %in% c("1","2"))
summary.aov(xt1_2)
xt1_3 <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) ~ catagory, data = z, subset = catagory %in% c("1","3"))
summary.aov(xt1_3)
xt1_4 <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) ~ catagory, data = z, subset = catagory %in% c("1","4"))
summary.aov(xt1_4)
xt2_3 <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) ~ catagory, data = z, subset = catagory %in% c("2","3"))
summary.aov(xt2_3)
xt2_4 <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) ~ catagory, data = z, subset = catagory %in% c("2","4"))
summary.aov(xt2_4)
xt3_4 <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) ~ catagory, data = z, subset = catagory %in% c("3","4"))
summary.aov(xt3_4)

# This will help you grab the p-value for PC1
############################################################################
pa <- summary.aov(xt1_2)[[1]][5][[1,1]]
pb <- summary.aov(xt1_2)[[2]][5][[1,1]]
pc <- summary.aov(xt1_2)[[3]][5][[1,1]]
pd <- summary.aov(xt1_2)[[4]][5][[1,1]]
pe <- summary.aov(xt1_2)[[5]][5][[1,1]]
pf <- summary.aov(xt1_2)[[6]][5][[1,1]]
pg <- summary.aov(xt1_2)[[7]][5][[1,1]]
ph <- summary.aov(xt1_2)[[8]][5][[1,1]]
pi <- summary.aov(xt1_2)[[9]][5][[1,1]]
pj <- summary.aov(xt1_2)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))
############################################################################
pa <- summary.aov(xt1_2)[[1]][5][[1,1]]
pb <- summary.aov(xt1_2)[[2]][5][[1,1]]
pc <- summary.aov(xt1_2)[[3]][5][[1,1]]
pd <- summary.aov(xt1_2)[[4]][5][[1,1]]
pe <- summary.aov(xt1_2)[[5]][5][[1,1]]
pf <- summary.aov(xt1_2)[[6]][5][[1,1]]
pg <- summary.aov(xt1_2)[[7]][5][[1,1]]
ph <- summary.aov(xt1_2)[[8]][5][[1,1]]
pi <- summary.aov(xt1_2)[[9]][5][[1,1]]
pj <- summary.aov(xt1_2)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))
############################################################################
pa <- summary.aov(xt1_3)[[1]][5][[1,1]]
pb <- summary.aov(xt1_3)[[2]][5][[1,1]]
pc <- summary.aov(xt1_3)[[3]][5][[1,1]]
pd <- summary.aov(xt1_3)[[4]][5][[1,1]]
pe <- summary.aov(xt1_3)[[5]][5][[1,1]]
pf <- summary.aov(xt1_3)[[6]][5][[1,1]]
pg <- summary.aov(xt1_3)[[7]][5][[1,1]]
ph <- summary.aov(xt1_3)[[8]][5][[1,1]]
pi <- summary.aov(xt1_3)[[9]][5][[1,1]]
pj <- summary.aov(xt1_3)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))
############################################################################
pa <- summary.aov(xt1_4)[[1]][5][[1,1]]
pb <- summary.aov(xt1_4)[[2]][5][[1,1]]
pc <- summary.aov(xt1_4)[[3]][5][[1,1]]
pd <- summary.aov(xt1_4)[[4]][5][[1,1]]
pe <- summary.aov(xt1_4)[[5]][5][[1,1]]
pf <- summary.aov(xt1_4)[[6]][5][[1,1]]
pg <- summary.aov(xt1_4)[[7]][5][[1,1]]
ph <- summary.aov(xt1_4)[[8]][5][[1,1]]
pi <- summary.aov(xt1_4)[[9]][5][[1,1]]
pj <- summary.aov(xt1_4)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))
############################################################################
pa <- summary.aov(xt2_3)[[1]][5][[1,1]]
pb <- summary.aov(xt2_3)[[2]][5][[1,1]]
pc <- summary.aov(xt2_3)[[3]][5][[1,1]]
pd <- summary.aov(xt2_3)[[4]][5][[1,1]]
pe <- summary.aov(xt2_3)[[5]][5][[1,1]]
pf <- summary.aov(xt2_3)[[6]][5][[1,1]]
pg <- summary.aov(xt2_3)[[7]][5][[1,1]]
ph <- summary.aov(xt2_3)[[8]][5][[1,1]]
pi <- summary.aov(xt2_3)[[9]][5][[1,1]]
pj <- summary.aov(xt2_3)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))
############################################################################
pa <- summary.aov(xt2_4)[[1]][5][[1,1]]
pb <- summary.aov(xt2_4)[[2]][5][[1,1]]
pc <- summary.aov(xt2_4)[[3]][5][[1,1]]
pd <- summary.aov(xt2_4)[[4]][5][[1,1]]
pe <- summary.aov(xt2_4)[[5]][5][[1,1]]
pf <- summary.aov(xt2_4)[[6]][5][[1,1]]
pg <- summary.aov(xt2_4)[[7]][5][[1,1]]
ph <- summary.aov(xt2_4)[[8]][5][[1,1]]
pi <- summary.aov(xt2_4)[[9]][5][[1,1]]
pj <- summary.aov(xt2_4)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))
############################################################################
pa <- summary.aov(xt3_4)[[1]][5][[1,1]]
pb <- summary.aov(xt3_4)[[2]][5][[1,1]]
pc <- summary.aov(xt3_4)[[3]][5][[1,1]]
pd <- summary.aov(xt3_4)[[4]][5][[1,1]]
pe <- summary.aov(xt3_4)[[5]][5][[1,1]]
pf <- summary.aov(xt3_4)[[6]][5][[1,1]]
pg <- summary.aov(xt3_4)[[7]][5][[1,1]]
ph <- summary.aov(xt3_4)[[8]][5][[1,1]]
pi <- summary.aov(xt3_4)[[9]][5][[1,1]]
pj <- summary.aov(xt3_4)[[10]][5][[1,1]]
pp <- cbind(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
barplot(pp, ylim= c(0, 0.06))


############################################################################
# or you could plot this but see how the first few bins drown out pc1 I would rerun the fit without the first 
plot(fit$rotation[8:2046,1], type='line')
plot(fit$rotation[,2], type='line')
plot(fit$rotation[,3], type='line')
plot(fit$rotation[,4], type='line')
plot(fit$rotation[,5],type='line')
plot(fit$rotation[,6], type='line')
plot(fit$rotation[,7], type='line')
plot(fit$rotation[,8], type='line')
plot(fit$rotation[,9], type='line')

############################################################################



manPC <- manova(cbind(PC1, PC2) ~ catagory , data = z)
summary(manPC)
summary.aov(manPC)


##LINEAR MODEL CREATION################################
m <- a* b * c
model=lm( j ~ m, na.action = na.exclude )
                       
################define our own "rstandard" method for "mlm" class############
rstandard.mlm <- function (model) {
Q <- with(model, qr.qy(qr, diag(1, nrow = nrow(qr$qr), ncol = qr$rank)))  ## Q matrix
hii <- rowSums(Q ^ 2)  ## diagonal of hat matrix QQ'
RSS <- colSums(model$residuals ^ 2)  ## residual sums of squares (for each model)
sigma <- sqrt(RSS / model$df.residual)  ##  ## Pearson estimate of residuals (for each model)
pointwise_sd <- outer(sqrt(1 - hii), sigma)  ## point-wise residual standard error (for each model)
model$residuals / pointwise_sd  ## standardised residuals
  }
                       
 ##PLOT FITTED against RESIDUAL- LINEAR MODEL##################
k <- as.data.frame(j)
m1 <- lm(cbind(a,b,c)~., k)
r <- rstandard.mlm(m1)
n <- fitted(m1)
plot(n, j)
plot(n, j, col = as.numeric(col(r)), pch = 4, ylim = c(-100, 100))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
   col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4)
plot(r, j)
plot(r, j, col = as.numeric(col(r)), pch = 4, ylim = c(-80, 120))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
       col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4,)
abline(lm(waiting ~ duration))
plot(r, j)
                       
plot(r, n)
plot(r, n, col = as.numeric(col(r)), pch = 4, ylim = c(-50, 50))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
    col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4)
                       
plot(n, r)
plot(n, r, col = as.numeric(col(r)), pch = 4, ylim = c(-5, 5))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
     col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4)
                       
                       
##PLOT FITTED against RESIDUAL - ANOVA##################
n <- fitted(model)
r <- rstandard(model)
plot(r, n)
plot(r, n, col = as.numeric(col(r)), pch = 4, ylim = c(-100, 100))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4)
                       
plot(n, r)
plot(n, r, col = as.numeric(col(r)), pch = 4, ylim = c(-5, 5))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
      col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4)
                       
plot(n, j)
plot(n, j, col = as.numeric(col(r)), pch = 4, ylim = c(-100, 100))
legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
        col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4)
plot(r, j)
                       plot(r, j, col = as.numeric(col(r)), pch = 4, ylim = c(-80, 120), abline(fitted(model)~rstandard(model)))
                       legend("topleft", legend = paste0("catagory ", 1:ncol(n)), pch = 4,
                              col = 1:ncol(n), text.col = 1:ncol(n), cex= 0.4,)
                       v<-as.data.frame(r:j)
                       v<-as.data.frame(v)
                       library(ggplot2)
                       ?ggplot
                       ggplot(data= v, mapping = aes(x=r, y=j) + geom_line(aes))
                       plot(r, j)
                       
##################################################################################################                       
#PRINICIPAL COMPONENT PLOTTING #################################################################################################                       
##################################################################################################                       
                       
pcd <- fit$x [,1:3] #PC1 and 3
                       
plot (pcd, col = pal)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)

                       
pct <- fit$x[,2:3] #PC 2 and 3
plot (pct, col = unlist(catagories))
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pairs(pcd, col= pal)
pairs(pct, col = pal)
pairs(pcs, col = pal)
pairs(pcy, col - pal)
                       
pcp <- fit$x[,1:5]
                 
pairs(pcp, col = pal)
                       
##
#ENSEMBLE SECTION
##################################################################################
                       
rownames (AllData) <- DataNames
colnames (AllData) <- dat[,1]
em_range <- dat [,1]
setwd("~/Documents/phd/code")
source ('rse09382-mmc2.r')
                       
# pdf("PLS_Model_Fit.pdf")

########MAKE SURE CHANGED DIRECTORY BACK BELOW######################

setwd("~/Documents/phd/code/fertiliser2")
ensfit <- ensemble (AllData, unlist(catagories), em_range)
1
plot(ensfit)
                       
stop()

##
#ClUSTERING
###################################################################
                       
scaledalldata <- scale(AllData)
                       
library(mclust)
clustered <- Mclust(AllData, G = 3)
summary(clustered)
# pdf('cluster.pdf')
plot(clustered)
1
0
plot(clustered, what= 'classification')
par(mfrow = c(2,2))
plot(clustered, what = "uncertainty", dimens = c(2,1), main = "")
plot(clustered, what = "uncertainty", dimens = c(3,1), main = "")
plot(clustered, what = "uncertainty", dimens = c(2,3), main = "")
par(mfrow = c(1,1))
                       
par(mfrow = c(2,2))
plot(clustered, what = "classification", dimens = c(2,1), main = "")
plot(clustered, what = "classification", dimens = c(3,1), main = "")
plot(clustered, what = "classification", dimens = c(2,3), main = "")
par(mfrow = c(1,1))
                       
par(mfrow = c(2,2))
plot(clustered, what = "density", dimens = c(2,1), main = "")
plot(clustered, what = "density", dimens = c(3,1), main = "")
plot(clustered, what = "density", dimens = c(2,3), main = "")
par(mfrow = c(1,1))
                       
ICLAllData <- mclustICL(AllData)
summary(ICLAllData)
plot(ICLAllData)
# LRT <- mclustBootstrapLRT(AllData, modelName = "VEI")
# LRT
                       
plot(clustered, modelName = 'VEI')
1
0
                       
                       
library(mclust)
dimensionreduction <- MclustDR(clustered)
summary(dimensionreduction)
plot(dimensionreduction, what = "pairs")
plot(dimensionreduction)
1
3
4
5
6
7
0
plot(dimensionreduction, what = "boundaries", ngrid = 200)
mod1dr = MclustDR(clustered, lambda = 1)
summary(mod1dr)
plot(mod1dr, what = "scatterplot")
plot(mod1dr)
3
4
5
6
7
0
plot(mod1dr, what = "boundaries", ngrid = 200)
                       
dens <- densityMclust(AllData)
summary(dens$BIC)
summary(dens, parameters = TRUE)
                       
                       


                      
                       
model <- prcomp(t(AllData), scale=TRUE)
fitt <- hclust(dist(model$x[,1:3]), method="complete") # 1:3 -> based on 3 components
groups <- cutree(fit, k=3) # k=23 -> 23 groups
plot(groups)
                       
pca<-prcomp(AllData)
                       


                       
loadings(fit)
Load <-loadings(fit)
print(Load)
Load<-as.matrix(Load)
write.table (Load, "Load.txt", sep="")
summary(Load)
dev.off()

dendrogram <- hclust(dist(AllData))
plot(dendrogram)
rect.hclust(dendrogram, 3)
rect.hclust(dendrogram, 23)
                       
                       
library(rgl)
plot3d(pcs$scores[,1:3])

library(scatterplot3d)
scatterplot3d(pcs)
scatterplot3d(pcs, angle = 55)
scatterplot3d(fit)
plot3d(pcs)

install.packages("RColorBrewer")
                       library(RColorBrewer)
                       comp <- data.frame(fit$x[,1:7])
                       plot(comp, pch=1, col=unlist(catagories))
                       mydata<- AllData
                       wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
                       for (i in 2:23) wss[i] <- sum(kmeans(mydata,
                                                            centers=i)$withinss)
                       plot(1:23, wss, type="b", xlab="Number of Clusters",
                            ylab="Within groups sum of squares")
                       k <- kmeans(comp, 9, nstart=25, iter.max=1000)
                       palette(alpha(brewer.pal(9,'Set1'), 0.5))
                       plot(comp, col=k$clust, pch=16)
                       
                       k <- kmeans(comp, 23, nstart=25, iter.max=1000)
                       palette(alpha(brewer.pal(9,'Set1'), 0.5))
                       plot(comp, col=k$clust, pch=16)
                       
                       comp <- data.frame(fit$x)
                       plot3d(comp$PC1, comp$PC2, comp$PC3)
                       mydata<-comp
                       wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
                       for (i in 2:9) wss[i] <- sum(kmeans(mydata,
                                                           centers=i)$withinss)
                       plot(1:9, wss, type="b", xlab="Number of Clusters",
                            ylab="Within groups sum of squares")
                       k <- kmeans(comp, 4, nstart=25, iter.max=1000)
                       palette(alpha(brewer.pal(4,'Set1'), 0.5))
                       plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
                       plot3d(comp$PC1, comp$PC2, comp$PC4, col=k$clust)
                       
                       wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
                       for (i in 2:23) wss[i] <- sum(kmeans(mydata,
                                                            centers=i)$withinss)
                       plot(1:23, wss, type="b", xlab="Number of Clusters",
                            ylab="Within groups sum of squares")
                       k <- kmeans(comp, 23, nstart=25, iter.max=1000)
                       palette(alpha(brewer.pal(9,'Set1'), 0.5))
                       plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
                       plot3d(comp$PC1, comp$PC2, comp$PC4, col=k$clust)
                       
                       
                       
                       A1<-AllData[,1:100]
                       A2<-AllData[,101:200]
                       A3<-AllData[,201:300]
                       A4<-AllData[,301:400]
                       A5<-AllData[,401:500,]
                       A6<-AllData[,501:600,]
                       A7<-AllData[,601:700]
                       A8<-AllData[,701:800]
                       A9<-AllData[,801:900]
                       A10<-AllData[,901:1000]
                       A11<-AllData[,1001:1024]
                       A12<-AllData[110:119,]
                       A13<-AllData[120:129,]
                       A14<-AllData[130:139,]
                       A15<-AllData[140:147,]
                       A16<-AllData[148:157,]
                       A17<-AllData[158:167,]
                       A18<-AllData[168:177,]
                       A19<-AllData[178:187,]
                       A20<-AllData[188:201,]
                       A21<-AllData[202:211,]
                       A22<-AllData[212:221,]
                       A23<-AllData[222:231,]
                       #########################################################
                       fit <- prcomp(A1[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ##########################################################
                       #########################################################
                       fit <- prcomp(A2[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A3[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A4[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A5[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A6[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A7[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A8[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A9[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A10[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A11[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A12[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A13[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A14[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A15[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A16[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A17[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A18[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A19[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A20[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A21[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A22[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                       ###################################################################################################################
                       fit <- prcomp(A23[,-3], scale = TRUE) #pca
                       plot(fit, type = "bar") #plot pca as bar
                       biplot(fit)
                       summary (fit)
                       # Extracts for PC1 and PC2
                       print(fit)
                       pcs <- fit$x[,1:2]
                       plot(pcs, col = unlist(catagories))
                       pcy<- fit$x[,1:1]
                       plot(pcy, col = unlist (catagories))
                     
stop()
                       