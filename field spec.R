####################################################
#IMPORT DATA####################################################
####################################################

setwd('~/Documents/phd/code')
options(max.print=10000000)
whiteref <- read.table('White Standard.txt')
setwd("~/Documents/phd/code/fertiliserblackback")
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
    dat <- read.table(paste("./", topFolders[iFo], "/", folderFiles[iFi], sep = ""), sep = "", header = F, skip = 1, skipNul = TRUE)
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
          rgb(100,157,97, maxColorValue=255),
          rgb(180,78,94, maxColorValue=255)))
palette()
pal<-palette()

maxy <- max(AllData[1:3,])
miny <- min(AllData[1:3,])

plot(1:2151, AllData[1,], type = "l", col = pal, ylim = c(0, 0.5))

for(i in 1:3) {
  lines(1:2151, AllData[i+1,], col = pal[i+1])
}
legend('bottomleft', legend=c('b', 'ck','cn'), col=pal,pch=16, ncol=4,cex=0.5)

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


####################################################################
#spectra plotting
#################################################################
##############################################MAKE COLOUR PALETTE OF 30 then plot
##############################################
mycols <- adjustcolor(palette(rainbow(30)), alpha.f = 0.3)

plot(1:2151, a[1,], type = "l", col = mycols, ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1:2151, a[i+1,], col = mycols[i+1])
}
plot(1:459, a[1,1:459], type = "l", col = pal, ylim = c(0, 0.500))
for(i in 1:30) {
  lines(1:482, a[i+1,1:482], col = pal[i+1])
}
plot(460:1190, a[1,460:1190], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(460:1190, a[i+1,460:1190], col = pal[i+1])
}
plot(1191:2151, a[1,1191:2151], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1191:2151, a[i+1,1191:2151], col = pal[i+1])
}

####################################################################

plot(1:2151, b[1,], type = "l", col = mycols, ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1:2151, b[i+1,], col = mycols[i+1])
}
plot(1:459, b[1,1:459], type = "l", col = pal, ylim = c(0, 0.500))
for(i in 1:30) {
  lines(1:482, b[i+1,1:482], col = pal[i+1])
}
plot(460:1190, b[1,460:1190], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(460:1190, b[i+1,460:1190], col = pal[i+1])
}
plot(1191:2151, b[1,1191:2151], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1191:2151, b[i+1,1191:2151], col = pal[i+1])
}

####################################################################
plot(1:2151, c[1,], type = "l", col = mycols, ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1:2151, c[i+1,], col = mycols[i+1])
}
plot(1:459, c[1,1:459], type = "l", col = pal, ylim = c(0, 0.500))
for(i in 1:30) {
  lines(1:482, c[i+1,1:482], col = pal[i+1])
}
plot(460:1190, c[1,460:1190], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(460:1190, c[i+1,460:1190], col = pal[i+1])
}
plot(1191:2151, c[1,1191:2151], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1191:2151, c[i+1,1191:2151], col = pal[i+1])
}

####################################################################

plot(1:2151, a[1,], type = "l", col = mycols, ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1:2151, a[i+1,], col = mycols[i+1])
}
plot(1:459, a[1,1:459], type = "l", col = pal, ylim = c(0, 0.500))
for(i in 1:30) {
  lines(1:482, a[i+1,1:482], col = pal[i+1])
}
plot(460:1190, a[1,460:1190], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(460:1190, a[i+1,460:1190], col = pal[i+1])
}
plot(1191:2151, a[1,1191:2151], type = "l", col = mycols[1], ylim = c(0, 0.5))
for(i in 1:30) {
  lines(1191:2151, a[i+1,1191:2151], col = pal[i+1])
}

##################################################################
#Extracts for PC1 and PC2##############
#######################################################

pcs <- fit$x[,1:2]
plot(pcs, col = pal, pch=10)
legend('topleft', legend=c('b', 'ck','cn'), col=pal,pch=16, ncol=4,cex=0.5)
pcs2 <- cbind(pcs,catagories)

pcy<- fit$x[,1:1]
plot(pcy, col = pal, pch=10)
legend('bottomleft', legend=c('c', 'd','di', 'i'), col=pal,pch=16, ncol=4,cex=0.5)
pcy<- fit$x[,1]

########################################################################################
#MANOVA and TUKEY#
#############################################################################

pcys <- split (pcy, unlist(catagories))

a <- pcys$`1` 
b <- pcys$`2` 
c <- pcys$`3` 


nn <- max(length(a), length(b), length(c))

length(a) <- 50
length(b) <- 50
length(c) <- 50
length(d) <- 30

j <- cbind(a,b, c)
boxplot(j, na.omit =TRUE, outline=TRUE, col = pal)
legend('topleft', legend=c('b', 'ck','cn'), col=pal,pch=16, ncol=4,cex=0.5)
head(j)
j

pcp <- fit$x[,1:10]
head (pcp)
catagory<-factor(gl(3,15,45 ,labels=c('b','ck', 'cn')))
catagory
pcp

manPC <- manova(pcp ~ as.factor(catagory))
summary.aov(manPC)

xt <- manova(cbind(PC1, PC2) ~ as.factor(X1L), data = pcs2)

z <- cbind(pcp,catagory)
colnames(z) <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'PC6', 'PC7', 'PC8', 'PC9', 'PC10' 'catagory')
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
plot(fit$rotation[1:2046,1], type='line')
plot(fit$rotation[,2], type='line')
plot(fit$rotation[,3], type='line')
plot(fit$rotation[,4], type='line')
plot(fit$rotation[,5],type='line')
plot(fit$rotation[,6], type='line')
plot(fit$rotation[,7], type='line')
plot(fit$rotation[,8], type='line')
plot(fit$rotation[,9], type='line')
plot(fit$rotation[,10], type='line')
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

pct <- fit$x[,2:3] #PC 2 and 3

plot (pct, col = pal)

#install.packages("randomcoloR")
#library(randomcoloR)
#cols<-distinctColorPalette(10)
plot(pcs, col=pal)
legend('bottomleft', legend=c('1','2','3','4','5','6','7','8', '9','10','11','12', '13','14','15','16','17','18','19','20','21','22','23'), col=cols,pch=1, ncol=4,cex=0.2)#legend("bottomright", c("P_I_M", "NMg_I_M", "K_I_M", "NKMg_I_M", "PK_I_M", "NPMg_I_M", "All_I_M", "Mg_I_M", "N_I_M", "None_I_M", "NPK_I_M", "PKMg_I_M", "All_M", "All_Control", "All_I"))

cols<-distinctColorPalette(100)
plot(pcy, col=pal)
legend('bottomleft', legend=c('1','2','3','4','5','6','7','8', '9','10','11','12', '13','14','15','16','17','18','19','20','21','22','23'), col=cols,pch=1, ncol=4,cex=0.2)#legend("bottomright", c("P_I_M", "NMg_I_M", "K_I_M", "NKMg_I_M", "PK_I_M", "NPMg_I_M", "All_I_M", "Mg_I_M", "N_I_M", "None_I_M", "NPK_I_M", "PKMg_I_M", "All_M", "All_Control", "All_I"))

# Remove a two rows:

# fit <- prcomp(AllData[c(-3, -73)], .scale = T)

# pcs <- fit$x[,1:2]

# plot(pcs, col = unlist(catagories[c(-3, -73)]))


framealldata<-as.data.frame(AllData)
library(ggplot2)
ndf <- data.frame(CNames = rownames(pcs), PC1 = pcs[,1], PC2 = pcs[,2])

ggplot(ndf, aes(x= PC1, y= PC2, colour="green", label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0)
head(ndf)
ndf2<-ndf
ndf2[,1]<-unlist(catagories[,1])
head(ndf2)
uncat <- unlist(catagories)
P=ggplot(ndf2, aes(x= PC1, y= PC2, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0, col=unlist(catagories))
P+theme_bw()
ggplot(ndf2, aes(x= PC1, y= PC2, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0, col=unlist(catagories))
ggplot(ndf2, aes(x= PC1, y= PC2, col= uncat , label= CNames)) + geom_point()
pairs(ndf)

#ndf3 <- data.frame(CNames = rownames(pcs), PC1 = pcs[,1], PC3 = pcs[,2])
#ggplot(ndf3, aes(x= PC1, y= PC3, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= (catagories)),hjust=0, vjust=0, col=unlist(catagories))
#ggplot(ndf3, aes(x= PC1, y= PC3, col= uncat , label= CNames)) + geom_point()


#ndf4 <- data.frame(CNames = rownames(pcs), PC2 = pcs[,1], PC3 = pcs[,2])
#ggplot(ndf3, aes(x= PC1, y= PC3, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= (catagories)),hjust=0, vjust=0, col=unlist(catagories))
#ggplot(ndf3, aes(x= PC1, y= PC3, col= uncat , label= CNames)) + geom_point()

ggplot(ndf2, aes(PC1, PC2, color = CNames)) + geom_point()


library(ggplot2)
ndf5 <- data.frame(CNames = rownames(pcd), PC1 = pcs[,1], PC3 = pcs[,2])

ggplot(ndf5, aes(x= PC1, y= PC3, colour="green", label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0)
head(ndf5)
ndf6<-ndf5
ndf6[,1]<-unlist(catagories[,1])
head(ndf6)
uncat <- unlist(catagories)
ggplot(ndf6, aes(x= PC1, y= PC3, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0, col=unlist(catagories))
ggplot(ndf6, aes(x= PC1, y= PC3, col= uncat , label= CNames)) + geom_point()
pairs(ndf6)

ndf7 <- data.frame(CNames = rownames(pct), PC2 = pcs[,1], PC3 = pcs[,2])

ggplot(ndf7, aes(x= PC2, y= PC3, colour="green", label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0)
head(ndf7)
ndf8<-ndf7
ndf8[,1]<-unlist(catagories[,1])
head(ndf8)
uncat <- unlist(catagories)
ggplot(ndf8, aes(x= PC2, y= PC3, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0, col=unlist(catagories))
ggplot(ndf8, aes(x= PC2, y= PC3, col= uncat , label= CNames)) + geom_point()
pairs(ndf8)

pairs(pcd)
pairs(pct)
pairs(pcs)
pairs(pcy)

pcp <- fit$x[,1:5]

pairs(pcp, col = unlist(catagories))

##
#ENSEMBLE SECTION
##################################################################################

rownames (AllData) <- DataNames
colnames (AllData) <- dat[,1]
em_range <- dat [,1]
setwd("~/Documents/phd/code")
source ('rse09382-mmc2.r')

# pdf("PLS_Model_Fit.pdf")
setwd("~/Documents/phd/code/fertiliserblackback")
ensfit <- ensemble (AllData, unlist(catagories), em_range)
23

plot(ensfit)

›stop()
