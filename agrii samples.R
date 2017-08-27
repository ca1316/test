#IMPORT DATA

setwd('~/Documents/phd/code')
options(max.print=10000000)
whiteref <- read.table('White Standard.txt')
setwd("~/Documents/phd/code/agrii data belchamp")
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
    #  dat <- apply (dat, 2, function(x) x*whiteref[,2]) #apply white reference
    AllData <- rbind(AllData, t(dat[,2]))
    DataNames <- rbind(DataNames, paste("Cat_", iFo, "_sample_", iFi, sep = ""))
    catagories <- rbind(catagories, iFo)
  }
}


rownames(AllData) <- DataNames

##Look at available colors
####

showcols <-  function (indx = 0:6)
{
  for (ii in unique(indx)) {
    is <- 100 * ii + 1:100
    if (min(is) > length(colors())) {
      cat("Maximum value of arg is", floor(length(colors())/100),
          "\n")
      return(NULL)
    }
    foo <- matrix(colors()[is], nrow = 10)
    par(mar = c(3, 3, 0.25, 0.25))
    plot(1:10, 1:10, type = "n", yaxt = "n", xlab = "", ylab = "")
    axis(2, at = 1:10, lab = 10:1)
    for (j in 1:10) {
      for (i in 1:10) {
        points(j, 11 - i, col = foo[i, j], pch = 16,
               cex = 4)
        text(j, 11 - i - 0.3, foo[i, j], cex = 0.8)
      }
    }
    if (length(indx) > 1 & ii < max(indx))
      readline(paste("Currently showing group", ii, "  CR to continue "))
  }
  invisible(foo)
}
showcols()

##
#PRINCIPAL COMPONENT ANALYSIS
##

fit <- prcomp(AllData[,-3], scale = TRUE) #pca

plot(fit, type = "bar") #plot pca as bar

biplot(fit)
summary (fit)

# Extracts for PC1 and PC2
print(fit)
pcs <- fit$x[,1:2]

plot(pcs, col = unlist(catagories))

pcy<- fit$x[,1:1]
plot(pcy, col = unlist (catagories))
print(pcy)
setwd('~/Documents/phd/code')
y<-read.csv('agrii box.csv', skipNul = TRUE)
setwd("~/Documents/phd/code/agrii data belchamp")
boxplot(y)

pcd <- fit$x [,1:3] #PC1 and 3

plot (pcd, col = unlist(catagories))

pct <- fit$x[,2:3] #PC 2 and 3

plot (pct, col = unlist(catagories))

#install.packages("randomcoloR")
library(randomcoloR)
cols<-distinctColorPalette(10)
plot(pcs, col=rep(cols, each=23))
legend('bottomleft', legend=c('1','2','3','4','5','6','7','8', '9','10','11','12', '13','14','15','16','17','18','19','20','21','22','23'), col=cols,pch=1, ncol=4,cex=0.2)#legend("bottomright", c("P_I_M", "NMg_I_M", "K_I_M", "NKMg_I_M", "PK_I_M", "NPMg_I_M", "All_I_M", "Mg_I_M", "N_I_M", "None_I_M", "NPK_I_M", "PKMg_I_M", "All_M", "All_Control", "All_I"))

cols<-distinctColorPalette(100)
plot(pcy, col=rep(cols, each=23))
legend('bottomleft', legend=c('1','2','3','4','5','6','7','8', '9','10','11','12', '13','14','15','16','17','18','19','20','21','22','23'), col=cols,pch=1, ncol=4,cex=0.2)#legend("bottomright", c("P_I_M", "NMg_I_M", "K_I_M", "NKMg_I_M", "PK_I_M", "NPMg_I_M", "All_I_M", "Mg_I_M", "N_I_M", "None_I_M", "NPK_I_M", "PKMg_I_M", "All_M", "All_Control", "All_I"))

# Remove a two rows:

# fit <- prcomp(AllData[c(-3, -73)], .scale = T)

# pcs <- fit$x[,1:2]

# plot(pcs, col = unlist(catagories[c(-3, -73)]))

#
#
#
# PLS analysis
#
#
#

framealldata<-as.data.frame(AllData)
library(ggplot2)
ndf <- data.frame(CNames = rownames(pcs), PC1 = pcs[,1], PC2 = pcs[,2])

ggplot(ndf, aes(x= PC1, y= PC2, colour="green", label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0)
head(ndf)
ndf2<-ndf
ndf2[,1]<-unlist(catagories[,1])
head(ndf2)
uncat <- unlist(catagories)
ggplot(ndf2, aes(x= PC1, y= PC2, colour= 'red' , label= CNames))+ geom_point() +geom_text(aes(label= CNames),hjust=0, vjust=0, col=unlist(catagories))
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
pairs(pcp, col=unlist(catagories))

##
#ENSEMBLE SECTION
##

rownames (AllData) <- DataNames
colnames (AllData) <- dat[,1]
em_range <- dat [,1]
setwd("~/Documents/phd/code")
source ('rse09382-mmc2.r')

# pdf("PLS_Model_Fit.pdf")
setwd("~/Documents/phd/code/agrii data belchamp")
ensfit <- ensemble (AllData, unlist(catagories), em_range)
6
plot(ensfit)

stop()

##
#ClUSTERING
##

scaledalldata <- scale(AllData)

library(mclust)
clustered <- Mclust(AllData)
summary(clustered)
# pdf('cluster.pdf')
plot(clustered)
1
0
# dev.off()
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

require(mixtools)
out <- mvnormalmixEM(AllData, lambda = NULL, mu = NULL, sigma = NULL,
                     k = 8,arbmean = TRUE, arbvar = TRUE, epsilon = 1e-08,  maxit = 10000, verb = FALSE)
plot(out, density = TRUE, alpha = c(0.01, 0.05, 0.10, 0.12, 0.15),  marginal = TRUE)


#install.packages("rgl")
library(rgl)
plot3d(pcs$scores[,1:3])
#dev.off()

library(mclust)
plot(clustered, AllData, what = "BIC")
coordProj(AllData, dimens = c(1,2), what = "classification",
          classification = clustered$classification,
          parameters = clustered$parameters)
coordProj(AllData[,-1], dimens = c(1,2), what = "classification",
          classification = clustered$classification,
          parameters = clustered$parameters)

odd <- seq(from = 1, to = nrow(AllData), by = 2)
even <- seq(from = 2, to = nrow(AllData), by = 2)
DA1 <- MclustDA ( train = list(data = wine[odd]),
                  test = list(data = wine[even]))
DA1

model <- prcomp(t(AllData), scale=TRUE)
fitt <- hclust(dist(model$x[,1:3]), method="complete") # 1:3 -> based on 3 components
groups <- cutree(fit, k=23) # k=23 -> 23 groups
plot(groups)

library(scatterplot3d)
scatterplot3d(pcs)
scatterplot3d(pcs, angle = 55)
scatterplot3d(fit)

pca<-prcomp(AllData)

library(rgl)
plot3d(pcs)

loadings(fit)
Load <-loadings(fit)
print(Load)
Load<-as.matrix(Load)
write.table (Load, "Load.txt", sep="")
summary(Load)
dev.off()
dendrogram <- hclust(dist(AllData))
plot(dendrogram)
rect.hclust(dendrogram, 9)
rect.hclust(dendrogram, 6)

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
k <- kmeans(comp, 9, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC2, comp$PC3, col=unlist(catagories))
plot3d(comp$PC1, comp$PC2, comp$PC4, col=unlist(catagories))

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:23) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:23, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
k <- kmeans(comp, 23, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC2, comp$PC4, col=k$clust)


X <- AllData[,c(4, 26, 27)]
Class <- AllData[,2]

set.seed(124)
train <- sample(1:nrow(X), size = round(nrow(X)*2/3), replace = FALSE)
X.train <- X[train,]
Class.train <- Class[train]
table(Class.train)
Class.train

X.test <- X[-train,]
Class.test <- Class[-train]
table(Class.test)
Class.test

mod1 <- MclustDA(X.train, Class.train, modelType = c("EDDA"))
summary(mod1)
summary(mod1, newdata = X.test, newclass = Class.test)

cv <- cvMclustDA(mod1)
unlist(cv[c("error", "se")])
mod2 <- MclustDA(X.train, Class.train)
summary(mod2, newdata = X.test, newclass = Class.test)

#source("https://bioconductor.org/biocLite.R")
#biocLite("timecourse")
#install.packages('timecourse')
#a
library(timecourse)
assay <- rep(c("cat_1", "cat_2", "cat_3", "cat_4", 'cat_5', 'cat_6', 'cat_7', 'cat_8', 'cat_9', 'cat_10','cat_11','cat_12','cat_13','cat_14','cat_15', 'cat_16', 'cat_17','cat_18', 'cat_19','cat_20','cat_21','cat_22','cat_23'), each=10)
time.grp <- rep(c(1:10), 23)
size <- rep(23, 1024)
out1 <- mb.long(AllData, times=10, reps=size)
out2 <- mb.long(AllData, times=10, reps=size, rep.grp=assay, time.grp=time.grp)

install.packages('vegan')		
library(vegan)
AllData<-scale(AllData)
bray  <- vegdist(AllData, method="bray", binary=F)
pcoa <- cmdscale(bray, eig=T)
pco1 <- pcoa$points[,1]
pco2 <- pcoa$points[,2]
cor(pco1, pco2)
cor(pco1, pco1)
plot(pco1,pco2, pch=as.character(pcoa$eig)) 
total.var=sum(pcoa$eig)
rel.eigen=pcoa$eig/total.var
barplot(rel.eigen)
cumsum(rel.eigen)
Euc.pcoa  <- vegdist(cbind(pco1,pco2), method="euc", binary=F)
plot(Euc.pcoa  , bray)

pca <- prcomp(AllData[,-3], scale = T )
pca
cor(pca$x)
summary(pca)
pairs(AllData)
screeplot(pca)
assoc <- vegdist(AllData, method="euclidean", binary=F)
pcoa2 <- cmdscale(assoc, eig=T)
cor(pcoa2$points[,1],pca$x[,1])
local.FC.PCA <- pca$x[,1]
lm.PCA <- lm(pco1~local.FC.PCA)
summary(lm.PCA)$r.squared

Data <- data.frame(v=c(1:1024))
temp <- apply (AllData, 1, function(x) mean(x[(i*10+1):(i+1)*10]))
Data <- cbind (Data,temp)

apply(AllData, 2, function(x) (mean(x[355:502])-mean(x[586:772])/mean(x[586:772]+mean(x[355:502]))))

