fit <- prcomp(AllData[,-3], scale = TRUE) #pca

plot(fit, type = "line") #plot pca as bar

biplot(fit)
summary (fit)
library(FactoMineR)

res.pca <- PCA(AllData, scale.unit=TRUE, ncp=Inf,
               graph = FALSE,quanti.sup=13:16,quali.sup=17)

res.hcpc <- HCPC(res.pca)
print(fit)
pcs <- fit$x[,1:2]

plot(pcs, col = unlist(catagories))

pcy<- fit$x[,1:1]
plot(pcy, col = unlist (catagories))

pcd <- fit$x [,1:3] #PC1 and 3

plot (pcd, col = unlist(catagories))

pct <- fit$x[,2:3] #PC 2 and 3

plot (pct, col = unlist(catagories))
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
pairs(pcp, col=unlist(catagories))

library(rgl)
plot3d(pcs$scores[,1:3])
#dev.off()

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
