#IMPORT DATA####################################################

setwd('~/Documents/phd/code')
options(max.print=10000000)
whiteref <- read.table('White Standard.txt')
setwd("~/Documents/phd/code/repeat repeat")
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
    dat <- apply (dat, 2, function(x) x*whiteref[,2]) #apply white reference
    AllData <- rbind(AllData, t(dat[,2]))
    DataNames <- rbind(DataNames, paste("", iFo, "", iFi, sep = ""))
    catagories <- rbind(catagories, iFo)
  }
}

rownames(AllData) <- DataNames

##
#INDICIES
##

rownames(AllData) <- DataNames
eighthundred <- AllData[,978]
sixeighty <- AllData[,732]
a <- eighthundred - sixeighty
b <- eighthundred+sixeighty
NDVI <- a/b
NDVI

fourfifty <- AllData[,248]
c <- 6*sixeighty
d <- 7.5*fourfifty
e <- c+eighthundred
f <- d+1
g <- e-f
EVI <- a/g
EVI

h <- sixeighty - fourfifty
i <- h*2
l <- eighthundred-i
m <- eighthundred+i
ARVI <- l/m
ARVI

sevenfifty <- AllData[,876]
seven05 <- AllData[,783]
n <- sevenfifty - seven05
o <- sevenfifty + seven05
rededgeNDVI <- n/o
rededgeNDVI

four45 <- AllData[,237]
p <- 2*four45
q <- o-p
modifiedrededgeNDVI <- n/q
modifiedrededgeNDVI

r <- sevenfifty - four45
s <- seven05 - four45
modifiedrededgeSRI <- r/s
modifiedrededgeSRI

seven40 <- AllData[,855]
seven20 <- AllData[,814]
VOGREI <- seven40/seven20
VOGREI

seven34 <- AllData[,843]
seven47 <- AllData[,870]
t <- seven34-seven47
seven15 <- AllData[,804]
seven26 <- AllData[,827]
u <- seven15+seven26
VOGREI2 <- t/u
VOGREI2

v <- seven15 + seven20
VOGREI3 <- t/v
VOGREI3

five31 <- AllData[,420]
five70 <- AllData[,503]
w <- five31 - five70
x <- five31 + five70
PRI <- w/x
PRI

five00 <- AllData [,355]
seven50 <- AllData [,876]
y <- sixeighty - five00
PSRI <- y/seven50
PSRI

##
#PRINCIPAL COMPONENT ANALYSIS
#################################################################

fit <- prcomp(AllData[,-3], scale = TRUE) #pca

plot(fit, type = "bar") #plot pca as bar

biplot(fit, arrow.len = 0)
summary (fit)

######Set a color Palette############################################

palette(c(rgb(255,95,96, maxColorValue=255),
          rgb(113,213,0, maxColorValue=255),
          rgb(90,0,131, maxColorValue=255),
          rgb(107,155,0, maxColorValue=255),
          rgb(227,130,255, maxColorValue=255),
          rgb(0,90,6, maxColorValue=255),
          rgb(255,109,164, maxColorValue=255),
          rgb(0,217,162, maxColorValue=255),
          rgb(113,0,67, maxColorValue=255),
          rgb(0,159,92, maxColorValue=255),
          rgb(135,25,0, maxColorValue=255),
          rgb(82,217,215, maxColorValue=255),
          rgb(241,151,0, maxColorValue=255),
          rgb(147,162,255, maxColorValue=255),
          rgb(251,187,61, maxColorValue=255),
          rgb(0,96,158, maxColorValue=255),
          rgb(255,152,61, maxColorValue=255),
          rgb(0,46,80, maxColorValue=255),
          rgb(179,119,0, maxColorValue=255),
          rgb(233,182,229, maxColorValue=255),
          rgb(118,70,0, maxColorValue=255),
          rgb(234,182,181, maxColorValue=255),
          rgb(79,9,24, maxColorValue=255)))
palette()

#Extracts for PC1 and PC2####################################################################

print(fit)
pcs <- fit$x[,1:2]
plot(pcs, col = unlist(catagories), pch=10)

pcy<- fit$x[,1:1]
plot(pcy, col = unlist(catagories), pch=10)

#MANOVA and TUKEY########################################################################################################

pcys <- split (pcy, unlist(catagories))
pcys

lengths <- 10  #max catergory length
length(pcys$`1`) <- lengths
a <- pcys$`1`
length(pcys$`2`) <- lengths
b <- pcys$`2`
length(pcys$`3`) <- lengths
c <- pcys$`3`
length(pcys$`4`) <- lengths
d <- pcys$`4`
length(pcys$`5`) <- lengths
length(pcys$`6`) <- lengths
length(pcys$`7`) <- lengths
length(pcys$`8`) <- lengths
length(pcys$`9`) <- lengths
length(pcys$`10`) <- lengths
length(pcys$`11`) <- lengths
length(pcys$`12`) <- lengths
length(pcys$`13`) <- lengths
length(pcys$`14`) <- lengths
length(pcys$`15`) <- lengths
length(pcys$`16`) <- lengths
length(pcys$`17`) <- lengths
length(pcys$`18`) <- lengths
length(pcys$`19`) <- lengths
length(pcys$`20`) <- lengths
length(pcys$`21`) <- lengths
length(pcys$`22`) <- lengths
length(pcys$`23`) <- lengths

j <- cbind(a, b, c, d)
j

boxplot(j)

manpca <- manova(j ~ a* b* c* d)
manpca
p<-cbind(a,b,c,d)
p <- as.data.frame(p)
m1 <- lm(cbind(a,b,c,d)~., p)
rstandard.mlm(m1)
fitted(m1)
summary(manpca)
summary(manpca, test="Pillai")
summary(manpca, test="Wilks")
summary(manpca, test="Hotelling-Lawley")
summary(manpca, test="Roy")
summary.aov(manpca)

v <- as.data.frame(j)
AOV <- aov(j ~ a*b*c*d, data=v)
summary(AOV)
summary.aov(AOV)
?model.tables.aov

ANOVA=aov(AOV)
summary(ANOVA)
summary.aov(ANOVA)

##LINEAR MODEL CREATION################################
z <- pcys$`1`* pcys$`2` * pcys$`3` * pcys$`4`
model=lm( j ~ z, na.action = na.exclude )

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
m1 <- lm(cbind(a,b,c,d)~., p)
f <- rstandard.mlm(m1)
r <- fitted(m1)
plot(r, f)
plot(r, f, col = as.numeric(col(r)), pch = 4, ylim = c(-5, 5))
legend("topleft", legend = paste0("catagory ", 1:ncol(f)), pch = 4,
       col = 1:ncol(f), text.col = 1:ncol(f), cex= 0.4)

##PLOT FITTED against RESIDUAL - ANOVA##################
plot(f, r)
plot(f, r, col = as.numeric(col(r)), pch = 4, ylim = c(-100, 100))
legend("topleft", legend = paste0("catagory ", 1:ncol(f)), pch = 4,
       col = 1:ncol(f), text.col = 1:ncol(f), cex= 0.4)

f <- fitted(model)
r <- rstandard(model)
plot(r, f)
plot(r, f, col = as.numeric(col(r)), pch = 4, ylim = c(-200, 200))
legend("topleft", legend = paste0("catagory ", 1:ncol(f)), pch = 4,
       col = 1:ncol(f), text.col = 1:ncol(f), cex= 0.4)

plot(f, r)
plot(f, r, col = as.numeric(col(r)), pch = 4, ylim = c(-5, 5))
legend("topleft", legend = paste0("catagory ", 1:ncol(f)), pch = 4,
       col = 1:ncol(f), text.col = 1:ncol(f), cex= 0.4)

# Tukey test to study each pair of treatment :
?TukeyHSD
s <- as.matrix(f)
TUKEY <- TukeyHSD(x=s, pcys$`1`* pcys$`2` * pcys$`3` * pcys$`4`, conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )


# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS=generate_label_df(TUKEY , "data$treatment")


# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))

# Draw the basic boxplot
a=boxplot(data$value ~ data$treatment , ylim=c(min(data$value) , 1.1*max(data$value)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="value" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over=0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )

plot(pcy, col = unlist(catagories))

# library
install.packages("multcompView")
library(multcompView)

# Create data
fit
FR<-fit$x[,1]
head(D)
D<-as.data.frame(D)
View(D)
treatment=c(rep('A', 10), rep("B", 10) , rep("C", 10), rep("D", 10) ,  rep("E", 10), rep('F', 14))
value=c( sample(D[,1], 1:10) , sample(D [,1], 11:20), sample(D [,1], 21:30), sample(D [,1], 31:40) , sample(D[,1], 41:50), sample(D[,1], 51:64) )
View(value)
data=data.frame(treatment,value)
View(data)

# What is the effect of the treatment on the value ?
model=lm( data$value ~ data$treatment )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'data$treatment', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )


# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS=generate_label_df(TUKEY , "data$treatment")


# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))

# Draw the basic boxplot
a=boxplot(data$value ~ data$treatment , ylim=c(min(data$value) , 1.1*max(data$value)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="value" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over=0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )

####setwd('~/Documents/phd/code')
####y<-read.csv('Workbook2.csv')
####Z<-read.csv('repeat repeat tuk.csv')
####setwd("~/Documents/phd/code/repeat repeat")
####boxplot(y)
#print(y)

pcd <- fit$x [,1:3] #PC1 and 3

plot (pcd, col = unlist(catagories))

pct <- fit$x[,2:3] #PC 2 and 3

plot (pct, col = unlist(catagories))

#install.packages("randomcoloR")
#library(randomcoloR)
#cols<-distinctColorPalette(10)
plot(pcs, col=rep(cols, each=23))
legend('bottomleft', legend=c('1','2','3','4','5','6','7','8', '9','10','11','12', '13','14','15','16','17','18','19','20','21','22','23'), col=cols,pch=1, ncol=4,cex=0.2)#legend("bottomright", c("P_I_M", "NMg_I_M", "K_I_M", "NKMg_I_M", "PK_I_M", "NPMg_I_M", "All_I_M", "Mg_I_M", "N_I_M", "None_I_M", "NPK_I_M", "PKMg_I_M", "All_M", "All_Control", "All_I"))

cols<-distinctColorPalette(100)
plot(pcy, col=rep(cols, each=23))
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
pairs(pcp, col=unlist(catagories))

##
#ENSEMBLE SECTION
##################################################################################

rownames (AllData) <- DataNames
colnames (AllData) <- dat[,1]
em_range <- dat [,1]
setwd("~/Documents/phd/code")
source ('rse09382-mmc2.r')

# pdf("PLS_Model_Fit.pdf")
setwd("~/Documents/phd/code/repeat repeat")
ensfit <- ensemble (AllData, unlist(catagories), em_range)
2
plot(ensfit)

stop()

##
#ClUSTERING
###################################################################

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
rect.hclust(dendrogram, 23)

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