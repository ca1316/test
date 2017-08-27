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
dens <- densityMclust(AllData)
summary(dens$BIC)
summary(dens, parameters = TRUE)

require(mixtools)
out <- mvnormalmixEM(AllData, lambda = NULL, mu = NULL, sigma = NULL,
                     k = 8,arbmean = TRUE, arbvar = TRUE, epsilon = 1e-08,  maxit = 10000, verb = FALSE)
plot(out, density = TRUE, alpha = c(0.01, 0.05, 0.10, 0.12, 0.15),  marginal = TRUE)

library(mclust)
plot(clustered, AllData, what = "BIC")
coordProj(AllData, dimens = c(1,2), what = "classification",
          classification = clustered$classification,
          parameters = clustered$parameters)
coordProj(AllData[,-1], dimens = c(1,2), what = "classification",
          classification = clustered$classification,
          parameters = clustered$parameters)

dendrogram <- hclust(dist(AllData))
plot(dendrogram)
rect.hclust(dendrogram, 9)
rect.hclust(dendrogram, 23)