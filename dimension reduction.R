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