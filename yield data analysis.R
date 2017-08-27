setwd("~/Desktop")
yield <- read.csv('throws.txt')

palette(c(rgb(185,130,62, maxColorValue=255),
          rgb(137,93,181, maxColorValue=255),
          rgb(100,157,97, maxColorValue=255),
          rgb(180,78,94, maxColorValue=255)))
palette()
pal<-palette()
yield <- as.matrix(yield)
barplot(yield, col=pal)

head(yield)
yield <- t(yield)
yield <- as.matrix(yield)
barplot(yield, xlab = c('replicates'))

yield <- t(yield)
boxplot(yield)

head(yield)
yieldwith <- t(yield)
yieldwith <- as.matrix(yield)
barplot(yieldwith, xlab = c('replicates'))

yieldwith <- t(yield)
boxplot(yield)

as.list(yield)
yield <- as.numeric(as.list(yield))

catagory<-factor(gl(12,8,96 ,labels=c('a','b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l')))
catagory
cbind(yield, catagory)

bartlett.test(yield, catagory)
qchisq(0.950, 3)
fligner.test(yield, catagory)

afit <- aov(yield~catagory)
summary.aov(afit)

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

kruskal.test(yieldwith ~ catagory)


############################################################################################

yieldno <- read.csv('throws no fung.txt')
palette(c(rgb(185,130,62, maxColorValue=255),
          rgb(137,93,181, maxColorValue=255),
          rgb(100,157,97, maxColorValue=255),
          rgb(180,78,94, maxColorValue=255)))
palette()
pal<-palette()
yieldno <- as.matrix(yieldno)
barplot(yieldno, col=pal)

total <- colSums (yieldno, na.rm = FALSE, dims = 1)
a <- (total[1])/(total[1])
b <- (total[2])/(total[1])
c <- (total[3])/(total[1])
d <- (total[4])/(total[1])
e <- (total[5])/(total[1])
f <- (total[6])/(total[1])

percent <- cbind(a,b,c,d,e, f)
barplot(percent, xlab= 'total % yield', col='blue', ylim= c(0, 1.2))

means <- .colMeans(yieldno, 4, 6, na.rm = FALSE)
a <- (means[1])/(means[1])
b <- (means[2])/(means[1])
c <- (means[3])/(means[1])
d <- (means[4])/(means[1])
e <- (means[5])/(means[1])
f <- (means[6])/(means[1])
percent <- cbind(a,b,c,d,e, f)
barplot(percent, xlab= 'mean % yield', col='red', ylim= c(0, 1.2))

head(yieldno)
yieldno <- t(yieldno)
yieldno <- as.matrix(yieldno)
barplot(yieldno, xlab = c('replicates'))

yieldno <- t(yieldno)
boxplot(yieldno)

as.list(yieldno)
yieldno <- as.numeric(as.list(yieldno))

catagory<-factor(gl(6,8,48 ,labels=c('a','c', 'e', 'g', 'i', 'k')))
catagory
cbind(yieldno, catagory)

bartlett.test(yieldno, catagory)
qchisq(0.950, 3)
fligner.test(yieldno, catagory)

afit <- aov(yieldno~catagory)
summary.aov(afit)

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

kruskal.test(yieldno ~ catagory)

####################################################################################

yieldwith <- read.csv('throws with fung.txt')
palette(c(rgb(185,130,62, maxColorValue=255),
          rgb(137,93,181, maxColorValue=255),
          rgb(100,157,97, maxColorValue=255),
          rgb(180,78,94, maxColorValue=255)))
palette()
pal<-palette()
yieldwith <- as.matrix(yieldwith)
barplot(yieldwith, col=pal)

total <- colSums (yieldwith, na.rm = FALSE, dims = 1)
a <- (total[1])/(total[1])
b <- (total[2])/(total[1])
c <- (total[3])/(total[1])
d <- (total[4])/(total[1])
e <- (total[5])/(total[1])
f <- (total[6])/(total[1])

percent <- cbind(a,b,c,d,e, f)
barplot(percent, xlab= 'total % yield', col='blue', ylim= c(0, 1.2))

means <- .colMeans(yieldwith, 4, 6, na.rm = FALSE)
a <- (means[1])/(means[1])
b <- (means[2])/(means[1])
c <- (means[3])/(means[1])
d <- (means[4])/(means[1])
e <- (means[5])/(means[1])
f <- (means[6])/(means[1])
percent <- cbind(a,b,c,d,e, f)
barplot(percent, xlab= 'mean % yield', col='red', ylim= c(0, 1.2))

head(yieldwith)
yieldwith <- t(yieldwith)
yieldwith <- as.matrix(yieldwith)
barplot(yieldwith, xlab = c('replicates'))

yieldwith <- t(yieldwith)
boxplot(yieldwith)

as.list(yieldwith)
yieldwith <- as.numeric(as.list(yieldwith))

catagory<-factor(gl(6,8,48 ,labels=c('b','d', 'f', 'h', 'j', 'l')))
catagory
cbind(yieldwith, catagory)

bartlett.test(yieldwith, catagory)
qchisq(0.950, 3)
fligner.test(yieldwith, catagory)

afit <- aov(yieldwith~catagory)
summary.aov(afit)

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

kruskal.test(yieldwith ~ catagory)

