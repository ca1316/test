setwd('~/Documents/phd/code')
y<-read.csv('Workbook2.csv')
Z<-read.csv('repeat repeat tuk.csv')
setwd("~/Documents/phd/code/repeat repeat")
boxplot(y)
y<-na.omit(y)
ZZ<-aov(Cat_1~Cat_2, data=y)
summary(ZZ)
library(agricolae)
ZZZ<-HSD.test(Z$Cat_1, Z$Cat_2, 6, 47.3, group=T)
TukeyHSD(ZZ)
ZZZ
summary(ZZZ)
y<-as.matrix(y, skipNul = TRUE)
print(y)

HSD.test(wavelength)
TukeyHSD(y)
print
library(foreign)
tx <- with(y, interaction(fertilizer, irrigation))
amod <- aov(yield ~ tx, data=yield)
library(agricolae)
HSD.test(amod, "tx", group=TRUE)

boxplot(y, col=unlist(catagories))
# print(y)