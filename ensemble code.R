##
#ENSEMBLE SECTION
##

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
