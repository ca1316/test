#IMPORT DATA

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
    #  dat <- apply (dat, 2, function(x) x*whiteref[,2]) #apply white reference
    AllData <- rbind(AllData, t(dat[,2]))
    DataNames <- rbind(DataNames, paste("Cat_", iFo, "_sample_", iFi, sep = ""))
    catagories <- rbind(catagories, iFo)
  }
}


rownames(AllData) <- DataNames
