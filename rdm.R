url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url, "pml-training.csv")
data <- read.csv("./pml-training.csv", header = TRUE, sep = ",")

library(caret)
library(plyr)
nsv <- nearZeroVar(data, saveMetrics = TRUE)
data <- data.frame(rbind(data, ifelse(nsv$nzv == TRUE, 1, 0)))
newdata <- data[, -which((data[nrow(data),]) == 1)]
newdata <- newdata[, -which(is.na(newdata[nrow(data),]))]

non <- subset(nsv, nzv == FALSE)
non$ind <- rep(0, times = nrow(non))
for (i in 1:nrow(non)){if((row.names(non[i,]) %in% colnames(newdata) ) == TRUE)
                                {non$ind[i] <- "yes"} else {non$ind[i] <- "no"}}
a <- subset(non, ind == "no")
oui <- as.data.frame(matrix(rep(0, times = nrow(data)*nrow(a)), ncol = nrow(a)))
for(i in 1:nrow(a)){oui[,i] <- data[,grep(row.names(a[i,]), colnames(data))]}
names(oui) <- row.names(a)
data <- as.data.frame(cbind(oui, newdata))
data <- data[-nrow(data),]
#imputing missing values
#pca
prComp <- prcomp(data[,4:ncol(data)])