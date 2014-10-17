url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url, "pml-training.csv")
data <- read.csv("./pml-training.csv", header = TRUE, sep = ",")

library(caret)
library(plyr)
nsv <- nearZeroVar(data, saveMetrics = TRUE)
nsv2 <- ifelse(nsv$nzv == TRUE, 1, 0)
data1 <- data.frame(rbind(data, nsv2))
newdata <- data1[, -which((data1[19623,]) == 1)]
newdata1 <- newdata[, -which(is.na(newdata[19623,]))]



oui <- data.frame(cbind(rownames(nsv),nsv$nzv))
names(oui) <- c("name", "zero")
non <- subset(oui, zero == "FALSE")
non$ind <- rep(0, times = nrow(non))
for (i in 1:nrow(non)){if((non$name[i] %in% colnames(newdata1) ) == TRUE) {non$ind[i] <- "yes"} else {non$ind[i] <- "no"}}
a <- subset(non, ind == "no")
me <- as.data.frame(matrix(rep(0, times = nrow(data1)*length(a)), ncol = length(a)))
for(i in 1:length(a)){ me[,i] <- data1[,grep(a[i,1], colnames(data1))] }
names(me) <- a$name
wow <- as.data.frame(cbind(me, newdata1))

