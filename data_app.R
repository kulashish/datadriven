source('app_util.R')
library(rpart)
library(RWeka)

cats  <<- read.csv('categories.txt', header=T)
cats.disc <<- cats[cats$discretionary==1, 'category']
patterns <<- read.csv('subscription.txt', header=F)

data  <- read.csv('app_train1.csv', header=T)
data[is.na(data)]   <- -1
m <- PART(label ~ date_diff+date_var+amt+cat+discret+sub, data=data)

data.raw <- read.table('C:/Users/akulkarni/Documents/Yodlee/Data driven app/EJ/Export Data Checking 1 - EJ.csv', numerals="no.loss", sep="|", quote="", comment.char="", header = T, stringsAsFactors=F)
data.raw$mem_id <- rep(1, nrow(data.raw))
data.raw$transaction_id <- seq.int(1, nrow(data.raw))
#data.raw <- read.csv('C:/Users/akulkarni/Documents/Yodlee/Data driven app/ravindra/BofA_checking.csv', numerals="no.loss", header = T, stringsAsFactors=F)
#data.raw[, 1]  <- as.character(data.raw[, 1])
#data.raw[, 2]  <- as.character(data.raw[, 2])
#data.raw[, 4]  <- as.numeric(data.raw[, 4])
data.sim.df <- do.call("rbind", lapply(split(data.raw, data.raw$mem_id), find_similar))
data.sim.df[is.na(data.sim.df)] <- -1
data.sim.df[, 'date_diff'] <- gsub(" weeks", "", data.sim.df[, 'date_diff'])
data.test <- trans_predict(m, data.sim.df)
data.test[, 1] <- as.character(data.test[, 1])
data.test[, 2] <- as.character(data.test[, 2])
data.test[, 3] <- as.character(data.test[, 3])
data.test[data.test$pred=='Valid', ]

data.result <- data.test[data.test$pred=='Valid', c(F, T, T, F, F, F, F, T, F, T, F)]
data.result <- cbind(data.result, data.raw[data.result$transaction_id==data.raw$transaction_id, c('amount', 'description')])
colnames(data.result) <- c('Transation ID', 'Similar Transactions', 'Category', 'Subscription', 'Amount', 'Description')
rownames(data.result) <- NULL
write.csv(data.test[, c(2, 3, 8:11)], file='BofA_checking_out.csv')
