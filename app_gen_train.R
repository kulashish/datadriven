source('../global/redshift_conn.R')
source('app_util.R')
library(rpart)
library(RWeka)

cats  <<- read.csv('categories.txt', header=T)
cats.disc <<- cats[cats$discretionary==1, 'category']
patterns <<- read.csv('subscription.txt', header=F)
#trans <- YodleeInsightsConnector.query(conn, query.rand)
trans <- YodleeInsightsConnector.query(conn, query.fix)
trans[, 1]  <- as.character(trans[, 1])
trans[, 2]  <- as.character(trans[, 2])
data.sim_df <- do.call("rbind", lapply(split(trans, trans$mem_id), find_similar))
process_write(data.sim_df, 'trans_features.csv')

#--------Train a PART model -------------------
data  <- read.csv('app_train1.csv', header=T)
data[is.na(data)]   <- 0
#model.dt  <- rpart(label ~ date_diff+date_var+amt+cat+discret+sub, data=data, method='class', control = rpart.control(cp = 0, xval=2))
m <- PART(label ~ date_diff+date_var+amt+cat+discret+sub, data=data)
print(m)
#plot(model.dt)
#text(model.dt)
pred <- trans_predict(m, data)

#--------Computation of Accuracy measures----------------------
TP <- pred[(pred$label=='Valid') & (pred$pred=='Valid'), ]
TN <- pred[(pred$label=='Invalid') & (pred$pred=='Invalid'), ]
FP <- pred[(pred$label=='Invalid') & (pred$pred=='Valid'), ]
FN <- pred[(pred$label=='Valid') & (pred$pred=='Invalid'), ]
Recall <- nrow(TP) / (nrow(TP)+nrow(FN))
Precision <- nrow(TP) / (nrow(TP)+nrow(FP))
F1 <- 2 * Recall * Precision / (Recall + Precision)

#--------Inference-----------------------------
instances.test <- YodleeInsightsConnector.query(conn, query.rand)
instances.test[, 1] <- as.character(instances.test[, 1])
data.sim_df <- do.call("rbind", lapply(split(instances.test, instances.test$mem_id), find_similar))
data.sim_df[is.na(data.sim_df)] <- 0
data.sim_df[, 'date_diff'] <- gsub(" weeks", "", data.sim_df[, 'date_diff'])
#data.test <- cbind(data.sim_df, pred=trans_predict(m, data.sim_df))
data.test <- trans_predict(m, data.sim_df)
process_write(data.test, 'test.csv')