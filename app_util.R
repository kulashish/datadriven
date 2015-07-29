query.rand  <- "select mem_id, description, amount, transaction_category as category, optimized_transaction_date as date from yi_base_views.bank_panel where
mem_id in (select distinct mem_id from yi_base_views.bank_panel order by random() limit 10) and transaction_date > '2014-09-01' order by 1"

query.fix  <- "select mem_id, transaction_id, description, amount, transaction_category as category, optimized_transaction_date as date from yi_base_views.bank_panel where
mem_id in (1000164415352201, 1000564012973460, 1000564013171293, 1000564013502219, 1000564014242573, 1000616412894541, 1000564012609530, 1000564013137781, 1000564014123899, 1000564014583825, 1000616410283476) and transaction_date > '2014-09-01' order by 1"

query <- "select description, amount, transaction_category as category, optimized_transaction_date as date from yi_base_views.bank_panel where mem_id=1000616410222094 and transaction_date > '2014-08-01'"

DATE_FORMAT <- "%d/%m/%Y"

find_similar  <- function(X){
  X$flag  <- rep(0, nrow(X))
  data.sim <- data.frame()
  for(i in 1:nrow(X)) {
    if(X[i, 'flag'] == 1) next
    cond_match <- sim(X$description[i], X$description) < 2
    X[cond_match, 'flag'] <- 1
    if(nrow(X[cond_match, ]) >= 2) {
      temp <- do.call("rbind", lapply(split(X[cond_match, ], X[cond_match, ]$category), feature_vector))
      if(nrow(data.sim) == 0)
        #data.sim <- feature_vector(X[cond_match, ])
      #print(split(X[cond_match, ], X[cond_match, ]$category))
        data.sim <- temp
      else 
        data.sim <- rbind(data.sim, temp)        
    }
  }
  return (data.sim)
}

strprocess <- function(x) {
  x <- gsub("[^[:alpha:]]", "", x)
  x <- gsub("X", "", x)
  return (tolower(x))
}

sim <- function(x, y){
  x <- strprocess(x)
  y <- strprocess(y)
  return (adist(x, y))
}

date_diff <- function(x, y){
  return (difftime(strptime(x, format="%d/%m/%Y"), strptime(y, format="%d/%m/%Y"), units="weeks"))
#  return (difftime(strptime(x, format="%d-%m-%Y"), strptime(y, format="%d-%m-%Y"), units="weeks"))
}

Mode<-function(x){ux<-unique(x); ux[which.max(tabulate(match(x,ux)))]};

load_data <- function(file) {
  x <- read.table(file, numerals="no.loss", sep="|", quote="", comment.char="", header = T, stringsAsFactors=F)
  x$mem_id <- rep(1, nrow(x))
  x$transaction_id <- seq.int(1, nrow(x))
  return(x)
}

filter_similar <- function(x) {
  sim <- do.call("rbind", lapply(split(x, x$mem_id), find_similar))
  sim[is.na(sim)] <- -1
  sim[, 'date_diff'] <- gsub(" weeks", "", sim[, 'date_diff'])
  return(sim)
}

build_model <- function() {
  cats  <<- read.csv('categories.txt', header=T)
  cats.disc <<- cats[cats$discretionary==1, 'category']
  patterns <<- read.csv('subscription.txt', header=F)
  
  data  <- read.csv('app_train1.csv', header=T)
  data[is.na(data)]   <- -1
  m <- PART(label ~ date_diff+date_var+amt+cat+discret+sub, data=data)
}

feature_vector <- function(x) {
  order.date      <- order(as.Date(x$date, format=DATE_FORMAT))
  x               <- x[order.date, ]
  diff.date       <- date_diff(tail(x$date, -1), head(x$date, -1))
  diff.date.avg   <- round(mean(diff.date), digits = 2)
  diff.date.var   <- round(var(diff.date), digits = 2)
  #x$norm_amount <- (x$amount - min(x$amount)) / (max(x$amount) - min(x$amount))
  #amt.sd        <- round(sd(x$norm_amount, na.rm=T), digits = 2)
  amt.var_to_mean <- round(var(x$amount) / mean(x$amount), digits = 2)
  trans.cat       <- Mode(x$category)
  trans.cat.d     <- trans.cat %in% cats.disc
  trans.desc      <- strprocess(x$description[1])
  trans.sub       <- Reduce('|', lapply(patterns$V1, grepl, trans.desc))
  data.features      <- data.frame(mem=unique(x$mem_id), transaction_id=x$transaction_id[1], similar_transactions=paste(x$transaction_id[-1], collapse="|"), desc=trans.desc, date_diff=diff.date.avg, date_var=diff.date.var, amt=amt.var_to_mean, cat=trans.cat, discret=trans.cat.d, sub=trans.sub)
  return(data.features)
}

process_write <- function(x, file){
 # data.train <- feature_vector(x)
  write.table(x, file=file, append=T, row.names=F, col.names=F,  sep=",")
}

trans_predict <- function(m, x) {
  x$pred <- predict(m, x)
  x$pred[which(x$discret=='TRUE' | x$cat=='Credit Card Payments')] <- 'Invalid'
  x$pred[which(x$cat=='Paychecks/Salary' | x$cat=='Other Income' | x$sub=='TRUE')] <- 'Valid'
  return (x)
}

build_result <- function(raw, processed) {
  result <- processed[processed$pred=='Valid', c(F, T, T, F, F, F, F, T, F, T, F)]
  result <- merge(result, raw[, c('transaction_id', 'description', 'amount')], by='transaction_id')
  #result <- cbind(result, raw[result$transaction_id==raw$transaction_id, c('amount', 'description')])
  colnames(result) <- c('Transation ID', 'Similar Transactions', 'Category', 'Subscription', 'Description', 'Amount')
  rownames(result) <- NULL
  return(result)
}