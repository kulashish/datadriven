source('../global/redshift_conn.R')
source('app_util.R')

trans <- YodleeInsightsConnector.query(conn, query)
trans$flag <- rep(0, nrow(trans))
cats  <<- read.csv('categories.txt', header=T)
cats.disc <<- cats[cats$discretionary==1, 'category']

trans.recur <- data.frame()
for(i in 1:nrow(trans)) {
  if(trans[i, 'flag'] == 1) next
  cond_match <- sim(trans$description[i], trans$description) < 2
  trans[cond_match, 'flag'] <- 1
  if(nrow(trans[cond_match, ]) >= 2) {
    process_write(trans[cond_match, ])
    trans.recur <- rbind(trans.recur, trans[i, ])
  }
}
trans.recur
