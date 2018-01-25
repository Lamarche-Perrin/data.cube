rm(list=ls())

df <- read.csv ('../data/twitter.eu.raw.csv', sep='\t', stringsAsFactors=FALSE)

df$week <- df$week + (df$yr-2017)*53
df$day <- as.Date("2017-10-01") + (df$week-40) * 7

data <- df[,c('from_user_name','hashtag','day','count')]
names(data) <- c('user','topic','time','obs')
head(data)

write.csv (data, '../data/twitter.eu.csv', row.names=FALSE, quote=FALSE)
