rm(list=ls())

library ('R.matlab')

data <- readMat ('../data/opinion.traces.mat')

sizes <- dim(data$DAT)
user <- paste ('a', 1:sizes[1], sep='')
topic <- paste ('arg', 1:sizes[2], sep='')
time <- 1:sizes[3]

dimnames(data$DAT) <- list (user=user, topic=topic, time=time)

df <- as.data.frame.table (data$DAT, responseName="obs", stringsAsFactors=FALSE)
df <- df[df$obs == 1,]

df$time <- as.numeric (df$time)
df$time <- floor((df$time-1)/10)*10 + 10
df <- aggregate (obs ~ user + topic + time, data=df, FUN=sum)
head(df)

write.csv (df, '../data/opinion.traces.csv', row.names=FALSE, quote=FALSE)

