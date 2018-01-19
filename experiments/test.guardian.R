rm(list=ls())
source ('../data.cube.R')

df <- read.csv ('../data/guardian.small.csv', stringsAsFactors=FALSE)

## prevM <- function (x) { return (7 * floor(as.numeric(x-1+4) / 7) + as.Date(1-4,origin = "1970-01-01")) }
## df$time <- prevM (as.Date (df$time))
## df <- aggregate (list(obs=df$obs), by=list(user=df$user, topic=df$topic, time=df$time), FUN=sum)
## df <- df[! df$time %in% c(min(df$time), max(df$time)),]
## write.csv (df, 'guardian.csv', quote=FALSE, row.names=FALSE)

dc <- as.data.cube (df)
str(dc)

dc.dev <- remove.dims (dc, dims=c('user'))
dc.dev <- compute.expected (dc.dev, dims=c('topic','time'))
dc.dev <- compute.deviated (dc.dev)
dc.dev <- compute.outliers (dc.dev, threshold=4)
str(dc.dev)

plot.outliers (dc.dev, labels=FALSE)


data.nb <- sum (abs (dc.dev$data$dev) < 1)
data.distribution (dc.dev, data='dev', threshold=3) +
    coord_cartesian (ylim = c(0, data.nb / 300))


indices <- which (dc.dev$data[['out']] == 1)
df <- as.data.frame (cbind (as.data.frame (dc$cells), as.data.frame (dc$data)))

dc.plot <- select.elems (dc, list('user'=15131989, 'topic'=c('Science','Energy','Politics')))
dc.plot <- compute.expected (dc.plot, dims=c('user','topic'))
    
##dc.plot$data$rank <- rank(-dc.plot$data$obs)
##dc.plot$data$display <- (dc.plot$data$obs >= 10)
str(dc.plot)

plot.data (dc.plot, data='obs/exp') #, sep.dim='topic')

df <- as.data.frame (cbind (as.data.frame (dc.plot$cells), as.data.frame (dc.plot$data)))
head(df)
str(df)

dummy <- lapply (dc.plot$dim.names, function (dim) { return (df[[dim]] <<- as.character (dc.plot$elem.names[[dim]][df[[dim]]])) })
