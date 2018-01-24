rm(list=ls())

library ('RCurl')
library('rjson')

## PARAMETERS
url <- 'https://www.fcg-net.org/penelope/data/comment_structure'

fields <- list (
    collection = 'GuardianArticles',
    start_date = '2017-01-02',
    end_date = '2017-12-31',
    limit = 0
)

start.time <- Sys.time()

## SEND CURL REQUEST
fields[['start_date']] <- paste (fields[['start_date']], 'T00:00:00.000Z', sep='')
fields[['end_date']] <- paste (fields[['end_date']], 'T23:59:59.999Z', sep='')

fields.json <- toJSON (fields)

header <- c('Content-Type'='application/json')
data <- postForm (url, .opts=list (httpheader=header, postfields=fields.json))

data.json <- fromJSON (data)


## COMPUTE TIME DIFF BETWEEN COMMENTS AND ARTICLES

time.diff <- unlist (sapply (data.json, function (article) {
    article.time <- as.Date (substr (article$data_published, 1, 10))
    sapply (article$comments, function (comment) as.numeric (as.Date (substr (comment$time_stamp, 1, 10)) - article.time))
}))

(table(time.diff)[1] + table(time.diff)[2]) / sum(table(time.diff))

png ('guardian.time.diff.png', res=100, width=600, height=600)
hist (time.diff)
dev.off()
