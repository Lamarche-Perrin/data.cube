rm(list=ls())

library ('RCurl')
library('rjson')

## PARAMETERS
comment.time <- FALSE

url <- 'https://www.fcg-net.org/penelope/data/comment_structure'

fields <- list (
    collection = 'GuardianArticles',
    start_date = '2016-01-04',
    end_date = '2017-01-01',
    limit = 0
)

start.time <- Sys.time()

## SEND CURL REQUEST
fields[['start_date']] <- paste (fields[['start_date']], 'T00:00:00.000Z', sep='')
fields[['end_date']] <- paste (fields[['end_date']], 'T23:59:59.999Z', sep='')

fields.json <- toJSON (fields)

header <- c('Content-Type'='application/json')
data <- postForm (url, .opts=list (httpheader=header, postfields=fields.json))


## CREATE DATA.FRAME

data.json <- fromJSON (data)

times <- unique (unlist (lapply (data.json, function (article) substr (article$data_published, 1, 10))))

df <- data.frame (user=character(), topic=character(), time=character(), obs=integer())

#article.index <- 0
#article.nb <- length (data.json)
prevM <- function (x) 7 * floor (as.numeric (x-1+4) / 7) + as.Date (1-4, origin="1970-01-01")

for (article in data.json) {
    if (length (article$comments) > 0) {
        topics <- gsub (' ', '_', article$article_tags)
        users <- sapply (article$comments, function (comment) comment$author_id)
        
        if (!comment.time) {
            times <- prevM (as.Date (substr (article$data_published, 1, 10)))
            sub.df <- as.data.frame (table (users))
            sub.df$time <- times
            
            names(sub.df)[1] <- 'user'
            names(sub.df)[2] <- 'obs'
            sub.df$obs <- sub.df$obs / length (topics)

            row.nb <- nrow (sub.df)
            sub.df <- sub.df [rep(1:row.nb, times=length(topics)),]
            sub.df$topic <- rep (topics, each=row.nb)

            sub.df <- sub.df[,c(1,4,3,2)]
        }

        else {
            times <- prevM (as.Date (sapply (article$comments, function (comment) substr (comment$time_stamp, 1, 10))))

            sub.df <- aggregate (list(obs=rep(1,length(users))), by=list(user=users, time=times), FUN=sum)
            sub.df$obs <- sub.df$obs / length (topics)

            row.nb <- nrow (sub.df)
            sub.df <- sub.df [rep(1:row.nb, times=length(topics)),]
            sub.df$topic <- rep (topics, each=row.nb)
        }

        df <- rbind (df, sub.df)
    }

    #article.index <- article.index + 1
    #print (article.index / article.nb * 100)
}

df <- aggregate (obs ~ user + topic + time, data=df, FUN=sum)

end.time <- Sys.time()
end.time - start.time

#head(df)
#write.csv (df, "guardian.2016.csv", row.names=FALSE, quote=FALSE)
