rm(list=ls())

library ('RCurl')
library('rjson')

## PARAMETERS
comment.time <- FALSE

url <- 'https://www.fcg-net.org/penelope/data/comment_structure'

fields <- list (
    collection = 'GuardianArticles',
    start_date = '2016-01-04', #'2017-01-02', #'2016-01-04',
    end_date = '2017-01-01', #'2017-01-29', #'2017-01-01',
    limit = 2
)

start.time <- Sys.time()

## SEND CURL REQUEST
fields[['start_date']] <- paste (fields[['start_date']], 'T00:00:00.000Z', sep='')
fields[['end_date']] <- paste (fields[['end_date']], 'T23:59:59.999Z', sep='')

fields.json <- toJSON (fields)

header <- c('Content-Type'='application/json')
data <- postForm (url, .opts=list (httpheader=header, postfields=fields.json))
data.json <- fromJSON (data)

## CREATE DATA.FRAME

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





## New formating function
data.json <- fromJSON (paste (readLines ('../outlier-explorer.api/guardian.raw'), collapse=""))

#users <- unique (unlist (sapply (data.json, function (article) unique (sapply (article$comments, function (comment) comment$author_id)))))

#topics <- unique (unlist (sapply (data.json, function (article) article$article_tags)))

time.format <- function (date) { date }
if (param$time$resolution == 'week') { time.format <- function (date) { 7 * floor (as.numeric (date-1+4) / 7) + as.Date (1-4, origin='1970-01-01') } }

start.time <- Sys.time()

data <- list()
for (article in data.json) {
    if (length (article$comments) > 0) {
        topics <- gsub (' ', '_', article$article_tags)
        times <- as.character (time.format (as.Date (substr (article$data_published, 1, 10))))
        obsPerUser <- table (sapply (article$comments, function (comment) comment$author_id))
        users <- names (obsPerUser)
        obss <- unname (obsPerUser) / length (topics)

        for (topic in topics) {
            if (is.null (data[[topic]])) data[[topic]] <- list()
            time <- times
            if (is.null (data[[topic]][[time]])) data[[topic]][[time]] <- list()
            for (i in seq_along(users)) {
                user <- users[i]
                obs <- obss[i]
                if (is.null (data[[topic]][[time]][[user]])) data[[topic]][[time]][[user]] <- 0
                data[[topic]][[time]][[user]] <- data[[topic]][[time]][[user]] + obs
            }
        }
    }
}

df <- data.frame (user=character(), topic=character(), time=character(), obs=numeric(), stringsAsFactors=FALSE)
for (topic in names(data)) {
    for (time in names(data[[topic]])) {
        for (user in names(data[[topic]][[time]])) {
            obs <- data[[topic]][[time]][[user]]
            df[nrow(df)+1,] <- list (user, topic, time, obs)
        }
    }
}

end.time <- Sys.time()
end.time - start.time



format_comments <- function (data, param=NULL) {
    if (is.null (param)) { param <- list() }
    if (is.null (param$time)) { param$time <- list() }
    if (is.null (param$time$reference)) { param$time$reference <- 'article' }
    if (is.null (param$time$resolution)) { param$time$reference <- 'week' }

    df <- data.frame (user=character(), topic=character(), time=character(), obs=integer())

    time.format <- function (date) { date }
    if (param$time$resolution == 'week') { time.format <- function (date) { 7 * floor (as.numeric (date-1+4) / 7) + as.Date (1-4, origin='1970-01-01') } }

    for (index in seq_along (data$data_published)) {
        if (length (data$comments[[index]]) > 0) {
            topics <- gsub (' ', '_', data$article_tags[[index]])
            users <- data$comments[[index]]$author_id
            
            if (param$time$reference == 'article') {
                times <- time.format (as.Date (substr (data$data_published[index], 1, 10)))
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
                times <- time.format (as.Date (substr (data$comments[[index]]$time_stamp, 1, 10)))

                sub.df <- aggregate (list(obs=rep(1,length(users))), by=list(user=users, time=times), FUN=sum)
                sub.df$obs <- sub.df$obs / length (topics)

                row.nb <- nrow (sub.df)
                sub.df <- sub.df [rep(1:row.nb, times=length(topics)),]
                sub.df$topic <- rep (topics, each=row.nb)
            }

            df <- rbind (df, sub.df)
        }
    }

    aggregate (obs ~ user + topic + time, data=df, FUN=sum)
}
