library ('magrittr')

source ('../src/data.cube.R')

data <- data.frame()

#' @get /tools/ping
ping <- function () { return ("OK!"); }

#' @post /tools/format_comments
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


#' @post /tools/outliers
outliers <- function (data=NULL, dataset=NULL, param=NULL) {

    ## Load dataset
    if (! is.null (dataset)) {
        data <- paste ('../data/', dataset, '.csv', sep='') %>%
            read.csv (stringsAsFactors=FALSE)
        dc <- data %>% as.data.cube_(dim = names(data) [1:(length(data)-1)], var = names(data) [length(data)])
    } else {
        if (is.null (data)) { data <- data.frame () }
        dc <- data %>% as.data.cube_(dim = names(data) [1:(length(data)-1)], var = names(data) [length(data)])
    }
    
    ## Select dimensions
    dims <- c()
    for (index in 1:nrow(param$select)) {
        dim <- as.list (param$select[index,])
        dims <- append (dims, dim$dim)
        if (dim$select == 'some') {
            if (! is.null (dim$list[[1]])) { dc <- dc %>% select.elm_(dim$dim, elm.array = dim$list[[1]]) }
            if (! is.na (dim$head)) { dc <- dc %>% select.elm_(dim$dim, top.nb = dim$head) }
        }
    }

    dc <- dc %>% select.dim_(dims)

    ## Compute outliers
    dc <- dc %>% compute.model_(dim = param$normalise, deviation.type = param$stat.test$type, deviation.threshold = param$stat.test$threshold)

    ## Return list
    df <- dc %>% as.data.frame () %>% filter (outlier == 1) %>% select (-outlier) %>% arrange (desc (deviation))

    return (df)
}


## dataset <- "guardian.2016"
## param <- list()
## param$select <- data.frame (dim = c ("topic", "week"), select = c ("some", "all"), head = c (5, NA), stringsAsFactors = FALSE)
## param$normalise <- c("topic","time")
## param$stat.test <- list (type = "poisson", threshold = 3)
## param
