library ('magrittr')

source ('../data.cube.R')

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
        dc <- paste ('../data/', dataset, '.csv', sep='') %>%
            read.csv (stringsAsFactors=FALSE) %>%
            as.data.cube ()
    } else {
        if (is.null (data)) { data <- data.frame () }
        dc <- as.data.cube (data)
    }
    
    ## Select dimensions
    dims <- c()
    elems <- list()
    for (index in 1:nrow(param$select)) {
        dim <- as.list (param$select[index,])
        dims <- append (dims, dim$dim)
        if (dim$select == 'some') {
            if (! is.null (dim$list)) { elems[[dim$dim]] <- dim$list }
            if (! is.na (dim$head)) { elems[[dim$dim]] <- dc$elem.names[[dim$dim]][dc$margins[[dim$dim]]$cells[[dim$dim]][order(-dc$margins[[dim$dim]]$data$obs)[1:dim$head]]] }
        }
    }
    if (length (elems) > 0) { dc <- select.elems (dc, elems) }

    dims <- dc$dim.names [! dc$dim.names %in% dims]
    if (length (dims) > 0) { dc <- remove.dims (dc, dims) }

    ## Normalise data
    dc <- compute.expected (dc, dims=param$normalise)

    ## Compute outliers
    dc <- compute.deviated (dc, type=param$stat.test$type)
    dc <- compute.outliers (dc, threshold=param$stat.test$threshold)
    
    dc$data$display <- (dc$data$out == 1)
    dc$data$rank <- rank (-dc$data$dev)

    ## Return list
    df <- as.data.frame (dc, display='display', rank='rank')
    df$out <- NULL

    return (df)
}

