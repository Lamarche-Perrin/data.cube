rm(list=ls())
source ('../data.cube.1.1.R')

## Import dataset
df <- read.csv ('../data/guardian.small.csv', stringsAsFactors=FALSE)
head (df)

## Create data.cube
dc <- as.data.cube (
    df,
    dim = list (user, topic, time),
    var = list (count = comments)
)

dc %>% str ()
dc %>% is.data.cube ()
dc %>% as.data.frame () %>% head ()

dc %>%
    compute.margins (user, topic, recursive = TRUE) %>%
    str()

## Explore temporal dimension
dc %>%
    select (time) %>%
    print ()

dc %>%
    select (time) %>%
    arrange.dim (time) %>%
    print ()

dc <-
    dc %>%
    arrange.dim (time)

dc %>%
    select (time) %>%
    print ()

dc %>%
    select (time) %>%
    plot.var ()

first.week <-
    dc %>%
    select (time) %>%
    head (time, n=1) %>%
    print.dim ()

last.week <-
    dc %>%
    select (time) %>%
    tail (time, n=1) %>%
    print.dim ()

dc <-
    dc %>%
    remove.elm (time, c (first.week, last.week), suppress=TRUE)

dc %>%
    select (time) %>%
    plot.var ()

## Explore topic dimension
dc %>%
    select (topic) %>%
    print ()

dc %>%
    select (topic) %>%
    arrange.var () %>%
    print ()

dc %>%
    select (topic) %>%
    arrange.var () %>%
    plot.var ()

dc %>%
    select (topic) %>%
    head (topic, n=5) %>%
    arrange.var () %>%
    plot.var ()

top.topics <-
    dc %>%
    select (topic) %>%
    head (topic, n=5) %>%
    arrange.var () %>%
    print.dim ()

dc %>%
    select (topic) %>%
    select.elm (topic, top.topics) %>%
    plot.var ()

## Explore topic and time
dc %>%
    select (topic, time) %>%
    select.elm (topic, top.topics) %>%
    plot.var ()

dc %>%
    select (topic, time) %>%
    select.elm (topic, top.topics) %>%
    arrange.dim (time, topic) %>%
    plot.var ()

dc %>%
    select (topic, time) %>%
    select.elm (topic, top.topics) %>%
    arrange.dim (time, topic) %>%
    plot.var (sep.dim = topic)

dc %>%
    select (topic, time) %>%
    select.elm (topic, top.topics) %>%
    arrange.dim (time, topic) %>%
    plot.var (sep.dim = time)

dc %>%
    select (user) %>%
    head (user, n=10) %>%
    arrange.var () %>%
    print ()

dc %>%
    select (topic, time, user) %>%
    select.elm (topic, top.topics) %>%
    select.elm (user, "4437052") %>%
    arrange.dim (time, topic) %>%
    plot.var (sep.dim = topic)

## Find outliers
source ('../data.cube.1.1.R')
dc2 <-
    dc %>%
    select (topic, time) %>%
    select.elm (topic, top.topics) %>%
    arrange.dim (time, topic)

dc2 %>%
    compute.model (topic) %>%
    print ()

dc2 %>%
    compute.model (topic) %>%
    plot.model ()

dc2 %>%
    compute.model (topic, deviation.type = 'poisson') %>%
    plot.model (sep.dim = topic)

dc2 %>%
    compute.model (topic, time, deviation.type = 'poisson') %>%
    plot.model (sep.dim = topic)





dc2 %>%
    compute.model (dim = c (topic, time)) %>%
    compute.outliers (type = 'poisson', threshold = 3) %>%
    plot.outliers ()

dc2 %>%
    compute.model (dim = c (topic, time)) %>%
    compute.outliers (type = 'poisson', threshold = 3) %>%
    print.outliers ()

## Yet another example
dc %>%
    select (user, topic) %>%
    filter (user, comments > 10) %>%
    compute.model (dim = c (user, topic)) %>%
    compute.outliers (type = 'poisson', threshold = 15) %>%
    plot.outliers ()
    



df <- read.csv ('../data/guardian.small.csv', stringsAsFactors=FALSE)
head (df)

dc <- as.data.cube (
    df,
    dimensions = list (user, week=time, topic),
    variables = list (tweets, comments)
)

dc %>% str()
dc %>% is.data.cube()
dc %>% as.data.frame() %>% head()

dc %>% compute.margins () %>% str()
dc %>% compute.margins (user) %>% compute.margins (topic) %>% str()
dc %>% compute.margins (user, topic) %>% str()
dc %>% compute.margins (user, topic, recursive=TRUE) %>% str()

select <- function (dc, ...) {
    params <- sapply (eval (substitute (alist (user, comments))), deparse)
    dimensions <- params [params %in% dc$dim.names]
    variables <- params [params %in% dc$var.names]

    ## Compute corresponding data plane
    old.dimensions <- dc$dim.names[dc$dim.names %in% dimensions]
    old.dp.name <- paste (old.dimensions, collapse='.')
    if (is.null (dc$obs[[old.dp.name]])) dc <- dc %>% compute.margins (dimensions = parse (text=dimensions))

    p <- parse (text=dimensions)
    sapply (eval (substitute (alist (p))), deparse)

    ## Adjust data cube attributes
    dc$dim.nb <- length (dimensions)
    dc$dim.names <- dimensions
    dc$elm.nb <- dc$elm.nb[dimensions]
    dc$elm.names <- dc$elm.names[dimensions]

    ## Adjust all data planes
    for (dp.name in names (dc$obs)) {
        if (dp.name == '.') next
        dims <- strsplit (dp.name, '.', fixed=TRUE)[[1]]
        if (all (dims %in% dimensions)) {
            new.dimensions <- dc$dim.names[dc$dim.names %in% dims]
            new.dp.name <- paste (new.dimensions, collapse='.')
            names (dc$obs) [names (dc$obs) == dp.name] <- new.dp.name
            dc$obs[[new.dp.name]]$elms <- dc$obs[[new.dp.name]]$elms[new.dimensions]
        }
        else dc$obs[[dp.name]] <- NULL
    }

    return (dc)
}


dc %>% select (user) %>% str()
dc %>% select (topic, user) %>% str()
dc %>% select (topic, user, time) %>% str()

dc %>% select (topic) %>% arrange (comments) %>% str()
dc %>% select (topic) %>% arrange (tweets) %>% str()

dc %>% compute.margins (topic, time, recursive=TRUE) %>% select (topic, time) %>% arrange (tweets, comments) %>% str()


dc %>% select (topic) %>% arrange (comments) %>% head (5) %>% str()
dc %>% select (topic) %>% arrange (comments) %>% tail (5) %>% str()

dc %>% compute.margins (topic, time, recursive=TRUE) %>% select (topic, time) %>% arrange (tweets, comments) %>% head (5) %>% str()


?head
