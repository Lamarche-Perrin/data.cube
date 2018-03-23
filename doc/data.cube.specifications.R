library (data.cube)

## Import dataset
df <- read.csv ('../data/guardian.small.csv', stringsAsFactors=FALSE)
head (df)

## Create data.cube
dc <- as.data.cube (
    df,
    dim = list (user=user_id, topic=article_keyword, time=publication_date),
    var = comments
)

dc %>% str()
dc %>% is.data.cube()
dc %>% as.data.frame() %>% head()

dc %>%
    compute.margins (user, topic, recursive = TRUE) %>%
    str()

## Explore temporal dimension
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
    print (var = NULL)

last.week <-
    dc %>%
    select (time) %>%
    tail (time, n=1) %>%
    print (var = NULL)

dc <-
    dc %>%
    remove.elm (time = c(firstWeek, lastWeek), suppress=TRUE)

dc %>%
    select (time) %>%
    plot.var ()

## Explore topic dimension
dc %>%
    select (topic) %>%
    plot.var ()

dc %>%
    select (topic) %>%
    head (topic, n=5) %>%
    plot.var ()

top.topics <-
    dc %>%
    select (topic) %>%
    head (topic, n=5) %>%
    print (var = NULL)

dc %>%
    select (topic = top.topics) %>%
    plot.var ()

dc %>%
    select (dim = topic, elm = list (topic = top.topics)) %>%
    plot.var ()

## Explore topic and time
dc %>%
    select (topic = top.topics, time) %>%
    plot.var ()

dc %>%
    select (topic = top.topics, user = 'user1', time) %>%
    plot.var ()

dc2 <-
    dc %>%
    select (topic = top.topics, time) %>%
    
dc2 %>%
    compute.model (dim = topic) %>%
    plot.model ()

dc2 %>%
    compute.model (dim = c (topic, time)) %>%
    plot.model ()

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
    
