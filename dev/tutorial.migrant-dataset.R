rm (list = ls())
source ("../src/data.cube.R")

## HASHTAG ANALYSIS

df.hashtag <- read_delim ("../data/migrant-dataset/migrant-day-userA-userB-hashtag.csv", delim = " ",
                          col_names = c ("day", "userA", "type", "userB", "hashtag", "count", "weight"),
                          col_types = cols (
                              day = col_character(),
                              userA = col_character(),
                              type = col_integer(),
                              userB = col_character(),
                              hashtag = col_character(),
                              count = col_integer(),
                              weight = col_double()
                          )
                          )

df.hashtag

dc.hashtag <-
    df.hashtag %>%
    ## filter (type == 1) %>%
    ## select (day, userA, hashtag, weight) %>%
    ## rename (user = userA, tweets = weight) %>%
    ## distinct (day, user, hashtag, .keep_all = TRUE) %>%
    as.data.cube (
        dim.names = c (day, user = userA, hashtag),
        var.names = c (tweets = weight)
    )

dc.hashtag %>% summary

dc.hashtag %>%
    select.dim (day) %>%
    arrange.elm (day, name) %>%
    plot.var (tweets)

dc.hashtag %>%
    select.dim (day) %>%
    group.day.elm (week) %>%
    plot.var (tweets)

keywords <- c ("emigrant", "emigrants", "immigrant", "immigrants", "migrant", "migrants", "Emigrant", "Emigrants", "Immigrant", "Immigrants", "Migrant", "Migrants")

dc.hashtag %>%
    select.dim (hashtag) %>%
    filter.elm (hashtag, ! name %in% keywords, name != "NULL") %>%
    top_n.elm (hashtag, tweets, n = 20) %>%
    arrange.elm (hashtag, desc (tweets)) %>%
    plot.var (tweets)

dc.hashtag %>%
    select.dim (hashtag) %>%
    filter.elm (hashtag, ! name %in% keywords, name != "NULL") %>%
    top_n.elm (hashtag, tweets, n = 100) %>%
    arrange.elm (hashtag, desc (tweets)) %>%
    as.data.frame %>%
    print (n = 100)

hashtag.list <- c ("Libia", "Iran", "Iraq", "Afghanistan", "Kosovo", "Eritrea", "Nigeria")

dc.countries <-
    dc.hashtag %>%
    filter.elm (hashtag, name %in% hashtag.list) %>%
    arrange.elm (hashtag, desc (tweets)) %>%
    select.dim (day, hashtag) %>%
    complete.elm %>%
    group.day.elm (week) %>%
    arrange.elm (week, name)

dc.countries %>%
    plot.var (tweets, sep.dim.names = hashtag, type = "line")

dc.countries %>%
    select.dim (hashtag) %>%
    as.data.frame

dc.countries %>%
    compute.var.model (tweets (week * hashtag), tweets (hashtag)) %>%
    plot.var (tweets.model, sep.dim.names = hashtag, type = "line")

dc.countries %>%
    compute.var.model (tweets (week * hashtag), tweets (hashtag)) %>%
    compute.var.deviation (tweets (week * hashtag)) %>%
    plot.var (tweets.deviation, sep.dim.names = hashtag, type = "bar")

dc.countries %>%
    compute.var.model (tweets (week * hashtag), tweets (hashtag)) %>%
    compute.var.deviation (tweets (week * hashtag), deviation.type = "chisq") %>%
    plot.var (tweets.deviation, sep.dim.names = hashtag, type = "bar")

dc.countries %>%
    compute.var.model (tweets (week * hashtag), tweets (hashtag)) %>%
    compute.var.deviation (tweets (week * hashtag), deviation.type = "chisq") %>%
    compute.var.outlier (tweets (week * hashtag)) %>%
    arrange.elm (list (week, hashtag), desc (tweets.deviation)) %>%
    as.data.frame


## USER ANALYSIS

dc.user <-
    df.hashtag %>%
    filter (type == 3) %>%
    group_by (userA, userB) %>%
    summarize (weight = sum (weight)) %>%
    ## rename (user = anoname) %>%
    ## distinct (user, .keep_all = TRUE) %>%
    as.data.cube (
        dim.names = c (spreader = userA, author = userB),
        var.names = c (tweets = weight),
        sub.dim.names = c (user = c (spreader, author))
    )

dc.user %>% summary

dc.main.users <-
    dc.user %>%
    select.dim (user) %>%
    filter.elm (user, tweets.spreader >= 100 & tweets.author >= 100)

dc.main.users %>% biplot.var (tweets.spreader, tweets.author, log = "xy")

main.users <- dc.main.users %>% pull.dim (user)

dc.selected.user <-
    dc.user %>%
    filter.elm (user, name %in% main.users)

igraph <-
    dc.selected.user %>%
    as.igraph (spreader, author, edge.weight = tweets)

communities <-
    igraph %>%
    as.undirected (mode = "collapse", edge.attr.comb = sum) %>%
    cluster_louvain ()

igraph <-
    igraph %>%
    delete.edges (which (E(igraph)$weight < 10))

edge.weight <- E(igraph)$weight %>% rescale (c (1, 10))

plot (communities, igraph,
      edge.width = edge.weight)

membership (communities)

pagerank <- page_rank (igraph)
pagerank$vector

dc.selected.user <-
    dc.selected.user %>%
    join (
        membership (communities) %>% as.data.cube (user),
        pagerank$vector %>% as.data.cube (user, pagerank)
    )

source ("../src/data.cube.R")

dc.community <-
    dc.selected.user %>%
    group.dim.by.var (
        user, community,
        main.user = name [order (desc (pagerank)) [1:5]] %>% paste0 (collapse = '\n'),
        #main.user = paste0 (name [which (pagerank > 0.5 * max (pagerank))], collapse = '\n'),
        count = n()
    )

dc.community %>% pull.var (main.user)

community.igraph <-
    dc.community %>%
    filter.elm (community, count >= 200) %>%
    mutate.var (community, name = main.user) %>%
    as.igraph (spreader, author, vertex.size = count, edge.weight = tweets)

library (ForceAtlas2)

vertex.size <- 50 * V(community.igraph)$size / max (V(community.igraph)$size)
edge.weight <- 30 * E(community.igraph)$weight / max (E(community.igraph)$weight)

set.seed (5)
vertex.coordinate <-
    community.igraph %>%
    layout.forceatlas2 (k = 400, gravity = 1, iterations = 2000, plotstep = 10)
community.igraph %>%
    plot (
        layout = vertex.coordinate,
        vertex.size = vertex.size,
        edge.width = edge.weight,
        edge.curved = 0.1
    )

help ("layout.forceatlas2")


source ("../src/data.cube.R")
dc.community %>%
    filter.elm (community, count >= 50) %>%
    mutate.var (community, name = main.user) %>%
    as.data.frame (complete = TRUE) %>%
    print (n = 100)

x
                                        # vertex.coordinate <- layout_with_fr (community.igraph, niter = 3000)
# https://igraph.org/r/doc/layout_with_fr.html
# vertex.coordinate <- layout_with_drl (community.igraph)
# https://igraph.org/r/doc/layout_with_drl.html


source ("../src/data.cube.R")


df.user <- read_delim ("../data/migrant-dataset/migrant-user.csv", delim = " ",
                         col_names = c ("id", "name", "anoname", "tweetcount", "followercount", "all_tweets", "tweets", "replies", "retweets", "received_replies", "received_retweets"),
                         col_types = cols (
                             id = col_character(),
                             name = col_character(),
                             anoname = col_character(),
                             tweetcount = col_integer(),
                             followercount = col_integer(),
                             all_tweets = col_integer(),
                             tweets = col_integer(),
                             replies = col_integer(),
                             retweets = col_integer(),
                             received_replies = col_integer(),
                             received_retweets = col_integer()
                         )
                         )



df.hashtag
