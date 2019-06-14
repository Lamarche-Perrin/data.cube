rm (list = ls())
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

dc.user <-
    df.user %>%
    rename (user = anoname) %>%
    distinct (user, .keep_all = TRUE) %>%
    as.data.cube (
        dim.names = c (user),
        var.names = c (tweetcount, followercount)
    )

dc.user %>% summary


dc.hashtag <-
    df.hashtag %>%
    filter (type == 1) %>%
    select (day, userA, hashtag, weight) %>%
    rename (user = userA, tweets = weight) %>%
    distinct (day, user, hashtag, .keep_all = TRUE) %>%
    as.data.cube (
        dim.names = c (day, user, hashtag),
        var.names = c (tweets)
    )

dc.hashtag %>% summary
dc.hashtag %>% str

dc <- join (dc.user, dc.hashtag)

dc %>% summary


dc %>%
    str

    select.dim (day) %>%
    str

    arrange.elm (day, name) %>%

    
    plot.var (tweets)
