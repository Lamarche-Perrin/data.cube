rm (list = ls())

source ("../src/data.cube.R")


## 1. DATA PREPARATION

## 1.1. Import dataset
df <- read.csv ("../data/geomedia.csv", stringsAsFactors = FALSE)
head (df)

## 1.2. Create data.cube
dc <- as.data.cube (
    df,
    dim = list (media = id_media, space = id_country, time = week),
    var = list (articles = data)
)

## 1.3. See intern data structure
str (dc)

## 1.4. Properties of the data.cube
dim.nb (dc)
dim.names (dc)

elm.nb (dc)
elm.names (dc)

elm.nb (dc, media)
elm.names (dc, media)

summary (dc)

## 1.5. Transform data.cube back into data.frame
dc %>% as.data.frame () %>% head ()


## 2. EXPLORE TEMPORAL DIMENSION

## 2.1. Get temporal dimension as a data.frame
dc %>%
    select.dim (time) %>%
    as.data.frame ()

## 2.2. Order temporal dimension by dates and return it as a data.frame
dc %>%
    select.dim (time) %>%
    arrange.elm (time) %>%
    as.data.frame ()

## 2.3. Order temporal dimension by dates, and save the modification into the data structure
dc <-
    dc %>%
    arrange.elm (time)

dc %>%
    select.dim (time) %>%
    as.data.frame ()

## 2.4. Plot variable along the temporal dimension
dc %>%
    select.dim (time) %>%
    plot.obs ()

dc %>%
    select.dim (time) %>%
    plot.obs (type = "line")

## 2.5. Suppress first and last week in the data, then plot variable along the temporal dimension
weeks <- elm.names (dc, time)
weeks
min (weeks)
max (weeks)

dc %>%
    select.dim (time) %>%
    remove.elm (time, c (min (weeks), max (weeks)), suppress = TRUE) %>%
    plot.obs ()

## 2.6. Suppress first and last week in the data, and save the modification into the data structure
dc <-
    dc %>%
    remove.elm (time, c (min (weeks), max (weeks)), suppress = TRUE)

dc %>%
    select.dim (time) %>%
    plot.obs ()


## 3. EXPLORE SPACIAL DIMENSION

## 3.1. Ordered by countries' name
dc %>%
    select.dim (space) %>%
    arrange.elm (space) %>%
    plot.obs ()

## 3.2. Only the top 50 countries, ordered by values
dc %>%
    select.dim (space) %>%
    select.elm (space, top.nb = 50) %>%
    arrange.obs () %>%
    plot.obs ()

## 3.3. Only countries with more than 5000 articles, ordered by values
dc %>%
    select.dim (space) %>%
    filter.elm (space, articles > 5000) %>%
    arrange.obs () %>%
    plot.obs ()

## 3.4. Only a subset of designated countries, ordered by values
elm.names (dc, space)
G8 <- c ("USA", "JPN", "DEU", "FRA", "RUS", "GBR", "ITA", "CAN")

dc %>%
    select.dim (space) %>%
    select.elm (space, G8) %>%
    arrange.obs () %>%
    plot.obs ()


## 4. EXPLORE SPACIO-TEMPORAL DIMENSION

## 4.1. Plot space and time
dc %>%
    select.dim (space, time) %>%
    select.elm (space, G8) %>%
    plot.obs ()

## 4.2. Plot space along time
dc %>%
    select.dim (space, time) %>%
    select.elm (space, G8) %>%
    plot.obs (sep.dim = space)

dc %>%
    select.dim (space, time) %>%
    select.elm (space, G8) %>%
    plot.obs (sep.dim = space, type = "line")

## 4.3. Bidimensional plot of space and time
dc %>%
    select.dim (space, time) %>%
    select.elm (space, G8) %>%
    biplot.obs (space, time)

## 4.4. Bidimensinal plot of space and time, for a particular media
elm.names (dc, media)

dc %>%
    select.dim (media, space, time) %>%
    select.elm (space, G8) %>%
    select.elm (media, "fr_FRA_lmonde_int") %>%
    biplot.obs (space, time)


## 5. FINDING OUTLIERS

## 5.1. Preparing data.cube
weeks.2014 <- weeks [substring (weeks, 1, 4) == "2014"]

dc2 <-
    dc %>%
    select.dim (space, time) %>%
    select.elm (time, weeks.2014, suppress=TRUE) %>%
    select.elm (space, G8)

dc2 %>%
    plot.obs (sep.dim = space, type="line")

dc2 %>%
    compute.model () %>%
    plot.obs (model, sep.dim = space, type="line")

dc2 %>%
    compute.model (space) %>%
    plot.obs (model, sep.dim = space, type="line")

dc2 %>%
    compute.model (space) %>%
    plot.obs (ratio, sep.dim = space)

dc2 %>%
    compute.model (space, time) %>%
    plot.obs (ratio, sep.dim = space)

dc2 %>%
    compute.model (space, time, deviation.type = 'poisson') %>%
    plot.obs (deviation, sep.dim = space)

dc2 %>%
    compute.model (space, time, deviation.type = 'poisson') %>%
    plot.outlier ()

dc %>%
    select.dim (space, time) %>%
    compute.model (space, time, deviation.type = 'poisson') %>%
    plot.outlier ()

dc %>%
    select.dim (space, time) %>%
    compute.model (space, time, deviation.type = 'poisson') %>%
    filter.obs (articles >= 100) %>%
    filter.obs (ratio >= 1) %>%
    plot.outlier ()

dc %>%
    select.dim (media, space) %>%
    compute.model (space, media, deviation.type = 'poisson') %>%
    filter.obs (articles >= 100) %>%
    filter.obs (ratio >= 1) %>%
    plot.outlier ()

dc %>%
    select.dim (media, time) %>%
    compute.model (time, media, deviation.type = 'poisson') %>%
    filter.obs (articles >= 100) %>%
    filter.obs (ratio >= 1) %>%
    plot.outlier ()

dc %>%
    select.dim (media, space) %>%
    compute.model (space, media, deviation.type = 'poisson') %>%
    filter.obs (outlier == 1) %>%
    arrange.obs (deviation) %>%
    as.data.frame ()

dc %>%
    select.dim (media, space) %>%
    compute.model (space, media, deviation.type = 'poisson') %>%
    filter.obs (outlier == 1) %>%
    arrange.obs (deviation) %>%
    as.data.frame ()

dc %>%
    select.dim (media, space) %>%
    select.elm (space, top.nb = 10) %>%
    select.elm (media, top.nb = 5) %>%
    compute.model (space, media, deviation.type = 'poisson') %>%
    biplot.obs (media, space, deviation)

elm.names (dc, time)

dc %>%
    select.dim (media, space) %>%
    select.elm (space, top.nb = 50) %>%
    select.elm (media, top.nb = 10) %>%
    compute.model (space, media, deviation.type = 'poisson') %>%
    biplot.obs (media, space)



## Temporal outliers

dc3 <-
    dc %>%
    select.elm (time, weeks.2014, suppress=TRUE) %>%
    select.elm (space, G8)


