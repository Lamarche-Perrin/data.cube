library (readr)
df <- read_csv ('data/articles.csv')
head (df)

source ('../../src/data.cube.R')
summary (geomedia)

geomedia <-
    df %>%
    as.data.cube (
        dim.names = list (media = id_media, week, country = id_country),
        var.names = list (articles = article_nb)
    )

summary (geomedia)
str (geomedia)

geomedia %>% as.data.frame

geomedia %>%
    select.dim (week) %>%
    as.data.frame

geomedia %>%
    select.dim (week) %>%
    arrange.elm (week, name) %>%
    as.data.frame

geomedia %>%
    select.dim (week) %>%
    arrange.elm (week, name) %>%
    plot.var (articles) +
    theme (axis.text.x = element_text (angle = 90, size = 6))

geomedia %>%
    select.dim (week) %>%
    arrange.elm (week, name) %>%
    plot.var (articles, type = "line") +
    theme (axis.text.x = element_text (angle = 90, size = 6))


## 2.4. Suppress elements in the data with 'remove.elm()'

geomedia %>%
    select.dim (week) %>%
    filter.elm (week, articles <= 2500) %>%
    as.data.frame

geomedia %>%
    select.dim (week) %>%
    filter.elm (week, articles > 2500) %>%
    arrange.elm (week, name) %>%
    plot.var (articles, type = "line") +
    theme (axis.text.x = element_text (angle = 90, size = 6))

geomedia <- # Remove week and save results in 'geomedia'
    geomedia %>%
    remove.elm (week, rem.week, suppress = TRUE)



## ## 3. EXPLORE SPACIAL DIMENSION

#### Second Example: The Spacial Dimension

geomedia %>%
    select.dim (country) %>%
    top_n.elm (country, articles, 20) %>%
    arrange.elm (country, desc (articles)) %>%
    plot.var (articles)

## 3.3. Select a subset of particular country with 'select.elm()'

G8 <- c ("USA", "JPN", "DEU", "FRA", "RUS", "GBR", "ITA", "CAN")
geomedia %>%
    select.dim (country) %>%
    filter.elm (country, name %in% G8) %>%
    arrange.elm (country, match (name, G8)) %>%
    plot.var (articles)



## 4. EXPLORE SPACIO-TEMPORAL DIMENSION

## 4.1. Prepare data.cube

source ('../../src/data.cube.R')

geomedia %>%
    select.dim (week, country) %>%
    arrange.elm (country, name) %>%
    arrange.elm (week, name) %>%
    as.data.frame

geomedia %>%
    select.dim (week, country) %>%
    arrange.elm (week, name) %>%
    filter.elm (country, name %in% c ("USA", "FRA", "DEU")) %>%
    plot.var (articles, sep.dim.names = country, type = "line") +
    theme (axis.text.x = element_text (angle = 90, size = 6))

geomedia %>%
    select.dim (week, country) %>%
    filter.elm (week, format (name, "%Y") == "2014") %>%
    arrange.elm (week, name) %>%
    top_n.elm (country, articles, 20) %>%
    arrange.elm (country, desc (articles)) %>%
    plot.var (articles, sep.dim.names = country, type = "point") +
    theme (axis.text.x = element_text (angle = 90, size = 6))

geomedia %>%
    filter.elm (week, format (name, "%Y") == "2014") %>%
    arrange.elm (week, name) %>%
    top_n.elm (country, articles, 8) %>%
    arrange.elm (country, desc (articles)) %>%
    filter.elm (media, name %in% c ("fr_FRA_lmonde_int", "en_GBR_guardi_int")) %>%
    plot.var (articles, sep.dim.names = list (country, media), type = "point") +
    theme (axis.text.x = element_text (angle = 90, size = 6))


## 5. FINDING OUTLIERS

## 5.1. Data preparation

source ('../../src/data.cube.R')

geomedia_sample <-
    geomedia %>%
    select.dim (week, country) %>%
    arrange.elm (week, name) %>%
    filter.elm (country, name %in% c ("USA", "RUS", "FRA", "ITA", "JPN")) %>%
    arrange.elm (country, desc (articles))


## 5.2. Compute a simple model (taking into account the global popularity of country)

                                        # Raw observations
geomedia_sample %>%
    plot.var (sep.dim.names = country, type = "line")

                                        # Raw model
geomedia_sample %>%
    compute.var.model (list (week, country), articles) %>%
    as.data.frame

    plot.var (model, sep.dim.names = country, type = "line")

                                        # Model taking into account 'country' marginals
geomedia_sample %>%
    compute.model (country) %>%
    plot.var (model, sep.dim.names = country, type = "line")

                                        # Ratio between observed values and expected values
geomedia_sample %>%
    compute.model (country) %>%
    plot.var (ratio, sep.dim.names = country)


## 5.3. Compute a more complete model (also taking into account the global activity through time)

geomedia_sample %>% select.dim (week) %>% plot.var ()

geomedia_sample %>%
    compute.model (country, week) %>%
    plot.var (ratio, sep.dim.names = country)


## 5.4. Compute significativity of 

geomedia_sample %>%
    compute.model (country, week, deviation.type = 'poisson') %>%
    plot.var (deviation, sep.dim.names = country)

geomedia_sample %>%
    compute.model (country, week, deviation.type = 'poisson') %>%
    plot.outlier ()

geomedia %>%
    select.dim (country, week) %>%
    compute.model (country, week, deviation.type = 'poisson') %>%
    plot.outlier ()

geomedia %>%
    select.dim (country, week) %>%
    compute.model (country, week, deviation.type = 'poisson') %>%
    filter.obs (articles >= 100) %>%
    filter.obs (ratio >= 1) %>%
    plot.outlier ()

geomedia %>%
    select.dim (newspapers, country) %>%
    compute.model (country, newspapers, deviation.type = 'poisson') %>%
    filter.obs (articles >= 100) %>%
    filter.obs (ratio >= 1) %>%
    plot.outlier ()

geomedia %>%
    select.dim (newspapers, week) %>%
    compute.model (week, newspapers, deviation.type = 'poisson') %>%
    filter.obs (articles >= 100) %>%
    filter.obs (ratio >= 1) %>%
    plot.outlier ()

geomedia %>%
    select.dim (newspapers, country) %>%
    compute.model (country, newspapers, deviation.type = 'poisson') %>%
    filter.obs (outlier == 1) %>%
    arrange.obs (deviation) %>%
    as.data.frame ()

geomedia %>%
    select.dim (newspapers, country) %>%
    compute.model (country, newspapers, deviation.type = 'poisson') %>%
    filter.obs (outlier == 1) %>%
    arrange.obs (deviation) %>%
    as.data.frame ()

geomedia %>%
    select.dim (newspapers, country) %>%
    select.elm (country, top.nb = 10) %>%
    select.elm (newspapers, top.nb = 5) %>%
    compute.model (country, newspapers, deviation.type = 'poisson') %>%
    biplot.var (newspapers, country, deviation)

elm.names (geomedia, week)

geomedia %>%
    select.dim (newspapers, country) %>%
    select.elm (country, top.nb = 50) %>%
    select.elm (newspapers, top.nb = 10) %>%
    compute.model (country, newspapers, deviation.type = 'poisson') %>%
    biplot.var (newspapers, country)



## Temporal outliers

geomedia_sample <-
    geomedia %>%
    select.elm (week, week.2014, suppress = TRUE) %>%
    select.elm (country, G8)

