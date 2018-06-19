install.packages ('tidyverse')
install.packages ('ggrepel')

source ('../src/data.cube.R')


## 1. DATA PREPARATION

## 1.1. Import dataset with 'read.csv()'

df <- read.csv ('../data/geomedia.tutorial.csv', stringsAsFactors = FALSE)
head (df)


## 1.2. Create datacube with 'as.data.cube()'

dc <- as.data.cube (
  df,
  dim = list (
    newspapers = id_media,
    countries = id_country,
    weeks = week
  ),
  var = list (articles = article_nb)
)


## 1.3. Get datacube's properties with 'summary()', 'dim.nb()', 'dim.names()', 'elm.nb()', and 'elm.names()'

dim.nb (dc) # Number of dimensions
dim.names (dc) # Names of dimensions

elm.nb (dc) # Number of elements for each dimension
elm.names (dc) # Names of elements for each dimension

elm.nb (dc, countries) # Number of elements for the 'countries' dimension
elm.names (dc, countries) # Names of elements for the 'countries' dimension

summary (dc)


## 1.4. See data structure with 'str()'

str (dc)


## 1.5. Transform datacube back into dataframe with 'as.data.frame()'

head (as.data.frame (dc), 20)
dc %>% as.data.frame () %>% head (20) # Same as previous line with pipes '%>%'



## 2. EXPLORE TEMPORAL DIMENSION

## 2.1. Select temporal dimension with 'select.dim()'

dc %>%
  select.dim (weeks) %>%
  as.data.frame ()

dc %>%
  select.dim (weeks) %>%
  plot.var ()


## 2.2. Order temporal dimension with 'arrange.elm()'

dc %>%
  select.dim (weeks) %>%
  arrange.elm (weeks) %>%
  plot.var ()

dc <- # Order temporal dimension and save result in 'dc'
  dc %>% arrange.elm (weeks)


## 2.3. Plot variable along the temporal dimension with 'plot.var()'

dc %>%
  select.dim (weeks) %>%
  plot.var ()

dc %>%
  select.dim (weeks) %>%
  plot.var (type = 'line')

p <- # Get plot and save it in variable 'p'
  dc %>%
  select.dim (weeks) %>%
  plot.var (type = 'line')

p + ggtitle ('Number of articles through time') # Add a title

ggsave ('myplot.pdf', p) # Save plot as PDF


## 2.4. Suppress elements in the data with 'remove.elm()'

rem.weeks <- # Vector of weeks to remove
  c ('2013-12-30',
     '2014-06-16',
     '2014-09-08',
     '2015-01-19',
     '2015-06-29')

dc %>%
  select.dim (weeks) %>%
  remove.elm (weeks, rem.weeks, suppress = TRUE) %>%
  plot.var (type = 'line')

dc %>%
  select.dim (weeks) %>%
  remove.elm (weeks, rem.weeks, suppress = TRUE) %>%
  plot.var (type = 'line') + expand_limits (y = 0)

dc <- # Remove weeks and save results in 'dc'
  dc %>%
  remove.elm (weeks, rem.weeks, suppress = TRUE)



## 3. EXPLORE SPACIAL DIMENSION

## 3.1. Order by countries' name with 'arrange.elm()'

dc %>%
  select.dim (countries) %>%
  arrange.elm (countries) %>%
  plot.var ()


## 3.2. Order by values with 'arrange.obs()'

dc %>%
  select.dim (countries) %>%
  arrange.elm (countries, var = articles, decreasing = TRUE) %>%
  plot.var ()

dc <-
  dc %>%
  arrange.elm (countries, var = articles, decreasing = TRUE)


## 3.3. Select a subset of particular countries with 'select.elm()'

G8 <- c ('USA', 'JPN', 'DEU', 'FRA', 'RUS', 'GBR', 'ITA', 'CAN')

dc %>%
  select.dim (countries) %>%
  select.elm (countries, elm.array = G8) %>%
  plot.var ()


## 3.4. Select the top 20 countries with 'select.elm()'

dc %>%
  select.dim (countries) %>%
  select.elm (countries, top.nb = 20, var = articles) %>%
  plot.var ()


## 3.5. Select countries with more than 5000 articles with 'filter.elm()'

dc %>%
  select.dim (countries) %>%
  select.elm (countries, filter = articles > 5000) %>%
  plot.var ()



## 4. EXPLORE SPACIO-TEMPORAL DIMENSION

## 4.1. Prepare data.cube

dc2 <-
  dc %>%
  select.dim (weeks, countries) %>%
  select.elm (countries, G8) %>%
  arrange.elm (weeks, countries)
  
  
## 4.2. Plot countries and weeks

dc2 %>% plot.var ()


## 4.3. Separate the 'countries' dimension

dc2 %>% plot.var (sep.dim = countries)

dc2 %>% plot.var (sep.dim = countries, type = 'line')


## 4.4. Bidimensional plot of countries and weeks

dc2 %>% biplot.var (x.dim = countries, y.dim = weeks)



## 5. FINDING OUTLIERS

## 5.1. Data preparation

dc3 <-
  dc %>%
  select.dim (weeks, countries) %>%
  select.elm (countries, c('USA','RUS','DEU','ITA','JPN')) %>%
  arrange.elm (weeks, countries)


## 5.2. Compute a simple model (taking into account the global popularity of countries)

# Raw observations
dc3 %>% plot.var (sep.dim = countries, type = 'line')


# Raw model
dc3 %>%
  compute.model () %>%
  plot.var (model, sep.dim = countries, type = 'line')

# Model taking into account 'countries' marginals
dc3 %>%
  compute.model (countries) %>%
  plot.var (model, sep.dim = countries, type = 'line')

# Ratio between observed values and expected values
dc3 %>%
  compute.model (countries) %>%
  plot.var (ratio, sep.dim = countries)


## 5.3. Compute a more complete model (also taking into account the global activity through time)

dc3 %>% select.dim (weeks) %>% plot.var ()

dc3 %>%
  compute.model (countries, weeks) %>%
  plot.var (model, sep.dim = countries, type = 'line')

dc3 %>%
  compute.model (countries, weeks) %>%
  plot.var (ratio, sep.dim = countries)


## 5.4. Compute model's significativity

dc3 %>%
  compute.model (countries, weeks, deviation.type = 'KLdiv') %>%
  plot.var (deviation, sep.dim = countries)


## 5.5. Plot outliers (countries x weeks)

dc3 %>%
  compute.model (countries, weeks, deviation.type = 'poisson', deviation.threshold = 1) %>%
  plot.outliers ()

# For the whole dataset
dc %>%
  select.dim (countries, weeks) %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  plot.outliers ()

# Filtering and zooming on positive outliers
dc %>%
  select.dim (countries, weeks) %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  filter.obs (articles >= 100) %>%
  filter.obs (ratio >= 1) %>%
  plot.outliers ()

# Another visualisation
dc %>%
  select.dim (countries, weeks) %>%
  select.elm (countries, top.nb = 15, var = articles) %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  biplot.var (countries, weeks, var = deviation)


## 5.6. Plot outliers (countries x newspapers)

dc %>%
  select.dim (newspapers, countries) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  plot.outliers ()


## 5.8. Retrieve list of outliers

dc %>%
  select.dim (newspapers, countries) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  filter.obs (outlier == 1) %>%
  arrange.obs (deviation, decreasing=TRUE) %>%
  as.data.frame ()


