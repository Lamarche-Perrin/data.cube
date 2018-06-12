rm (list = ls())

source ('../src/data.cube.R')

## Import dataset
df.123 <- read.csv ('../unit.test/example.dim1.dim2.dim3.csv', stringsAsFactors=FALSE)
df.12 <- read.csv ('../unit.test/example.dim1.dim2.csv', stringsAsFactors=FALSE)
## df.23 <- read.csv ('../unit.test/example.dim2.dim3.csv', stringsAsFactors=FALSE)
df.1 <- read.csv ('../unit.test/example.dim1.csv', stringsAsFactors=FALSE)
## df.2 <- read.csv ('../unit.test/example.dim2.csv', stringsAsFactors=FALSE)

head (df.123)
head (df.12)
## head (df.23)
head (df.1)
## head (df.2)



## Create data.cube
str (as.data.cube (
    df.123,
    list (c=dim3, a=dim1, b=dim2),
    list (v1=var1, v2=var2)
))

dc <- merge (
    as.data.cube (
        df.123,
        list (c=dim3, a=dim1, b=dim2),
        list (v1=var1, v2=var2)
    ),
    as.data.cube (
        df.12,
        list (b=dim2, a=dim1),
        list (v3=var3)
    ),
    as.data.cube (
        df.1,
        list (a=dim1),
        list (v6=var6, v7=var7)
    )
)

str(dc)

is.data.cube (dc)
as.data.frame (dc)
as.data.frame (dc, complete = TRUE)
as.data.frame (dc, c, b, a)


## Basic properties

dim.names (dc)
dim.nb (dc)

elm.names (dc)
elm.nb (dc)

elm.names (dc, c, b)
elm.nb (dc, c, b)

var.names (dc)
var.nb (dc)

var.names (dc, a, b)
var.nb (dc, a, b)


## Compute margins

as.data.frame (compute.margin (dc, v1, b, a), b, a)
as.data.frame (compute.margin (dc, v1, a), a)
as.data.frame (compute.margin (dc, v3, a), a)


        
dc %>% as.data.frame (dim.names = a, )

dc %>%
  select.dim (weeks) %>%
  plot.obs ()

p <- # Get plot and save it in variable 'p'
  dc %>%
  select.dim (weeks) %>%
  plot.obs ()

p + ggtitle ("Number of articles through time") # Add a title

ggsave ("myplot.pdf", p) # Save plot as PDF


## 2.2. Order temporal dimension with 'arrange.elm()'

dc %>%
  select.dim (weeks) %>%
  arrange.elm (weeks) %>%
  plot.obs ()

dc <- # Order temporal dimension and save result in 'dc'
  dc %>% arrange.elm (weeks)


## 2.3. Plot variable along the temporal dimension with 'plot.obs()'

dc %>%
  select.dim (weeks) %>%
  plot.obs ()

dc %>%
  select.dim (weeks) %>%
  plot.obs (type = "line")


## 2.4. Suppress elements in the data with 'remove.elm()'

rem.weeks <- # Vector of weeks to remove
  c ("2013-12-30",
     "2014-06-16",
     "2014-09-08",
     "2015-01-19",
     "2015-06-29")

dc %>%
  select.dim (weeks) %>%
  remove.elm (weeks, rem.weeks, suppress = TRUE) %>%
  plot.obs (type = "line")

dc <- # Remove weeks and save results in 'dc'
  dc %>%
  remove.elm (weeks, rem.weeks, suppress = TRUE)



## 3. EXPLORE SPACIAL DIMENSION

## 3.1. Order by countries' name with 'arrange.elm()'

dc %>%
  select.dim (countries) %>%
  arrange.elm (countries) %>%
  plot.obs ()


## 3.2. Order by values with 'arrange.obs()'

dc %>%
  select.dim (countries) %>%
  arrange.elm (countries, var = articles, decreasing = TRUE) %>%
  plot.obs ()

dc <-
  dc %>%
  arrange.elm (countries, var = articles, decreasing = TRUE)


## 3.3. Select a subset of particular countries with 'select.elm()'

G8 <- c ("USA", "JPN", "DEU", "FRA", "RUS", "GBR", "ITA", "CAN")

dc %>%
  select.dim (countries) %>%
  select.elm (countries, elm.array = G8) %>%
  plot.obs ()


## 3.4. Select the top 20 countries with 'select.elm()'

dc %>%
  select.dim (countries) %>%
  select.elm (countries, top.nb = 20, var = articles) %>%
  plot.obs ()


## 3.5. Select countries with more than 5000 articles with 'filter.elm()'

dc %>%
  select.dim (countries) %>%
  select.elm (countries, filter = articles > 5000) %>%
  plot.obs ()



## 4. EXPLORE SPACIO-TEMPORAL DIMENSION

## 4.1. Prepare data.cube

weeks <- elm.names (dc, weeks)
weeks.2014 <- weeks [substring (weeks, 1, 4) == "2014"] # Select all weeks starting by '2014'
weeks.2014

dc2 <-
  dc %>%
  select.dim (weeks, countries) %>%
  select.elm (weeks, weeks.2014) %>%
  select.elm (countries, G8) %>%
  arrange.elm (weeks, countries)
  
  
## 4.2. Plot countries and weeks

dc2 %>% plot.obs ()


## 4.3. Separate the 'countries' dimension

dc2 %>% plot.obs (sep.dim = countries)

dc2 %>% plot.obs (sep.dim = countries, type = "line")


## 4.4. Bidimensional plot of countries and weeks

dc2 %>% biplot.obs (x.dim = countries, y.dim = weeks)



## 5. FINDING OUTLIERS

## 5.1. Data preparation

dc3 <-
  dc %>%
  select.dim (weeks, countries) %>%
  select.elm (countries, c("USA","RUS","FRA","ITA","JPN")) %>%
  arrange.elm (weeks, countries)


## 5.2. Compute a simple model (taking into account the global popularity of countries)

# Raw observations
dc3 %>% plot.obs (sep.dim = countries, type = "line")

# Raw model
dc3 %>%
  compute.model () %>%
  plot.obs (model, sep.dim = countries, type = "line")

# Model taking into account 'countries' marginals
dc3 %>%
  compute.model (countries) %>%
  plot.obs (model, sep.dim = countries, type = "line")

# Ratio between observed values and expected values
dc3 %>%
  compute.model (countries) %>%
  plot.obs (ratio, sep.dim = countries)


## 5.3. Compute a more complete model (also taking into account the global activity through time)

dc3 %>% select.dim (weeks) %>% plot.obs ()

dc3 %>%
  compute.model (countries, weeks) %>%
  plot.obs (ratio, sep.dim = countries)


## 5.4. Compute significativity of 

dc3 %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  plot.obs (deviation, sep.dim = countries)

dc3 %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  plot.outlier ()

dc %>%
  select.dim (countries, weeks) %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  plot.outlier ()

dc %>%
  select.dim (countries, weeks) %>%
  compute.model (countries, weeks, deviation.type = 'poisson') %>%
  filter.obs (articles >= 100) %>%
  filter.obs (ratio >= 1) %>%
  plot.outlier ()

dc %>%
  select.dim (newspapers, countries) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  filter.obs (articles >= 100) %>%
  filter.obs (ratio >= 1) %>%
  plot.outlier ()

dc %>%
  select.dim (newspapers, weeks) %>%
  compute.model (weeks, newspapers, deviation.type = 'poisson') %>%
  filter.obs (articles >= 100) %>%
  filter.obs (ratio >= 1) %>%
  plot.outlier ()

dc %>%
  select.dim (newspapers, countries) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  filter.obs (outlier == 1) %>%
  arrange.obs (deviation) %>%
  as.data.frame ()

dc %>%
  select.dim (newspapers, countries) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  filter.obs (outlier == 1) %>%
  arrange.obs (deviation) %>%
  as.data.frame ()

dc %>%
  select.dim (newspapers, countries) %>%
  select.elm (countries, top.nb = 10) %>%
  select.elm (newspapers, top.nb = 5) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  biplot.obs (newspapers, countries, deviation)

elm.names (dc, weeks)

dc %>%
  select.dim (newspapers, countries) %>%
  select.elm (countries, top.nb = 50) %>%
  select.elm (newspapers, top.nb = 10) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  biplot.obs (newspapers, countries)



## Temporal outliers

dc3 <-
  dc %>%
  select.elm (weeks, weeks.2014, suppress = TRUE) %>%
  select.elm (countries, G8)
