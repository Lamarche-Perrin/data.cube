rm (list = ls())

## Test primitives
source ('../src/data.cube.R')

test.arg.names <- function (args) { arg.names (substitute (args)) }
test.arg.names (c (a, b))
test.arg.names (c (v = a, b))
test.arg.names (list (a, b))
test.arg.names (list (v = a, b))
test.arg.names (c (a))
test.arg.names (list (a))
test.arg.names (a)
test.arg.names (c ())
test.arg.names (list ())
test.arg.names (NULL)

test.dot.names <- function (...) { dot.names (enquos (...)) }
test.dot.names (a, b)
test.dot.names (v = a, b)
test.dot.names (a)
test.dot.names (v = a)
test.dot.names ()
test.dot.names (NULL)

## Import datasets

df.123 <- read.csv ('../test/example.dim1.dim2.dim3.csv', stringsAsFactors=FALSE)
df.12 <- read.csv ('../test/example.dim1.dim2.csv', stringsAsFactors=FALSE)
df.23 <- read.csv ('../test/example.dim2.dim3.csv', stringsAsFactors=FALSE)
df.1 <- read.csv ('../test/example.dim1.csv', stringsAsFactors=FALSE)
df.2 <- read.csv ('../test/example.dim2.csv', stringsAsFactors=FALSE)
df. <- read.csv ('../test/example.csv', stringsAsFactors=FALSE)

head (df.123)
head (df.12)
head (df.23)
head (df.1)
head (df.2)
head (df.)


## Create data.cube
source ('../src/data.cube.R')

dc <- df.123 %>% as.data.cube (c (dim3, dim1, dim2), c (var1, var2))
dc %>% str

df.123 %>% as.data.cube (c (dim3, dim1, dim2), c (v1 = var1, var2)) %>% str
df.123 %>% as.data.cube (c (a = dim3, b = dim1, c = dim2), c (var1, var2)) %>% str
df.123 %>% as.data.cube (c (c = dim3, dim1, dim2), c (var1, v2 = var2)) %>% str

df.123 %>% as.data.cube (c (dim3, dim1, dim2), var1) %>% str
df.123 %>% as.data.cube (c (dim3, dim1), c (var1, var2)) %>% str
df.123 %>% as.data.cube (c (dim3, dim1, dim2)) %>% str

df.1 %>% as.data.cube (dim1) %>% str
df. %>% as.data.cube () %>% str
df. %>% as.data.cube (var.names = var10) %>% str

dc.123 <- df.123 %>% as.data.cube (c (dim1, dim2, dim3))
dc.231 <- df.123 %>% as.data.cube (c (dim2, dim3, dim1))
dc.321 <- df.123 %>% as.data.cube (c (dim3, dim2, dim1))
dc.12 <- df.12 %>% as.data.cube (c (dim1, dim2))
dc.23 <- df.23 %>% as.data.cube (c (dim2, dim3))
dc.1 <- df.1 %>% as.data.cube (dim1)
dc.2 <- df.2 %>% as.data.cube (dim2)
dc. <- df. %>% as.data.cube ()

is.data.cube (dc.1)
is.data.cube (df.1)


## Join data.cubes
source ('../src/data.cube.R')

join (dc.123, dc.1) %>% str
join (dc.231, dc.1) %>% str

join (dc.1, dc.123) %>% str
join (dc.1, dc.231) %>% str
join (dc.1, dc.321) %>% str

join (dc., dc.1) %>% str
join (dc.1, dc.) %>% str

join (dc.2, dc.23) %>% str
join (dc.2, dc.1, dc.23, dc.123, dc.) %>% str

dc.123 %>% select.var (var1) %>% str
dc.123 %>% select.var (var2) %>% str
dc.123 %>% select.var (var1) %>% join (dc.123 %>% select.var (var2)) %>% str

dc <- join (dc.2, dc.1, dc.12, dc.23, dc.123, dc.)
str (dc)


## Reorder data.cube
source ('../src/data.cube.R')

dc %>% reorder.dim (dim3) %>% reorder.var (var6, var1) %>% str()
dc %>% reorder.dim (dim1, dim2, dim3) %>% reorder.var (var1, var2, var3, var4, var5, var6, var7, var8, var9, var10) %>% str


## Rename dimensions and variables
source ('../src/data.cube.R')

dc %>% rename.dim (c = dim3, b = dim2) %>% str
dc %>% rename.var (v2 = var2, v5 = var5, v4=var4) %>% str

dc %>% rename.dim (a = dim1, b = dim2, c = dim3) %>% str
dc %>% rename.var (v1 = var1, v2 = var2, v3 = var3, v4 = var4, v5 = var5, v6 = var6, v7 = var7, v8 = var8, v9 = var9, v10 = var10) %>% str

dc <- dc %>%
    reorder.dim (dim1, dim2, dim3) %>%
    reorder.var (var1, var2, var3, var4, var5, var6, var7, var8, var9, var10) %>%
    rename.dim (a = dim1, b = dim2, c = dim3) %>%
    rename.var (v1 = var1, v2 = var2, v3 = var3, v4 = var4, v5 = var5, v6 = var6, v7 = var7, v8 = var8, v9 = var9, v10 = var10)
str (dc)

dca <- dc %>% arrange.elm (b, name) %>% arrange.elm (c (b, c), desc (v4)) %>% arrange.elm (a, desc (name), v6)


## Basic properties
source ('../src/data.cube.R')

dim.names (dc)
dim.nb (dc)

var.names (dc)
var.nb (dc)


## Compute margins
source ('../src/data.cube.R')

dca %>% compute.var (c (b, a), v1) %>% str
dca %>% compute.var (c (b, a), v2, v1) %>% str
dca %>% compute.var (c (b, a), v2, var1 = v1) %>% str
dca %>% compute.var (c (b, a), v5, v9) %>% str
dca %>% compute.var (c (b, a), v5, v1) %>% str

dca %>% compute.var (c (c, a), v1) %>% str
dca %>% compute.var (c (c, a), v10) %>% str
dca %>% compute.var (c (c, a)) %>% str

dca %>% compute.var (c (), v2, v3) %>% str
dca %>% compute.var (c (a, b, c), v8, v4) %>% str
dca %>% compute.var (c (b, c), var1 = v1, var8 = v8, var10 = v10) %>% str


## Arrange elements 1
source ('../src/data.cube.R')

dca %>% str
dca %>% rename.dim (dim2 = b) %>% str
dca %>% rename.var (var4 = v4) %>% str
dca %>% select.dim (c, b) %>% str
dca %>% select.var (v6) %>% str
dca %>% select.var (v4) %>% str
dca %>% select.var (v10) %>% str

dc %>% as.data.frame
dc %>% arrange.elm (a, name) %>% as.data.frame
dc %>% arrange.elm (a, desc (name)) %>% as.data.frame
dc %>% arrange.elm (b, name) %>% arrange.elm (a, name) %>% as.data.frame

dc %>% select.dim (a) %>% arrange.elm (a, v6) %>% as.data.frame
dc %>% select.dim (a) %>% arrange.elm (a, v6, desc (v2)) %>% as.data.frame
dc %>% arrange.elm (a, v6, desc (v2)) %>% as.data.frame

dc %>% select.var (v1) %>% select.dim (a, b) %>% as.data.frame
dc %>% select.var (v1) %>% select.dim (a, b) %>% arrange.elm (c (a, b), v1) %>% as.data.frame
dc %>% select.var (v1) %>% arrange.elm (c (a, b), v1) %>% as.data.frame
dc %>% select.var (v1) %>% arrange.elm (c, name) %>% arrange.elm (c (a, b), v1) %>% as.data.frame
dc %>% select.var (v1) %>% arrange.elm (c (a, b), v1) %>% arrange.elm (c, name) %>% as.data.frame


## Arrange elements 2
source ('../src/data.cube.R')

dc %>% as.data.frame
dc %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% arrange.elm (a, name) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% arrange.elm (b, name) %>% arrange.elm (a, name) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% arrange.elm (a, name) %>% arrange.elm (b, name) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% arrange.elm (a, name) %>% arrange.elm (b, name) %>% reorder.dim (b, a) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)

dc %>% select.dim (a) %>% arrange.elm (a, v6) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% select.dim (a) %>% arrange.elm (a, v6, desc (v2)) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% arrange.elm (a, v6, desc (v2)) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)

dc %>% select.var (v1) %>% select.dim (a, b) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% select.var (v1) %>% select.dim (a, b) %>% arrange.elm (c (a, b), v1) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% select.var (v1) %>% arrange.elm (c (a, b), v1) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)
dc %>% select.var (v1) %>% arrange.elm (c, name) %>% arrange.elm (c (a, b), v1) %>% as.data.frame (complete = TRUE) %>% print (n = Inf)


## Select data.plane
source ('../src/data.cube.R')

dc %>% str
dc %>% select.dim (a) %>% str
dc %>% select.dim (b, a) %>% str
dc %>% select.dim (a, c) %>% str
dc %>% select.dim () %>% str

dc %>% select.dim (dim2 = b, dim1 = a) %>% str
dc %>% select.dim (dim2 = b, a) %>% str

dc %>% select.var (v1) %>% str
dc %>% select.var (v9, v1) %>% str
dc %>% select.var (v2, v1) %>% str
dc %>% select.var () %>% str

dc %>% select.var (var9 = v9, var1 = v1) %>% str
dc %>% select.var (v9, var1 = v1) %>% str


## Create new variable
source ('../src/data.cube.R')

x <- 5
dc %>% mutate.var (c (a, b, c), v11 = 1) %>% str
dc %>% mutate.var (c (a, b, c), v11 = x) %>% str
dc %>% mutate.var (c (a, b, c), v11 = v1 + v2) %>% str

dc %>% mutate.var (c (b, a), v11 = v1 + v2) %>% str
dc %>% mutate.var (c (b, a), v11 = v1 + v2, v12 = ifelse (v3 == 'A', v11, NA)) %>% str

dc %>% transmute.var (c (b, a), v11 = v1 + v2, v12 = ifelse (v3 == 'A', v11, NA)) %>% str


## Filter elements
source ('../src/data.cube.R')

dc %>% str
dc %>% filter.elm.indices (a, 3) %>% str
dc %>% filter.elm.indices (a, c (4, 3)) %>% str

dc %>% select.var (v3) %>% select.dim (a, b) %>% as.data.frame
dc %>% select.var (v3) %>% select.dim (b) %>% as.data.frame
dc %>% select.var (v3) %>% filter.elm.indices (a, c (4, 3)) %>% select.dim (b) %>% as.data.frame
dc %>% select.var (v3) %>% compute.var (b, v3) %>% filter.elm.indices (a, c (4, 3)) %>% select.dim (b) %>% as.data.frame

dc %>% select.var (v3) %>% compute.var (b, v3) %>% as.data.frame
dc %>% select.var (v3) %>% compute.var (c (a, b, c), v3) %>% as.data.frame
dc %>% select.var (v3) %>% complete.elm (a, b, c) %>% as.data.frame
dc %>% select.var (v3) %>% complete.elm (a, b, c) %>% compute.var (c (a, b, c), v3) %>% as.data.frame
dc %>% select.var (v3) %>% select.dim (a, b) %>% as.data.frame

x <- 10
dc %>% filter.elm (a, name == 'a1') %>% str
dc %>% filter.elm (a, name %in% c ('a1', 'a2')) %>% str
dc %>% filter.elm (a, v1 > 10) %>% str
dc %>% filter.elm (a, v1 > x) %>% str
dc %>% filter.elm (a, v1 > 7, v6 == 'A') %>% str

dc %>% filter.elm (c (a, b), v3 == 'A') %>% str
dc %>% filter.elm (c (a, b), v1 > 8) %>% str
dc %>% filter.elm (c (a, b), v1 > 100) %>% str


## Plot variables 1
source ('../src/data.cube.R')

dc %>% plot.var (v9)
dc %>% plot.var (v1)

dca <- dc %>% arrange.elm (c, name) %>% arrange.elm (b, name) %>% arrange.elm (a, name)
dca %>% plot.var (v1)
dca %>% arrange.elm (c (a, b, c), v1) %>% plot.var (v1)
dca %>% arrange.elm (c (a, b, c), desc (v1)) %>% plot.var (v1)

dc %>% select.dim (a) %>% plot.var (v1)
dca %>% select.dim (a) %>% plot.var (v1)
dca %>% arrange.elm (a, v1) %>% select.dim (a) %>% plot.var (v1)

dca %>% select.dim (a, b) %>% plot.var (v1, v2)
dca %>% select.dim (a, b) %>% arrange.elm (c (a, b), v2) %>% plot.var (v1, v2)
dca %>% select.dim (a, b) %>% arrange.elm (c (a, b), v1) %>% plot.var (v1, v2)
dca %>% select.dim (a, b) %>% arrange.elm (c (a, b), v1) %>% arrange.elm (c (a, b), v2) %>% plot.var (v1, v2)
dca %>% select.dim (a, b) %>% arrange.elm (c (a, b), v2, v1) %>% plot.var (v1, v2)

dca %>% select.var (v2, v1) %>% select.dim (a, b) %>% plot.var ()

dca %>% filter.elm (a, name == "a2") %>% plot.var (v1)
dca %>% filter.elm (a, name == "a2") %>% filter.elm (b, name == "b1") %>% plot.var (v1)

dca %>% arrange.elm (b, v1) %>% plot.var (v1, sep.dim.names = c (b, c))
dca %>% filter.elm (b, name == "b2") %>% plot.var (v1, sep.dim.names = c (b, c))


## Plot variables 2
source ('../src/data.cube.R')

dca %>% plot.var (v1, type = "bar")
dca %>% plot.var (v1, type = "line")
dca %>% plot.var (v1, type = "point")

dca %>% plot.var (v1, v2, type = "bar")
dca %>% plot.var (v1, v2, type = "line")
dca %>% plot.var (v1, v2, type = "point")

dca %>% plot.var (v1, sep.dim.names = b, type = "bar")
dca %>% plot.var (v1, sep.dim.names = b, type = "line")
dca %>% plot.var (v1, sep.dim.names = b, type = "point")

dca %>% plot.var (v1, v2, sep.dim.names = b, type = "bar")
dca %>% plot.var (v1, v2, sep.dim.names = b, type = "line")
dca %>% plot.var (v1, v2, sep.dim.names = b, type = "point")

dca %>% plot.var (v1, sep.dim.names = c (b, c), type = "bar")
dca %>% plot.var (v1, sep.dim.names = c (b, c), type = "line")
dca %>% plot.var (v1, sep.dim.names = c (b, c), type = "point")


## Compute expected values
source ('../src/data.cube.R')

dcb <- dca %>% select.var (v1) %>% select.dim (a, b)
dcb %>% compute.var.model (c (a, b), v1) %>% as.data.frame
dcb %>% compute.var.model (c (a, b), v1) %>% complete.elm (a, b) %>% as.data.frame
dcb %>% complete.elm (a, b) %>% compute.var.model (c (a, b), v1) %>% as.data.frame

dc %>% compute.var.model (c (a, b), v1) %>% str
dc %>% select.dim (a, b) %>% compute.var.model (c (a, b), v1) %>% arrange.elm (c (a, b), desc (v1.deviation)) %>% as.data.frame
dc %>% compute.var.model (c (a, b), v1) %>% select.dim (a, b) %>% arrange.elm (c (a, b), desc (v1.deviation)) %>% as.data.frame
dc %>% complete.elm (a, b) %>% compute.var.model (c (a, b), v1) %>% select.dim (a, b) %>% arrange.elm (c (a, b), desc (v1.deviation)) %>% as.data.frame

dc %>% compute.var.model (c (a, b), v1, b = v1) %>% str
dc %>% compute.var.model (c (a, b), v1, b = v1) %>% select.dim (a, b) %>% as.data.frame
dc %>%  select.dim (a, b) %>% compute.var.model (c (a, b), v1, b = v1) %>% as.data.frame

dc %>% compute.var.model (c (a, b), v1, a = v2, b = v1) %>% str
dc %>% compute.var.model (c (a, b), v1, a = v2, b = v1) %>% select.dim (a, b) %>% as.data.frame
dc %>% select.dim (a, b) %>% compute.var.model (c (a, b), v1, a = v2, b = v1) %>% as.data.frame




dcb <- dcb %>% complete.elm (a, b) %>% compute.var.model (c (a, b), v1, a = v1, b = v1)

source ('../src/data.cube.R')
dc %>% str
dcb %>% plot.var (v1, v1.model, type = "line")

dim.names <- c ("a", "b")
var.name <- "v1"

dcb <- dcb %>% mutate.var_(dim.names, paste0 (var.name, ".deviation = ", var.name, " / v1.model"))
dcb$var.NA[[paste0 (var.name, ".deviation")]] <- 1
dcb %>% plot.var (v1.deviation, type = "bar")

if (all (sapply (dcb$var.NA[c('v1','v1.deviation')], is.numeric))) {
    NA.values <- unique (unlist (dcb$var.NA[c('v1','v1.deviation')]))
    if (length (NA.values) == 1) {
    }
}

identical
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
  as.data.frame

dc %>%
  select.dim (newspapers, countries) %>%
  compute.model (countries, newspapers, deviation.type = 'poisson') %>%
  filter.obs (outlier == 1) %>%
  arrange.obs (deviation) %>%
  as.data.frame

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
