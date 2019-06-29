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

df.123 <- read_csv ('../test/example.dim1.dim2.dim3.csv')
df.12 <- read_csv ('../test/example.dim1.dim2.csv')
df.23 <- read_csv ('../test/example.dim2.dim3.csv')
df.1 <- read_csv ('../test/example.dim1.csv')
df.2 <- read_csv ('../test/example.dim2.csv')
df. <- read_csv ('../test/example.csv')

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
df.123 %>% as.data.cube (c (dim3, dim1), c (var1, var2)) %>% str # ERROR
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
dc %>% as.data.frame %>% select (dim1, dim2, dim3, var1, var2) %>% arrange (dim1, dim2, dim3)
str (dc)


## Summary of data.cube
source ('../src/data.cube.R')
dc %>% summary


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
dc %>% select.dim (a) %>% arrange.elm (a, desc (v6), desc (v2)) %>% as.data.frame
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

dc %>% select.dim (-dim1) %>% str
dc %>% select.dim (contains ("1")) %>% str

dc %>% select.var (v1) %>% str
dc %>% select.var (v9, v1) %>% str
dc %>% select.var (v2, v1) %>% str
dc %>% select.var () %>% str

dc %>% select.var (var9 = v9, var1 = v1) %>% str
dc %>% select.var (v9, var1 = v1) %>% str

dc %>% select.var (-var1, -var10) %>% str
dc %>% select.var (contains ("1")) %>% str
dc %>% select.var (contains ("1"), everything ()) %>% str


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

dc %>% select.var (v3) %>% compute.var (b, v3) %>% as.data.frame # EMPTY
dc %>% select.var (v3) %>% compute.var (c (a, b, c), v3) %>% as.data.frame # EMPTY
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

str(dc)
dc %>% plot.var (v9) # NULL
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

dca %>% plot.var (v1, v2, sep.dim.names = b, type = "bar") # ERROR
dca %>% plot.var (v1, v2, sep.dim.names = b, type = "line") # ERROR
dca %>% plot.var (v1, v2, sep.dim.names = b, type = "point") # ERROR

dca %>% plot.var (v1, sep.dim.names = c (b, c), type = "bar")
dca %>% plot.var (v1, sep.dim.names = c (b, c), type = "line")
dca %>% plot.var (v1, sep.dim.names = c (b, c), type = "point")


## Compute expected values
source ('../src/data.cube.R')

dc <- dc %>% arrange.elm (c, name) %>% arrange.elm (b, name) %>% arrange.elm (a, name)
dc %>% compute.var.model (v1 (a * b * c) ~ v1 (a * b / b) * v2 (c)) %>% as.data.frame

dc %>% compute.var.model (v1 (a * b * c) ~ v1 (a) * v1 (b) * v1 (c), deviation.type = "ratio") %>% as.data.frame
dc %>% compute.var.model (v1 (a * b * c) ~ v1 (a) * v1 (b) * v1 (c), deviation.type = "poisson") %>% as.data.frame
dc %>% compute.var.model (v1 (a * b * c) ~ v1 (a) * v1 (b) * v1 (c), deviation.type = "KLdiv") %>% as.data.frame
dc %>% compute.var.model (v1 (a * b * c) ~ v1 (a) * v1 (b) * v1 (c), deviation.type = "chi2") %>% as.data.frame
