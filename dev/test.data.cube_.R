rm (list = ls())

source ('../src/data.cube.R')
##browseVignettes(package = "dplyr")


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

df.123 %>% as.data.cube_(c ("dim3", "dim1", "dim2"), c ("var1", "var2")) %>% str ()
df.123 %>% as.data.cube_(c ("dim3", "dim1", "dim2"), c (v1 = "var1", "var2")) %>% str ()
df.123 %>% as.data.cube_(c (a = "dim3", b = "dim1", c = "dim2"), c ("var1", "var2")) %>% str ()

df.123 %>% as.data.cube_(c ("dim3", "dim1", "dim2"), c ("var1", "var2")) %>% str ()
df.123 %>% as.data.cube_(c ("dim3", "dim1", "dim2"), "var1") %>% str ()
df.123 %>% as.data.cube_(c ("dim3", "dim1"), c ("var1", "var2")) %>% str ()
df.123 %>% as.data.cube_(c ("dim3", "dim1", "dim2")) %>% str ()

df.123 %>% as.data.cube_(c (c = "dim3", "dim1", "dim2"), c ("var1", v2 = "var2")) %>% str ()
df.123 %>% as.data.cube (c (c = dim3, dim1, dim2), c (var1, v2 = var2)) %>% str ()

df.1 %>% as.data.cube_("dim1") %>% str ()
df. %>% as.data.cube_() %>% str ()
df. %>% as.data.cube_(var.names = "var10") %>% str ()

dc.123 <- df.123 %>% as.data.cube_(c ("dim1", "dim2", "dim3"))
dc.231 <- df.123 %>% as.data.cube_(c ("dim2", "dim3", "dim1"))
dc.321 <- df.123 %>% as.data.cube_(c ("dim3", "dim2", "dim1"))
dc.12 <- df.12 %>% as.data.cube_(c ("dim1", "dim2"))
dc.23 <- df.23 %>% as.data.cube_(c ("dim2", "dim3"))
dc.1 <- df.1 %>% as.data.cube_("dim1")
dc.2 <- df.2 %>% as.data.cube_("dim2")
dc. <- df. %>% as.data.cube_()

is.data.cube (dc.1)
is.data.cube (df.1)


## Join data.cubes

join (dc.123, dc.1) %>% str ()
join (dc.231, dc.1) %>% str ()
join (dc.1, dc.123) %>% str ()
join (dc.1, dc.231) %>% str ()
join (dc.1, dc.321) %>% str ()

join (dc., dc.1) %>% str ()
join (dc.1, dc.) %>% str ()

join (dc.2, dc.23) %>% str ()
join (dc.2, dc.1, dc.23, dc.123, dc.) %>% str ()

dc.123 %>% select.var_("var1") %>% str ()
dc.123 %>% select.var_("var2") %>% str ()
dc.123 %>% select.var_("var1") %>% join (dc.123 %>% select.var_("var2")) %>% str ()

dc <- join (dc.2, dc.1, dc.12, dc.23, dc.123, dc.)
str (dc)


## Reorder data.cube

dc %>% reorder.dim_("dim3") %>% reorder.var_("var6") %>% str()
dc %>% reorder.dim_(dc$dim.names [order (dc$dim.names)]) %>% reorder.var_(dc$var.names [order (dc$var.names)]) %>% str ()


## Rename dimensions and variables

dc %>% rename.dim_(c (c = "dim3", b = "dim2")) %>% str ()
dc %>% rename.var_(c (v2 = "var2", v5 = "var5")) %>% str ()

dc %>% rename.dim_(c (a = "dim1", b = "dim2", c = "dim3")) %>% str ()
dc %>% rename.var_(paste0 ("var", 1:10) %>% setNames (paste0 ("v", 1:10))) %>% str ()

dc <- dc %>%
    reorder.dim_(dc$dim.names [order (dc$dim.names)]) %>%
    reorder.var_(dc$var.names [order (dc$var.names)]) %>%
    rename.dim_(c (a = "dim1", b = "dim2", c = "dim3")) %>%
    rename.var_(paste0 ("var", 1:10) %>% setNames (paste0 ("v", 1:10)))
str (dc)


## Convert to data.frame

as.data.frame (dc)
as.data.frame (dc, stringsAsFactors = TRUE)
as.data.frame (dc, complete = TRUE)


## Basic properties

dim.names (dc)
dim.nb (dc)

var.names (dc)
var.nb (dc)


## Compute margins

dc %>% compute.var_(c ("b", "a"), "v1") %>% str ()
dc %>% compute.var_(c ("b", "a"), c ("v2", "v1")) %>% str ()
dc %>% compute.var_(c ("b", "a"), c ("v2", var1 = "v1")) %>% str ()
dc %>% compute.var_(c ("b", "a")) %>% str ()

dc %>% compute.var_(c ("c", "a")) %>% str ()
dc %>% compute.var_(c (), c ("v2", "v3")) %>% str ()
dc %>% compute.var_() %>% str ()


## Select data.plane

dc %>% str ()
dc %>% select.dim_("a") %>% str ()
dc %>% select.dim_(c ("b", "a")) %>% str ()
dc %>% select.dim_(c ("a", "c")) %>% str ()
dc %>% select.dim_() %>% str ()

dc %>% select.dim_(c (dim2 = "b", dim1 = "a")) %>% str ()
dc %>% select.dim_(c (dim2 = "b", "a")) %>% str ()

dc %>% select.var_("v1") %>% str ()
dc %>% select.var_(c ("v9", "v1")) %>% str ()
dc %>% select.var_(c ("v2", "v1")) %>% str ()
dc %>% select.var_() %>% str ()

dc %>% select.var_(c (var9 = "v9", var1 = "v1")) %>% str ()
dc %>% select.var_(c ("v9", var1 = "v1")) %>% str ()


## Create new variable
x <- 5
dc %>% mutate.var_(c ("a", "b", "c"), "v11 = 1") %>% str ()
dc %>% mutate.var_(c ("a", "b", "c"), "v11 = x") %>% str ()
dc %>% mutate.var_(c ("a", "b", "c"), "v11 = v1 + v2") %>% str ()

dc %>% mutate.var_(c ("b", "a"), "v11 = v1 + v2") %>% str ()
dc %>% mutate.var_(c ("b", "a"), c ("v11 = v1 + v2", "v12 = ifelse (v3 == 'A', v11, NA)")) %>% str ()

dc %>% transmute.var_(c ("b", "a"), c ("v11 = v1 + v2", "v12 = ifelse (v3 == 'A', v11, NA)")) %>% str ()


## Filter elements

dc %>% str ()
dc %>% filter.elm.indices_("a", 3) %>% str ()
dc %>% filter.elm.indices_("a", c (4, 3)) %>% str ()

dc %>% select.var_("v3") %>% select.dim_(c ("a", "b")) %>% as.data.frame ()
dc %>% select.var_("v3") %>% filter.elm.indices_("a", c (4, 3)) %>% select.dim_("b") %>% as.data.frame ()
dc %>% select.var_("v3") %>% compute.var_("b", "v3") %>% filter.elm.indices_("a", c (4, 3)) %>% select.dim_("b") %>% as.data.frame ()


x <- 10
dc %>% filter.elm_("a", "name == 'a1'") %>% str ()
dc %>% filter.elm_("a", "name %in% c ('a1', 'a2')") %>% str ()
dc %>% filter.elm_("a", "v1 > 10") %>% str ()
dc %>% filter.elm_("a", "v1 > x") %>% str ()
dc %>% filter.elm_("a", c ("v1 > 7", "v6 == 'A'")) %>% str ()

dc %>% filter.elm_(c ("a", "b"), "v3 == 'A'") %>% str ()
dc %>% filter.elm_(c ("a", "b"), "v1 > 8") %>% str ()
dc %>% filter.elm_(c ("a", "b"), "v1 > 100") %>% str ()


## Arrange elements

dc %>% str ()
dc %>% select.dim_("a") %>% as.data.frame ()
dc %>% arrange.elm_("a", "name") %>% select.dim_("a") %>% as.data.frame ()
dc %>% arrange.elm_("a", "v1") %>% select.dim_("a") %>% as.data.frame ()
dc %>% arrange.elm_("a", "v6") %>% select.dim_("a") %>% as.data.frame ()
dc %>% arrange.elm_("a", c ("v6", "v2")) %>% select.dim_("a") %>% as.data.frame ()

dc %>% select.dim_(c ("a", "b")) %>% arrange.elm_("a", "name") %>% as.data.frame ()
dc %>% select.dim_(c ("a", "b")) %>% arrange.elm_("b", "name") %>% arrange.elm_("a", "name") %>% as.data.frame ()

dc %>% as.data.frame ()
dc %>% select.dim_(c ("a", "b")) %>% arrange.elm_(c ("a", "b"), "v1") %>% as.data.frame ()
dc %>% arrange.elm_(c ("a", "b"), "v1") %>% as.data.frame ()





