devtools::create ("data.cube")

file.copy (from = "src/data.cube.R", to = "data.cube/R/data.cube.R", overwrite = TRUE)
file.copy (from = "DESCRIPTION", to = "data.cube/DESCRIPTION", overwrite = TRUE)
file.copy (from = "LICENSE", to = "data.cube/LICENSE", overwrite = TRUE)

setwd ("./data.cube")
getwd ()

## Load data
## tripartite_example <- read.table ("data/tripartite_example.csv", stringsAsFactors=FALSE)
## save (tripartite_example, file="data/tripartite_example.RData")

## devtools::use_vignette("introduction")

devtools::document ()
devtools::spell_check ()
devtools::check (manual=TRUE)
devtools::build (manual=TRUE)
devtools::build_vignettes ()
devtools::build_win ()
devtools::install ()
devtools::use_cran_comments ()
devtools::release ()



## Build documentation
## trash doc/data.cube.pdf
## R CMD Rd2pdf --title="data.cube" -o doc/data.cube.pdf data.cube/man/*.Rd


setwd ("..")
getwd ()
