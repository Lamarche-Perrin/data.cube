rm(list=ls())

library ('testthat')
source ('../data.cube.1.1.R')

test.data.cube <- function (dc, file) {
    dim.names <- dc$dim.names
    
    out.df <- as.data.frame (dc)
    for (dim in rev(dim.names)) { out.df <- out.df[order(out.df[[dim]]),] }
    rownames (out.df) <- NULL

    exp.df <- read.csv (file, stringsAsFactors=FALSE)
    for (dim in rev(dim.names)) { exp.df <- exp.df[order(exp.df[[dim]]),] }
    rownames (exp.df) <- NULL

    expect_that (out.df, equals (exp.df))
}

results <- test_dir ('./', reporter='summary')
