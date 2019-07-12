library (markdown)
rmarkdown::run (
               file = "outlier-explorer.Rmd",
               shiny_args = list (port = 8888) # display.mode = "showcase"
           )


