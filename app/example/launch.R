#### RUN Shiny ####
library(markdown)
rmarkdown::run(file = "interface.Rmd", shiny_args = list(port = 8888))
# display.mode = "showcase #add to shiny_args to visualize code while running


