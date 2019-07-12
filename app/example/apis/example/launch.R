rm (list = ls ())
library ('plumber')

RtweeetAPI = plumber::plumb (file = "APIexample.R")
RtweeetAPI$run (port = 5685, host = "127.0.0.1", swagger = TRUE)
