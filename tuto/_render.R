library (rmarkdown)
render ("data.cube.tuto.Rmd")
##render ("test.render_toc.Rmd")

library (bookdown)
bookdown::render_book ("_bookdown.yml", output_format = "bookdown::gitbook")
