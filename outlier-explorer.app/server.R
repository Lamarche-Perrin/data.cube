## This file is part of data.cube.
##
## data.cube is an R package for the easy processing of multidimentional
## data. It has been developed by researchers of the Complex Networks team,
## within the Computer Science Laboratory of Paris 6 (LIP6), for the
## ODYCCEUS project, founded by the European Commission FETPROACT 2016-2017
## program under grant 732942.
## 
## Copyright © 2017 Robin Lamarche-Perrin (<Robin.Lamarche-Perrin@lip6.fr>)
## 
## data.cube is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at your
## option) any later version.
## 
## data.cube is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
## more details.
## 
## You should have received a copy of the GNU General Public License along
## with this program. If not, see <http://www.gnu.org/licenses/>.

library ('shiny')
library ('rjson')
library ('rgl')

source ('../src/data.cube.R')

options (rgl.useNULL = TRUE)

app.title <- 'The Outlier Explorer'
time.names <- c ('timestep', 'time', 'week')

function (input, output, session) {

    output$app.title <- renderUI ({ app.title })

    dim.str <- function (dim.name, plural = FALSE) {
        if (dim.name == 'user') {
            str <- 'user'
            if (plural) { str <- paste (str, 's', sep='') }
        } else if (dim.name == 'agent') {
            str <- 'agent'
            if (plural) { str <- paste (str, 's', sep='') }
        } else if (dim.name == 'topic') {
            str <- 'topic'
           if (plural) { str <- paste (str, 's', sep='') } 
        } else if (dim.name == 'hashtag') {
            str <- 'hashtag'
           if (plural) { str <- paste (str, 's', sep='') }
        } else if (dim.name == 'timestep') {
            str <- 'timestep'
            if (plural) { str <- paste (str, 's', sep='') }
        } else if (dim.name == 'week') {
            str <- 'week'
            if (plural) { str <- paste (str, 's', sep='') }
        }  else if (dim.name == 'media') {
            str <- 'newspaper'
            if (plural) { str <- paste (str, 's', sep='') }
        } else if (dim.name == 'country') {
            str <- 'country'
            if (plural) { str <- "countries" }
        } else {
            str <- dim.name
        }
        return (str)
    }
    
    dc <-  reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        
        output$app.title <- renderUI ({ paste (app.title, '-', input$dataset) })

        file <- paste ('../data/', input$dataset, '.csv', sep='')
        cat (file = stderr(), file, "\n")
    
        ## file <- 'data/geomedia.csv'
        df <- read.csv (file, stringsAsFactors=FALSE)
        dc <- as.data.cube_(
            df,
            dim = as.list (head (names (df), -1)),
            var = as.list (tail (names (df), 1))
        )
        
        ## Dimension selection
        dim.buttons <- function (num, dim, label = dim) {
            choices <- c ('all', 'some', 'one', 'none')
            names (choices) <- c (paste ('All (', dc$elm.nb[[dim]], ')', sep = ''), 'Some', 'One', 'Aggregate')
            return (renderUI ({ radioButtons (
                                    paste ('dim', num, '.selection', sep = ''),
                                    label = h4 (label), inline = TRUE,
                                    choices = choices, selected = 'none'
                                )
            }))
        }
        
        output$dim1.buttons <- dim.buttons (1, dc$dim.names[1], paste ('Select', dim.str (dc$dim.names[1], TRUE)))
        output$dim2.buttons <- dim.buttons (2, dc$dim.names[2], paste ('Select', dim.str (dc$dim.names[2], TRUE)))
        output$dim3.buttons <- dim.buttons (3, dc$dim.names[3], paste ('Select', dim.str (dc$dim.names[3], TRUE)))
        
        ## Selection list
        head.selection <- 10000

        dim.list <- function (num, dim, order = 'value') {
            dim.order <- 1:min(dc$elm.nb[[dim]], head.selection)
            if (order == 'value') { dim.order <- dc$obs[[dim]]$elms[[dim]] [head (order (-dc$obs[[dim]]$vars[[dc$var.names[1]]]), head.selection)] }
            if (order == 'key') { dim.order <- head (order (dc$elm.names[[dim]]), head.selection) }

            dim.list <- as.list (dc$elm.names[[dim]][dim.order])
            names (dim.list) <- paste (dc$elm.names[[dim]][dim.order], ' (', round (dc$obs[[dim]]$vars[[dc$var.names[1]]][match (dim.order, dc$obs[[dim]]$elms[[dim]])]), ' ', dc$var.names[1], ')', sep='')

            return (renderUI ({ conditionalPanel (
                                    condition = paste ('input["dim', num, '.selection"] == "one"', sep = ''),
                                    selectInput (
                                        paste ('selected.dim', num, sep = ''),
                                        label = NULL,
                                        choices = dim.list
                                    )
                                )
            }))
        }

        output$dim1.list <- dim.list (1, dc$dim.names[1])
        output$dim2.list <- dim.list (2, dc$dim.names[2], 'key')
        output$dim3.list <- dim.list (3, dc$dim.names[3], 'key')

        ## Selection slider
        dim.slider <- function (num, dim) {
            return (renderUI ({ conditionalPanel (
                                    condition = paste('input["dim', num, '.selection"] == "some"', sep = ''),
                                    sliderInput (
                                        paste ('dim', num, '.number', sep=''),
                                        label = NULL,
                                        value = 5, min = 1, max = dc$elm.nb[[dim]], step = 1
                                    )
                                )
            }))
        }

        output$dim1.slider <- dim.slider (1, dc$dim.names[1])
        output$dim2.slider <- dim.slider (2, dc$dim.names[2])
        output$dim3.slider <- dim.slider (3, dc$dim.names[3])

        output$input.panel.2 <- renderUI ({
            wellPanel (
                h4 ("Normalise data"),
                conditionalPanel (
                    condition = "input['dim1.selection'] != 'none'",
                    checkboxInput (
                        "dim1.normalisation",
                        label = paste ('By', dim.str (dc$dim.names[1], TRUE)),
                        value = FALSE
                    )
                ),
                conditionalPanel (
                    condition = "input['dim2.selection'] != 'none'",
                    checkboxInput (
                        "dim2.normalisation",
                        label = paste ('By', dim.str (dc$dim.names[2], TRUE)),
                        value = FALSE
                    )
                ),
                conditionalPanel (
                    condition = "input['dim3.selection'] != 'none'",
                    checkboxInput (
                        "dim3.normalisation",
                        label = paste ('By', dim.str (dc$dim.names[3], TRUE)),
                        value = FALSE
                    )
                ),
                radioButtons (
                    "deviation.type",
                    label = h4 ("Perform statistical test"),
                    inline = TRUE,
                    choices = c ("Ratio" = "ratio", "Poisson test" = "poisson", "KL Divergence" = "KLdiv")
                ),
                numericInput (
                    "outlier.threshold",
                    label = h5 ("Outlier threshold"),
                    3, min = 1, step = 1
                ),
                checkboxInput (
                    "outlier.labels",
                    label = "Display outlier labels",
                    value = TRUE
                )
            )
        })

        output$input.panel.3 <- renderUI ({
            wellPanel (
                numericInput (
                    "min.obs",
                    label = h4 ("Filter data (min value)"),
                    1, min = 0, step = 1
                )
            )
        })
        
        output$input.panel.4 <- renderUI ({
            rglwidgetOutput ("draw.cube", width="100%", height="150px")
        })
        
        return (dc)
    })



    ## DATA CUBE SCRIPTS

    ## DIMENSION SELECTION
    dc.agg <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        dc.agg <- dc()

        if (is.null (input$dim1.selection)) { return (dc.agg) }
        
        cat (file = stderr(), "NEW SELECTION\n")

        if (input$dim1.selection == 'one') {
            cat (file = stderr(), "-> dim1: select one\n")
            dc.agg <- select.elm_(dc.agg, dc.agg$dim.names[1], elm.array = c (input$selected.dim1))
        }
        if (input$dim2.selection == 'one') {
            cat (file = stderr(), "-> dim2: select one\n")
            dc.agg <- select.elm_(dc.agg, dc.agg$dim.names[2], elm.array = c (input$selected.dim2))
        }
        if (input$dim3.selection == 'one') {
            cat (file = stderr(), "-> dim3: select one\n")
            dc.agg <- select.elm_(dc.agg, dc.agg$dim.names[3], elm.array = c (input$selected.dim3))
        }

        if (input$dim1.selection == 'some') {
            cat (file = stderr(), "-> dim1: select some\n")
            dc.agg <- select.elm_(dc.agg, dc.agg$dim.names[1], top.nb = input$dim1.number)
        }
        if (input$dim2.selection == 'some') {
            cat (file = stderr(), "-> dim2: select some\n")
            dc.agg <- select.elm_(dc.agg, dc.agg$dim.names[2], top.nb = input$dim2.number)
        }
        if (input$dim3.selection == 'some') {
            cat (file = stderr(), "-> dim3: select some\n")
            dc.agg <- select.elm_(dc.agg, dc.agg$dim.names[3], top.nb = input$dim3.number)
        }

        dim.list <- dc.agg$dim.names
        if (input$dim1.selection == 'none') {
            cat (file = stderr(), "-> dim1: select none\n")
            dim.list <- dim.list [dim.list != dc.agg$dim.names[1]]
        }
        if (input$dim2.selection == 'none') {
            cat (file = stderr(), "-> dim2: select none\n")
            dim.list <- dim.list [dim.list != dc.agg$dim.names[2]]
        }
        if (input$dim3.selection == 'none') {
            cat (file = stderr(), "-> dim3: select none\n")
            dim.list <- dim.list [dim.list != dc.agg$dim.names[3]]
        }
        dc.agg <- select.dim_(dc.agg, dim.list)

        dc.agg <- arrange.elm_(dc.agg, dim = dc.agg$dim.names)
        
        return (dc.agg)
    })


    ## DATA NORMALISATION
    
    dc.dev <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        dc.dev <- dc.agg()

        if (is.null (input$dim1.selection)) { return (dc.dev) }

        if (dc.dev$dim.nb == 0) { return (dc.dev) }
        dc.name <- paste (dc.dev$dim.names, collapse = ".")
        if (length (dc.dev$obs[[dc.name]]$vars[[dc.dev$var.names[1]]]) <= 1) { return (dc.dev) }

        cat (file = stderr(), "NEW NORMALISATION\n")

        if (dc.dev$dim.nb > 0) { 
            dim.list <- c()
            if (input$dim1.selection %in% c ('all','some','one') && input$dim1.normalisation) {
                cat (file = stderr(), "-> dim1: normalise\n")
                dim.list <- append (dim.list, dc()$dim.names[1])
            }
            if (input$dim2.selection %in% c ('all','some','one') && input$dim2.normalisation) {
                cat (file = stderr(), "-> dim2: normalise\n")
                dim.list <- append (dim.list, dc()$dim.names[2])
            }
            if (input$dim3.selection %in% c ('all','some','one') && input$dim3.normalisation) {
                cat (file = stderr(), "-> dim3: normalise\n")
                dim.list <- append (dim.list, dc()$dim.names[3])
            }

            dc.dev <- compute.model_(dc.dev, dim = dim.list, deviation.type = input$deviation.type, deviation.threshold = input$outlier.threshold)
        }

        return (dc.dev)
    })

    dc.out <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.dev()$dim.nb == 0) { return (dc.dev()) }
        dc.out <- dc.dev()

        if (is.null (input$dim1.selection)) { return (dc.out) }

        dc.name <- paste (dc.out$dim.names, collapse = ".")
        keep <- dc.out$obs[[dc.name]]$vars[[dc.out$var.names[1]]] >= input$min.obs
        dc.out$obs[[dc.name]]$elms <- lapply (dc.out$obs[[dc.name]]$elms, function (dim) dim [keep])
        dc.out$obs[[dc.name]]$vars <- lapply (dc.out$obs[[dc.name]]$vars, function (dim) dim [keep])
        
        return (dc.out)
    })

    ## DATA STRUCTURE
    output$data.structure <- renderText ({
        if (is.null (input$dataset)) { return (NULL) }
        return (paste (capture.output (str (dc.out())), '\n'))
    })

    ## DRAW DATA CUBE
    output$draw.cube <- renderRglwidget ({
        dim.list <- c()
        if (input$dim1.selection %in% c ('all','some','one') && input$dim1.normalisation) { dim.list <- append (dim.list, dc()$dim.names[1]) }
        if (input$dim2.selection %in% c ('all','some','one') && input$dim2.normalisation) { dim.list <- append (dim.list, dc()$dim.names[2]) }
        if (input$dim3.selection %in% c ('all','some','one') && input$dim3.normalisation) { dim.list <- append (dim.list, dc()$dim.names[3]) }

        draw.cube (dc.agg(), dims = dc()$dim.names, dim.dev = dim.list)
        scene <- scene3d()
        rgl.close()
        rglwidget (scene)
    })

    ## DATA PLOT
    data.plot <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        dims <- c()
        if (input$dim1.selection %in% c ('some','all')) { dims <- append (dims, dc()$dim.names[1]) }
        if (input$dim2.selection %in% c ('some','all')) { dims <- append (dims, dc()$dim.names[2]) }
        if (input$dim3.selection %in% c ('some','all')) { dims <- append (dims, dc()$dim.names[3]) }
        if (length (dims) > 2) { return (NULL) }

        dc.name <- paste (dc.dev()$dim.names, collapse='.')
        if (dc.dev()$dim.nb == 0 || length (dc.dev()$obs[[dc.name]]$vars[[dc.dev()$var.names[1]]]) == 0) { return (NULL) }
        
        dc.plot <- dc.dev()

        ## Set main and sep dims
        main.dim <- dims[1]
        sep.dim <- NULL
        if (length (dims) == 2) {
            if (length (dc.plot$elm.names[[dims[1]]]) > length (dc.plot$elm.names[[dims[2]]])) {
                sep.dim <- dims[2]
            } else {
                sep.dim <- dims[1]
                main.dim <- dims[2]
            }
        }

        ## Set ploted variable
        var <- dc.plot$var.names[1]
        if (input$dim1.selection %in% c ('all','some','one') && input$dim1.normalisation
            || input$dim2.selection %in% c ('all','some','one') && input$dim2.normalisation
            || input$dim3.selection %in% c ('all','some','one') && input$dim3.normalisation) {
            if (input$deviation.type == 'ratio') { var <- 'ratio' } else { var <- 'deviation' }
        }

        ## Set type of plot
        type <- 'col'
        if (var == dc.plot$var.names[1] && main.dim %in% time.names) { type <- 'line' }
        
        ## Arrange elements        
        if (! is.null (sep.dim) && ! sep.dim %in% time.names) {
            dc.plot <- arrange.elm_(dc.plot, sep.dim, var = dc.plot$var.names[1], decreasing = TRUE)
        }
        if (! main.dim %in% time.names) {
            dc.plot <- arrange.elm_(dc.plot, main.dim, var = dc.plot$var.names[1], decreasing = TRUE)
        }

        if (type == 'line') {
            if (is.null (sep.dim)) {
                dc.plot <- arrange.elm_(dc.plot, dim = c (main.dim))
            } else {
                 dc.plot <- arrange.elm_(dc.plot, dim = c (main.dim, sep.dim))
            }
        }

        ## Get plot
        plot <- plot.var_(dc.plot, var = var, sep.dim = sep.dim, type = type)
        plot <- plot + theme (text = element_text (size = 20))

        return (plot)
    })

    output$data.plot <- renderPlot ({ data.plot() }, height=900)
    
    output$download.data.plot.pdf <- downloadHandler (
        filename = function () { "data.plot.pdf" },
        content = function (filename) { ggsave (filename, plot=data.plot(), device='pdf') } 
    )

    output$download.data.plot.png <- downloadHandler (
        filename = function () { "data.plot.png" },
        content = function (filename) { ggsave (filename, plot=data.plot(), device='png') } 
    )



    ## DATA BIPLOT
    data.biplot <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        dims <- c()
        if (input$dim1.selection %in% c ('some','all')) { dims <- append (dims, dc()$dim.names[1]) }
        if (input$dim2.selection %in% c ('some','all')) { dims <- append (dims, dc()$dim.names[2]) }
        if (input$dim3.selection %in% c ('some','all')) { dims <- append (dims, dc()$dim.names[3]) }
        if (length (dims) != 2) { return (NULL) }

        dc.name <- paste (dc.dev()$dim.names, collapse='.')
        if (dc.dev()$dim.nb == 0 || length (dc.dev()$obs[[dc.name]]$vars[[dc.dev()$var.names[1]]]) == 0) { return (NULL) }
        
        dc.plot <- dc.dev()

        ## Set 1st and 2nd dims
        first.dim <- dims[1]
        second.dim <- dims[2]
        if (length (dc.plot$elm.names[[dims[1]]]) > length (dc.plot$elm.names[[dims[2]]])) {
            first.dim <- dims[2]
            second.dim <- dims[1]
        }

        ## Set ploted variable
        var <- dc.plot$var.names[1]
        if (input$dim1.selection %in% c ('all','some','one') && input$dim1.normalisation
            || input$dim2.selection %in% c ('all','some','one') && input$dim2.normalisation
            || input$dim3.selection %in% c ('all','some','one') && input$dim3.normalisation) {
            if (input$deviation.type == 'ratio') { var <- 'ratio' } else { var <- 'deviation' }
        }

        ## Get plot
        plot <- biplot.var_(dc.plot, first.dim, second.dim, var = var)
        plot <- plot + theme (text = element_text (size = 20))

        return (plot)
    })

    output$data.biplot <- renderPlot ({ data.biplot() }, height=900)
    
    output$download.data.biplot.pdf <- downloadHandler (
        filename = function () { "data.biplot.pdf" },
        content = function (filename) { ggsave (filename, plot=data.biplot(), device='pdf') } 
    )

    output$download.data.biplot.png <- downloadHandler (
        filename = function () { "data.biplot.png" },
        content = function (filename) { ggsave (filename, plot=data.biplot(), device='png') } 
    )

    
    ## OUTLIER PLOT
    outlier.plot <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        dc.name <- paste (dc.out()$dim.names, collapse='.')
        if (dc.out()$dim.nb == 0 || length (dc.out()$obs[[dc.name]]$vars[[dc.out()$var.names[1]]]) == 0) { return (NULL) }

        dc.plot <- dc.out()

        labels <- input$outlier.labels
        repel.labels <- (sum (dc.plot$obs[[dc.name]]$vars[['outlier']] != 0) <= 200)
        plot <- plot.outliers (dc.plot, labels = labels, repel.labels = repel.labels)
        plot <- plot + theme (text = element_text (size = 20))

        return (plot)
    })
    
    output$outlier.plot <- renderPlot ({ outlier.plot() }, height=900)
    
    output$download.outlier.plot.pdf <- downloadHandler (
        filename = function () { "outlier.plot.pdf" },
        content = function (filename) { ggsave (filename, plot=outlier.plot(), device='pdf') } 
    )

    output$download.outlier.plot.png <- downloadHandler (
        filename = function () { "outlier.plot.png" },
        content = function (filename) { ggsave (filename, plot=outlier.plot(), device='png') } 
    )


    ## DISTRIBUTION PLOT
    output$distribution.plot <- renderPlot ({
        if (is.null (input$dataset)) { return (NULL) }

        dc.name <- paste (dc.dev()$dim.names, collapse='.')
        if (dc.dev()$dim.nb == 0 || length (dc.dev()$obs[[dc.name]]$vars[[dc.dev()$var.names[1]]]) == 0) { return (NULL) }

        dc.plot <- dc.dev()

        plot <- data.distribution (dc.plot, data = 'deviation', threshold = input$outlier.threshold)
        plot <- plot + theme (text = element_text (size = 20))

        return (plot)
    }, height=450)


    ## OUTLIER LISTS
    positive.outlier.list <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.dev()$dim.nb == 0) { return (NULL) }

        df <- as.data.frame (dc.dev())
        df <- df [df$outlier == 1, ]
        df <- df [order (-df$deviation), ]
        df$outlier <- NULL

        return (df)
    })
    
    output$positive.outlier.list <- renderDataTable ({ positive.outlier.list() })

    output$download.positive.outlier.list.csv <- downloadHandler (
        filename = function () { "positive.outlier.list.csv" },
        content = function (filename) { write.csv (positive.outlier.list(), filename, row.names=FALSE) } 
    )

    output$download.positive.outlier.list.json <- downloadHandler (
        filename = function () { "positive.outlier.list.json" },
        content = function (filename) { write (toJSON (unname (split (positive.outlier.list(), 1:nrow(positive.outlier.list())))), filename) }
    )


    negative.outlier.list <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.dev()$dim.nb == 0) { return (NULL) }

        df <- as.data.frame (dc.dev())
        df <- df [df$outlier == -1, ]
        df <- df [order (df$deviation), ]
        df$outlier <- NULL

        return (df)
    })

    output$negative.outlier.list <- renderDataTable ({ negative.outlier.list() })

    output$download.negative.outlier.list.csv <- downloadHandler (
        filename = function () { "negative.outlier.list.csv" },
        content = function (filename) { write.csv (negative.outlier.list(), filename, row.names=FALSE) } 
        )

    output$download.negative.outlier.list.json <- downloadHandler (
        filename = function () { "negative.outlier.list.json" },
        content = function (filename) { write (toJSON (unname (split (negative.outlier.list(), 1:nrow(negative.outlier.list())))), filename) }
    )
}
