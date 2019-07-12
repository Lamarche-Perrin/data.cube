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

library ("shiny")
library ("rjson")

source ("../../../src/data.cube.R")

options (rgl.useNULL = TRUE)
function (input, output, session) {

    output$app.title <- renderUI ({ "Multidimensional Outlier Explorer" })

    ## DYNAMIC INTERFACE SCRIPTS
    dc <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        output$app.title <- renderUI ({ paste0 ("Mutlidimensional Outlier Explorer - ", input$dataset) })

        filename <- paste0 ("../../../data/", input$dataset, ".csv")
        ## cat (file = stderr(), filename, "\n")

        df <- filename %>% read_csv
        dc <- df %>% as.data.cube_(df %>% names %>% head (-1))

        ## TODO: select other variables
        var.name <- var.names (dc) [1]
        dc <- dc %>% select.var_(var.name)

        ## Dimension selection buttons
        dim.buttons <- function (dim.name) {
            choices <- c ("all", "some", "one", "none")
            names (choices) <- c (paste0 ("All (", elm.nb (dc, dim.name), ")"), "Some", "One", "Aggregate")
            return (radioButtons (
                paste0 (dim.name, ".selection.mode"),
                label = h4 (paste ("Select ", dim.name)),
                inline = TRUE,
                choices = choices, selected = "none"
            ))
        }
        
        ## Dimension element list
        elm.list.maxsize <- 1000
        elm.list.byvalue <- FALSE

        dim.elm.list <- function (dim.name) {
            if (elm.list.byvalue) {
                elm.df <-
                    dc %>%
                    select.dim_(dim.name) %>%
                    arrange.elm_(dim.name, paste0 ("desc (", var.name, ")")) %>%
                    as.data.frame
            } else {
                elm.df <-
                    dc %>%
                    select.dim_(dim.name) %>%
                    arrange.elm_(dim.name, "name") %>%
                    as.data.frame
            }

            elm.list <- elm.df %>% pull (!! dim.name) %>% as.character
            names (elm.list) <- paste0 (elm.list, " (", round (elm.df %>% pull (!! var.name)), " ", var.name, ")")

            return (conditionalPanel (
                condition = paste0 ("input[\"", dim.name, ".selection.mode\"] == \"one\""),
                selectInput (
                    paste0 (dim.name, ".selected.elm.name"),
                    label = NULL,
                    choices = elm.list
                )
            ))
        }

        ## Dimension element slider
        dim.elm.slider <- function (dim.name) {
            return (conditionalPanel (
                condition = paste0 ("input[\"", dim.name, ".selection.mode\"] == \"some\""),
                sliderInput (
                    paste0 (dim.name, ".selected.elm.nb"),
                    label = NULL,
                    value = 5,
                    min = 1,
                    max = elm.nb (dc, dim.name),
                    step = 1
                )
            ))
        }

        ## Selection panel
        UI.list <- list()
        for (dim.name in dim.names (dc)) {
            UI.list <- append (UI.list,
                               list (dim.buttons (dim.name), dim.elm.list (dim.name), dim.elm.slider (dim.name))
                               )
        }
        output$input.selection.panel <- renderUI ({ wellPanel (UI.list) })
        
        ## Model panel
        dim.model.panel <- function (dim.name) {
            conditionalPanel (
                condition = paste0 ("input[\"", dim.name, ".selection.mode\"] != \"none\""),
                checkboxInput (
                    paste0 (dim.name, ".in-model"),
                    label = paste ("By", dim.name),
                    value = FALSE
                )
            )
        }
        
        output$input.model.panel <- renderUI ({
            wellPanel (
                h4 ("Normalise data"),
                lapply (dim.names (dc), dim.model.panel),
                radioButtons (
                    "deviation.type",
                    label = h4 ("Perform statistical test"),
                    inline = TRUE,
                    choices = c (
                        "No test (ratio)" = "ratio",
                        "Poisson test" = "poisson",
                        "KL Divergence" = "KLdiv",
                        "Chi-squared" = "chi2"
                    )
                ),
                numericInput (
                    "outlier.threshold",
                    label = h5 ("Outlier threshold"),
                    3,
                    min = 1,
                    step = 1
                ),
                checkboxInput (
                    "outlier.labels",
                    label = "Display outlier labels",
                    value = FALSE
                )
            )
        })

        ## Filter panel
        output$input.filter.panel <- renderUI ({
            wellPanel (
                numericInput (
                    "variable.min.value",
                    label = h4 ("Filter data (min value)"),
                    1,
                    min = 0,
                    step = 1
                )
            )
        })

        ## Cube panel
        output$input.cube.panel <- renderUI ({
            ##rglwidgetOutput ("draw.cube", width = "100%", height = "150px")
        })
        
        return (dc)
    })



    ## DATA CUBE SCRIPTS

    ## DIMENSION SELECTION
    dc.selected.dims <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        
        dc.selected.dims <- dc()

        select.dim.names <- c()
        for (dim.name in dim.names (dc.selected.dims)) {

            selection.mode <- input[[paste0 (dim.name, ".selection.mode")]]
            if (is.null (selection.mode)) { selection.mode <- "none" }
            
            if (selection.mode == "one") {
                dc.selected.dims <- dc.selected.dims %>%
                    filter.elm_(dim.name, paste0 ("name == ", input[[paste0 (dim.name, ".selected.elm.name")]]))
            }

            if (selection.mode == "some") {
                dc.selected.dims <- dc.selected.dims %>% top_n.elm_(dim.name, input[[paste0 (dim.name, "selected.elm.nb")]])
            }

            if (selection.mode != "none") {
                select.dim.names <- c (select.dim.names, dim.names)
            }
        }

        dc.selected.dims <- dc.selected.dims %>% select.dim_(select.dim.names)

        return (dc.selected.dims)
    })


    ## MODEL COMPUTATION
    
    dc.computed.var.model <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        dc.computed.var.model <- dc.selected.dims()
        var.name <- var.names (dc.computed.var.model)

        cat (file = stderr(), dim.names (dc.computed.var.model), "\n")
        formula <- paste0 (var.name, " (", dim.names (dc.computed.var.model) %>% paste (collapse = " * "), ") ~ ")

        delim <- ""
        for (dim.name in dim.names (dc.computed.var.model)) {

            inmodel <- input[[paste0 (dim.name, ".in-model")]]
            if (is.null (inmodel)) { inmodel <- FALSE }

            if (inmodel) {
                formula <- paste0 (formula, delim, var.name, " (", dim.name, ")")
                delim <- " * "
            }
        }

        if (delim == "") { formula <- paste0 (formula, "NULL") }
        cat (file = stderr(), formula, "\n")

        dc.computed.var.model <-
            dc.computed.var.model %>%
            compute.var.model_(
                formula,
                deviation.type = input$deviation.type##,
                ## deviation.threshold = input$deviation.threshold
            )

        return (dc.computed.var.model)
    })

    dc.final <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        
        if (dim.nb (dc.computed.var.model()) == 0) { return (dc.computed.var.model()) }
        dc.final <- dc.computed.var.model()
        ## dc.final$data$display <- (dc.final$data$obs >= input$min.obs)
        return (dc.final)
    })

    ## DATA STRUCTURE
    output$data.structure <- renderText ({
        if (is.null (input$dataset)) { return (NULL) }
        return (paste (capture.output (str (dc.final())), "\n"))
    })

    ## DATA PLOT
    ## data.plot <- reactive ({
    ##     if (is.null (input$dataset)) { return (NULL) }
    ##     if (dc.final()$dim.nb == 0 || length (dc.final()$data$obs) == 0) { return (NULL) }
        
    ##     dc.plot <- dc.final()
        
    ##     rank <- NULL
    ##     if (input$time.selection %in% c ("one","none")) {
    ##         dc.plot$data$rank <- rank(-dc.plot$data$obs)
    ##         rank <- "rank"
    ##     }

    ##     dims <- c()
    ##     if (! input$user.selection %in% c ("one","none")) { dims <- append (dims, "user") }
    ##     if (! input$topic.selection %in% c ("one","none")) { dims <- append (dims, "topic") }
    ##     if (! input$time.selection %in% c ("one","none")) { dims <- append (dims, "time") }

    ##     sep.dim <- NULL
    ##     if (length (dims) == 2) {
    ##         if (length (dc.plot$elem.names[[dims[1]]]) > length (dc.plot$elem.names[[dims[2]]])) { sep.dim <- dims[2] } else { sep.dim <- dims[1] }
    ##     }

    ##     data <- "obs"
    ##     if (input$user.selection %in% c ("all","some","one") && input$user.normalisation || input$topic.selection %in% c ("all","some","one") && input$topic.normalisation || input$time.selection %in% c ("all","some","one") && input$time.normalisation) { data <- "obs/exp" }

    ##     plot <- plot.data (dc.plot, data = data, rank = rank, display = "display", sep.dim = sep.dim) +
    ##         theme (text = element_text (size = 20))

    ##     if (! is.null (sep.dim)) { dims <- dims [dims != sep.dim] }
    ##     xlab.str <- paste (sapply (dims, function (dim) { return (dim.str (dim, TRUE)) }), collapse = " x ")
    ##     plot <- plot + xlab (xlab.str)
        
    ##     if (data == "obs") { plot <- plot + ylab ("Number of comments") }
    ##     else if (data == "obs/exp") { plot <- plot + ylab ("Ratio of observed comments vs. expected comments") }

    ##     if (! is.null (sep.dim)) {
    ##         fill.str <- dim.str (sep.dim, TRUE)
    ##         plot <- plot + guides (fill = guide_legend(title = fill.str))
    ##     }
        
    ##     return (plot)
    ## })

    ## output$data.plot <- renderPlot ({ data.plot() }, height = 900)
    
    ## output$download.data.plot.pdf <- downloadHandler (
    ##     filename = function () { "data.plot.pdf" },
    ##     content = function (filename) { ggsave (filename, plot = data.plot(), device = "pdf") } 
    ## )

    ## output$download.data.plot.png <- downloadHandler (
    ##     filename = function () { "data.plot.png" },
    ##     content = function (filename) { ggsave (filename, plot = data.plot(), device = "png") } 
    ## )

    
    ## OUTLIER PLOT
    ## outlier.plot <- reactive ({
    ##     if (is.null (input$dataset)) { return (NULL) }
    ##     if (dc.final()$dim.nb == 0 || length (dc.final()$data$obs) == 0) { return (NULL) }

    ##     labels <- input$outlier.labels && sum (dc.final()$data$out != 0) <= 100
    ##     plot <- plot.outliers (dc.final(), display = "display", labels = labels) +
    ##         theme (text = element_text (size = 20))

    ##     plot <- plot + ggtitle ("") +
    ##         xlab ("Number of comments") +
    ##         ylab ("Ratio of observed comments vs. expected comments") +
    ##         guides (
    ##             fill = guide_colourbar (title = "Deviation", order = 1),
    ##             size = guide_legend (title = "Absolute\ndeviation", order = 2),
    ##             shape = guide_legend (title = "Outliers", order = 3)
    ##         )

    ##     return (plot)
    ## })
    
    ## output$outlier.plot <- renderPlot ({ outlier.plot() }, height = 900)
    
    ## output$download.outlier.plot.pdf <- downloadHandler (
    ##     filename = function () { "outlier.plot.pdf" },
    ##     content = function (filename) { ggsave (filename, plot = outlier.plot(), device = "pdf") } 
    ## )

    ## output$download.outlier.plot.png <- downloadHandler (
    ##     filename = function () { "outlier.plot.png" },
    ##     content = function (filename) { ggsave (filename, plot = outlier.plot(), device = "png") } 
    ## )


    ## DISTRIBUTION PLOT
    ## output$distribution.plot <- renderPlot ({
    ##     if (is.null (input$dataset)) { return (NULL) }
    ##     if (dc.dev2()$dim.nb == 0 || length (dc.dev2()$data$obs) == 0) { return (NULL) }

    ##     plot <- data.distribution (dc.dev2(), data = "dev", threshold = input$outlier.threshold) +
    ##         theme (text = element_text (size = 20))

    ##     plot <- plot + ggtitle ("") + xlab ("Deviation") + ylab ("Number of observations")

    ##     return (plot)
    ## }, height = 450)


    ## OUTLIER LISTS
    ## positive.outlier.list <- reactive ({
    ##     if (is.null (input$dataset)) { return (NULL) }
    ##     if (dc.final()$dim.nb == 0) { return (NULL) }

    ##     dc.list <- dc.final()
    ##     dc.list$data$display <- (dc.list$data$out == 1)
    ##     dc.list$data$rank <- rank (-dc.list$data$dev)

    ##     df <- as.data.frame (dc.list, display = "display", rank = "rank")
    ##     df$out <- NULL

    ##     return (df)
    ## })
    
    ## output$positive.outlier.list <- renderDataTable ({ positive.outlier.list() })

    ## output$download.positive.outlier.list.csv <- downloadHandler (
    ##     filename = function () { "positive.outlier.list.csv" },
    ##     content = function (filename) { write.csv (positive.outlier.list(), filename, row.names = FALSE) } 
    ## )

    ## output$download.positive.outlier.list.json <- downloadHandler (
    ##     filename = function () { "positive.outlier.list.json" },
    ##     content = function (filename) { write (toJSON (unname (split (positive.outlier.list(), 1:nrow(positive.outlier.list())))), filename) }
    ## )


    ## negative.outlier.list <- reactive ({
    ##     if (is.null (input$dataset)) { return (NULL) }
    ##     if (dc.final()$dim.nb == 0) { return (NULL) }

    ##     dc.list <- dc.final()
    ##     dc.list$data$display <- (dc.list$data$out == -1)
    ##     dc.list$data$rank <- rank (dc.list$data$dev)

    ##     df <- as.data.frame (dc.list, display = "display", rank = "rank")
    ##     df$out <- NULL

    ##     return (df)
    ## })

    ## output$negative.outlier.list <- renderDataTable ({ negative.outlier.list() })

    ## output$download.negative.outlier.list.csv <- downloadHandler (
    ##     filename = function () { "negative.outlier.list.csv" },
    ##     content = function (filename) { write.csv (negative.outlier.list(), filename, row.names = FALSE) } 
    ##     )

    ## output$download.negative.outlier.list.json <- downloadHandler (
    ##     filename = function () { "negative.outlier.list.json" },
    ##     content = function (filename) { write (toJSON (unname (split (negative.outlier.list(), 1:nrow(negative.outlier.list())))), filename) }
    ## )


    ## DRAW DATA CUBE
    ## output$draw.cube <- renderRglwidget ({
    ##     dims <- c()
    ##     if (input$user.selection %in% c ("all","some","one") && input$user.normalisation) { dims <- append(dims,"user") }
    ##     if (input$topic.selection %in% c ("all","some","one") && input$topic.normalisation) { dims <- append(dims,"topic") }
    ##     if (input$time.selection %in% c ("all","some","one") && input$time.normalisation) { dims <- append(dims,"time") }

    ##     draw.cube (dc.selected.dims(), dims = dc()$dim.names, dim.dev = dims)
    ##     scene <- scene3d()
    ##     rgl.close()
    ##     rglwidget (scene)
    ## })
}
