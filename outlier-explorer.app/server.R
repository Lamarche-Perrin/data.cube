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

source ('../data.cube.R')

options (rgl.useNULL = TRUE)
function (input, output, session) {

    output$app.title <- renderUI ({ 'Outlier Explorer' })

    dim.str <- function (dim.name, plural=FALSE) {
        if (dim.name == 'user') { str <- 'User' }
        if (dim.name == 'topic') { str <- 'Topic' }
        if (dim.name == 'time') { str <- 'Week' }
        if (plural) { str <- paste (str, 's', sep='') }
        return (str)
    }
    
    dc <-  reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        
        output$app.title <- renderUI ({ paste ('Outlier Explorer - ', input$dataset, sep='') })

        file <- paste ('../data/', input$dataset, '.csv', sep='')
        cat (file=stderr(), file, "\n")
    
        df <- read.csv (file, stringsAsFactors=FALSE)
        dc <- as.data.cube (df)

        ## Dimension selection
        dim.buttons <- function (dim, label=dim) {
            choices <- c('all', 'some', 'one', 'none')
            names(choices) <- c(paste('All (', dc$elem.nb[[dim]], ')', sep=''), 'Some', 'One', 'Aggregate')
            return (renderUI ({ radioButtons (paste(dim, '.selection', sep=''), label=h4(label), inline=TRUE,
                                              choices=choices, selected='none')
            }))
        }
        
        output$user.buttons <- dim.buttons ('user', 'Select users')
        output$topic.buttons <- dim.buttons ('topic', 'Select topics')
        output$time.buttons <- dim.buttons ('time', 'Select weeks')
        
        ## Selection list
        head.selection <- 1000

        dim.list <- function (dim, order='value') {
            dim.order <- 1:min(dc$elem.nb[[dim]], head.selection)
            if (order == 'value') { dim.order <- dc$margins[[dim]]$cells[[dim]] [head (order(-dc$margins[[dim]]$data$obs), head.selection)] }
            if (order == 'key') { dim.order <- head (order(dc$elem.names[[dim]]), head.selection) }

            dim.list <- as.list (dc$elem.names[[dim]][dim.order])
            names (dim.list) <- paste (dc$elem.names[[dim]][dim.order], ' (', round (dc$margins[[dim]]$data$obs[match (dim.order, dc$margins[[dim]]$cells[[dim]])]), ' comments)', sep='')

            return (renderUI ({ conditionalPanel (condition=paste('input["', dim, '.selection"] == "one"', sep=''),
                                                  selectInput (paste ('selected', dim, sep='.'), label=NULL,
                                                               choices=dim.list)
                                                  )
            }))
        }

        output$user.list <- dim.list ('user')
        output$topic.list <- dim.list ('topic', 'key')
        output$time.list <- dim.list ('time', 'key')

        ## Selection slider
        dim.slider <- function (dim) {
            return (renderUI ({ conditionalPanel (condition=paste('input["', dim, '.selection"] == "some"', sep=''),
                                                  sliderInput (paste (dim, 'number', sep='.'), label=NULL,
                                                               value=5, min=1, max=dc$elem.nb[[dim]], step=1)
                                                  )
            }))
        }

        output$user.slider <- dim.slider ('user')
        output$topic.slider <- dim.slider ('topic')
        output$time.slider <- dim.slider ('time')

        output$input.panel.2 <- renderUI ({
            wellPanel (
                h4 ("Normalise data"),
                conditionalPanel (condition="input['user.selection'] != 'none'", checkboxInput ("user.normalisation", label="By users", value=FALSE)),
                conditionalPanel (condition="input['topic.selection'] != 'none'", checkboxInput ("topic.normalisation", label="By topics", value=FALSE)),
                conditionalPanel (condition="input['time.selection'] != 'none'", checkboxInput ("time.normalisation", label="By weeks", value=FALSE)),
                radioButtons ("deviation.type", label=h4("Perform statistical test"), inline=TRUE,
                              choices=c("Poisson test"="poisson", "KL Divergence"="KLdiv")),
                numericInput ("outlier.threshold", label=h5("Outlier threshold"), 3, min=1, step=1),
                checkboxInput ("outlier.labels", label="Display outlier labels", value=FALSE)
            )
        })

        output$input.panel.3 <- renderUI ({
            wellPanel (
                numericInput ("min.obs", label=h4("Filter data (min value)"), 1, min=0, step=1)
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

        elems <- list()
        if (input$user.selection == 'one') { elems[['user']] <- c(input$selected.user) }
        if (input$topic.selection == 'one') { elems[['topic']] <- c(input$selected.topic) }
        if (input$time.selection == 'one') { elems[['time']] <- c(input$selected.time) }
        if (length(elems) > 0) { dc.agg <- select.elems (dc.agg, elems) }

        elems <- list()
        if (input$user.selection == 'some') { elems[['user']] <- dc.agg$elem.names$user[dc.agg$margins$user$cells$user[order(-dc.agg$margins$user$data$obs)[1:input$user.number]]] }
        if (input$topic.selection == 'some') { elems[['topic']] <- dc.agg$elem.names$topic[dc.agg$margins$topic$cells$topic[order(-dc.agg$margins$topic$data$obs)[1:input$topic.number]]] }
        if (input$time.selection == 'some') { elems[['time']] <- dc.agg$elem.names$time[dc.agg$margins$time$cells$time[order(-dc.agg$margins$time$data$obs)[1:input$time.number]]] }
        if (length(elems) > 0) { dc.agg <- select.elems (dc.agg, elems) }

        dims <- c()
        if (input$user.selection == 'none') { dims <- append(dims,'user') }
        if (input$topic.selection == 'none') { dims <- append(dims,'topic') }
        if (input$time.selection == 'none') { dims <- append(dims,'time') }
        if (length(dims) > 0) { dc.agg <- remove.dims (dc.agg, dims) }
        
        return (dc.agg)
    })


    ## DATA NORMALISATION
    
    dc.dev1 <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }

        dc.dev1 <- dc.agg()

        if (dc.dev1$dim.nb > 0) { 
            dims <- c()
            if (input$user.selection %in% c('all','some','one') && input$user.normalisation) { dims <- append(dims,'user') }
            if (input$topic.selection %in% c('all','some','one') && input$topic.normalisation) { dims <- append(dims,'topic') }
            if (input$time.selection %in% c('all','some','one') && input$time.normalisation) { dims <- append(dims,'time') }

            dc.dev1 <- compute.expected (dc.dev1, dims=dims)
        }

        return (dc.dev1)
    })

    dc.dev2 <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.dev1()$dim.nb == 0) { return (dc.dev1()) }
        return (compute.deviated (dc.dev1(), type=input$deviation.type))
    })

    dc.dev3 <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.dev2()$dim.nb == 0) { return (dc.dev2()) }
        return (compute.outliers (dc.dev2(), threshold=input$outlier.threshold))
    })

    dc.out <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.dev3()$dim.nb == 0) { return (dc.dev3()) }
        dc.out <- dc.dev3()
        dc.out$data$display <- (dc.out$data$obs >= input$min.obs)
        return (dc.out)
    })

    ## DATA STRUCTURE
    output$data.structure <- renderText ({
        if (is.null (input$dataset)) { return (NULL) }
        return (paste (capture.output (str (dc.out())), '\n'))
    })

    ## DATA PLOT
    data.plot <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.out()$dim.nb == 0 || length (dc.out()$data$obs) == 0) { return (NULL) }
        
        dc.plot <- dc.out()
        
        rank <- NULL
        if (input$time.selection %in% c('one','none')) {
            dc.plot$data$rank <- rank(-dc.plot$data$obs)
            rank <- 'rank'
        }

        dims <- c()
        if (! input$user.selection %in% c('one','none')) { dims <- append (dims, 'user') }
        if (! input$topic.selection %in% c('one','none')) { dims <- append (dims, 'topic') }
        if (! input$time.selection %in% c('one','none')) { dims <- append (dims, 'time') }

        sep.dim <- NULL
        if (length (dims) == 2) {
            if (length (dc.plot$elem.names[[dims[1]]]) > length (dc.plot$elem.names[[dims[2]]])) { sep.dim <- dims[2] } else { sep.dim <- dims[1] }
        }

        data <- 'obs'
        if (input$user.selection %in% c('all','some','one') && input$user.normalisation || input$topic.selection %in% c('all','some','one') && input$topic.normalisation || input$time.selection %in% c('all','some','one') && input$time.normalisation) { data <- 'obs/exp' }

        plot <- plot.data (dc.plot, data=data, rank=rank, display='display', sep.dim=sep.dim) +
            theme (text=element_text (size=20))

        if (! is.null (sep.dim)) { dims <- dims [dims != sep.dim] }
        xlab.str <- paste (sapply (dims, function (dim) { return (dim.str (dim, TRUE)) }), collapse=' x ')
        plot <- plot + xlab (xlab.str)
        
        if (data == 'obs') { plot <- plot + ylab ('Number of comments') }
        else if (data == 'obs/exp') { plot <- plot + ylab ('Ratio of observed comments vs. expected comments') }

        if (! is.null (sep.dim)) {
            fill.str <- dim.str (sep.dim, TRUE)
            plot <- plot + guides (fill=guide_legend(title=fill.str))
        }
        
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

    
    ## OUTLIER PLOT
    outlier.plot <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.out()$dim.nb == 0 || length (dc.out()$data$obs) == 0) { return (NULL) }

        labels <- input$outlier.labels && sum (dc.out()$data$out != 0) <= 100
        plot <- plot.outliers (dc.out(), display='display', labels=labels) +
            theme (text=element_text (size=20))

        plot <- plot + ggtitle ('') +
            xlab ('Number of comments') +
            ylab ('Ratio of observed comments vs. expected comments') +
            guides (
                fill = guide_colourbar (title='Deviation', order=1),
                size = guide_legend (title='Absolute\ndeviation', order=2),
                shape = guide_legend (title='Outliers', order=3)
            )

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
        if (dc.dev2()$dim.nb == 0 || length (dc.dev2()$data$obs) == 0) { return (NULL) }

        plot <- data.distribution (dc.dev2(), data='dev', threshold=input$outlier.threshold) +
            theme (text=element_text (size=20))

        plot <- plot + ggtitle ('') + xlab ('Deviation') + ylab ('Number of observations')

        return (plot)
    }, height=450)


    ## OUTLIER LISTS
    positive.outlier.list <- reactive ({
        if (is.null (input$dataset)) { return (NULL) }
        if (dc.out()$dim.nb == 0) { return (NULL) }

        dc.list <- dc.out()
        dc.list$data$display <- (dc.list$data$out == 1)
        dc.list$data$rank <- rank (-dc.list$data$dev)

        df <- as.data.frame (dc.list, display='display', rank='rank')
        df$out <- NULL

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
        if (dc.out()$dim.nb == 0) { return (NULL) }

        dc.list <- dc.out()
        dc.list$data$display <- (dc.list$data$out == -1)
        dc.list$data$rank <- rank (dc.list$data$dev)

        df <- as.data.frame (dc.list, display='display', rank='rank')
        df$out <- NULL

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


    ## DRAW DATA CUBE
    output$draw.cube <- renderRglwidget ({
        dims <- c()
        if (input$user.selection %in% c('all','some','one') && input$user.normalisation) { dims <- append(dims,'user') }
        if (input$topic.selection %in% c('all','some','one') && input$topic.normalisation) { dims <- append(dims,'topic') }
        if (input$time.selection %in% c('all','some','one') && input$time.normalisation) { dims <- append(dims,'time') }

        draw.cube (dc.agg(), dims=dc()$dim.names, dim.dev=dims)
        scene <- scene3d()
        rgl.close()
        rglwidget (scene)
    })
}
