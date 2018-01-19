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

source ('data.cube.R')

function (input, output) {
    file <- 'data/guardian.small.csv'
    
    df <- read.csv (file, stringsAsFactors=FALSE)
    dc <- as.data.cube (df)

    ## Dimension selection
    dim.buttons <- function (dim, label=dim) {
        choices <- c('all', 'some', 'one', 'none')
        names(choices) <- c(paste('All (', dc$elem.nb[[dim]], ')', sep=''), 'Some', 'One', 'None (aggregate)')
        return (renderUI ({ radioButtons (paste(dim, '.selection', sep=''), label=h4(label), inline=TRUE,
                                          choices=choices, selected='none')
        }))
    }
    
    output$user.buttons <- dim.buttons ('user', 'Users')
    output$topic.buttons <- dim.buttons ('topic', 'Topics')
    output$time.buttons <- dim.buttons ('time', 'Dates')
    
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

    ## Sever computation
    dc.agg <- reactive ({
        dc.agg <- dc

        elems <- list()
        if (input$user.selection == 'one') { elems[['user']] <- c(input$selected.user) }
        if (input$topic.selection == 'one') { elems[['topic']] <- c(input$selected.topic) }
        if (input$time.selection == 'one') { elems[['time']] <- c(input$selected.time) }
        if (length(elems) > 0) { dc.agg <- select.elems (dc.agg, elems) }

        elems <- list()
        if (input$user.selection == 'some') { elems[['user']] <- dc$elem.names$user[dc$margins$user$cells$user[order (-dc$margins$user$data$obs)[1:input$user.number]]] }
        if (input$topic.selection == 'some') { elems[['topic']] <- dc$elem.names$topic[dc$margins$topic$cells$topic[order (-dc$margins$topic$data$obs)[1:input$topic.number]]] }
        if (input$time.selection == 'some') { elems[['time']] <- dc$elem.names$time[dc$margins$time$cells$time[order (-dc$margins$time$data$obs)[1:input$time.number]]] }
        if (length(elems) > 0) { dc.agg <- select.elems (dc.agg, elems) }

        dims <- c()
        if (input$user.selection == 'none') { dims <- append(dims,'user') }
        if (input$topic.selection == 'none') { dims <- append(dims,'topic') }
        if (input$time.selection == 'none') { dims <- append(dims,'time') }
        if (length(dims) > 0) { dc.agg <- remove.dims (dc.agg, dims) }

        ## dc.agg <- compute.margins (dc.agg)
        
        return (dc.agg)
    })

    dc.dev1 <- reactive ({
        dims <- c()
        if (input$user.selection %in% c('all','some','one') && input$user.normalisation) { dims <- append(dims,'user') }
        if (input$topic.selection %in% c('all','some','one') && input$topic.normalisation) { dims <- append(dims,'topic') }
        if (input$time.selection %in% c('all','some','one') && input$time.normalisation) { dims <- append(dims,'time') }

        return (compute.expected (dc.agg(), dims=dims))
    })

    dc.dev2 <- reactive ({
        return (compute.deviated (dc.dev1(), type=input$deviation.type))
    })

    dc.dev3 <- reactive ({
        return (compute.outliers (dc.dev2(), threshold=input$outlier.threshold))
    })

    dc.out <- reactive ({
        dc.out <- dc.dev3()
        dc.out$data$display <- (dc.out$data$obs >= input$min.obs)
        return (dc.out)
    })

    output$data.structure <- renderText ({
        return (paste (capture.output (str (dc.out())), '\n'))
    })

    output$data.plot <- renderPlot ({
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
        if (input$user.normalisation || input$topic.normalisation || input$time.normalisation) { data <- 'obs/exp' }
        
        return (plot.data (dc.plot, data=data, rank=rank, display='display', sep.dim=sep.dim))
    })
    
    output$outlier.plot <- renderPlot ({
        return (plot.outliers (dc.out(), display='display', labels=input$outlier.labels))
    })

    output$distribution.plot <- renderPlot ({
        ##data.nb <- sum (abs (dc.dev$data$dev) < 1)
        return (data.distribution (dc.dev2(), data='dev', threshold=input$outlier.threshold)) #+
        ##coord_cartesian (ylim = c(0, data.nb / 300))
    })
}
