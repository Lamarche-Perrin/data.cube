library ('rjson')
library ('magrittr')

source ('../data.cube.R')

## dataset <- 'guardian.2016'
## select <- '[{"dim":"time","select":"all"},{"dim":"topic","select":"some","head":5},{"dim":"user","select":"some","list":["15131989"]}]'
## normalise <- '["topic","time"]'
## stat.test <- '{"type":"poisson","threshold":3}'

#' Compute the mean of
#' @param dataset Dataset to be explored
#' @param select Dimensions to be selected
#' @param normalise Dimensions to use for normalisation
#' @param stat.test Parameter of the statistical test
#' @get /outliers
outliers <- function (dataset, select='[]', normalise='[]', stat.test='{"type":"poisson","threshold":3}'){

    # Load dataset
    dc <- paste ('../data/', dataset, '.csv', sep='') %>%
        read.csv (stringsAsFactors=FALSE) %>%
        as.data.cube ()

    # Select dimensions
    select.dim <- fromJSON (select)

    elems <- list()
    for (dim in select.dim) {
        if (dim$select == 'some') {
            if (! is.null (dim$list)) { elems[[dim$dim]] <- dim$list }
            if (! is.null (dim$head)) { elems[[dim$dim]] <- dc$elem.names[[dim$dim]][dc$margins[[dim$dim]]$cells[[dim$dim]][order(-dc$margins[[dim$dim]]$data$obs)[1:dim$head]]] }
        }
    }
    if (length (elems) > 0) { dc <- select.elems (dc, elems) }

    dims <- c()
    for (dim in select.dim) { dims <- append (dims, dim$dim) }
    dims <- dc$dim.names [! dc$dim.names %in% dims]
    if (length (dims) > 0) { dc <- remove.dims (dc, dims) }

    # Normalise data
    normalise.dim <- fromJSON (normalise)
    dc <- compute.expected (dc, dims=normalise.dim)

    stat.test <- fromJSON (stat.test)
    dc <- compute.deviated (dc, type=stat.test$type)
    dc <- compute.outliers (dc, threshold=stat.test$threshold)
    #dc$data$display <- dc$data$obs >= min.obs

    dc$data$display <- (dc$data$out == 1)
    dc$data$rank <- rank (-dc$data$dev)

    df <- as.data.frame (dc, display='display', rank='rank')
    df$out <- NULL

    return (df)
}


    ## ## DATA STRUCTURE
    ## output$data.structure <- renderText ({
    ##     if (is.null (dataset)) { return (NULL) }
    ##     return (paste (capture.output (str (dc.out())), '\n'))
    ## })

    ## ## DATA PLOT
    ## data.plot <- reactive ({
    ##     if (is.null (dataset)) { return (NULL) }
    ##     if (dc.out()$dim.nb == 0 || length (dc.out()$data$obs) == 0) { return (NULL) }
        
    ##     dc.plot <- dc.out()
        
    ##     rank <- NULL
    ##     if (time.selection %in% c('one','none')) {
    ##         dc.plot$data$rank <- rank(-dc.plot$data$obs)
    ##         rank <- 'rank'
    ##     }

    ##     dims <- c()
    ##     if (! user.selection %in% c('one','none')) { dims <- append (dims, 'user') }
    ##     if (! topic.selection %in% c('one','none')) { dims <- append (dims, 'topic') }
    ##     if (! time.selection %in% c('one','none')) { dims <- append (dims, 'time') }

    ##     sep.dim <- NULL
    ##     if (length (dims) == 2) {
    ##         if (length (dc.plot$elem.names[[dims[1]]]) > length (dc.plot$elem.names[[dims[2]]])) { sep.dim <- dims[2] } else { sep.dim <- dims[1] }
    ##     }

    ##     data <- 'obs'
    ##     if (user.selection %in% c('all','some','one') && user.normalisation || topic.selection %in% c('all','some','one') && topic.normalisation || time.selection %in% c('all','some','one') && time.normalisation) { data <- 'obs/exp' }

    ##     plot <- plot.data (dc.plot, data=data, rank=rank, display='display', sep.dim=sep.dim) +
    ##         theme (text=element_text (size=20))

    ##     if (! is.null (sep.dim)) { dims <- dims [dims != sep.dim] }
    ##     xlab.str <- paste (sapply (dims, function (dim) { return (dim.str (dim, TRUE)) }), collapse=' x ')
    ##     plot <- plot + xlab (xlab.str)
        
    ##     if (data == 'obs') { plot <- plot + ylab ('Number of comments') }
    ##     else if (data == 'obs/exp') { plot <- plot + ylab ('Ratio of observed comments vs. expected comments') }

    ##     if (! is.null (sep.dim)) {
    ##         fill.str <- dim.str (sep.dim, TRUE)
    ##         plot <- plot + guides (fill=guide_legend(title=fill.str))
    ##     }
        
    ##     return (plot)
    ## })

    ## output$data.plot <- renderPlot ({ data.plot() }, height=900)
    
    ## output$download.data.plot.pdf <- downloadHandler (
    ##     filename = function () { "data.plot.pdf" },
    ##     content = function (filename) { ggsave (filename, plot=data.plot(), device='pdf') } 
    ## )

    ## output$download.data.plot.png <- downloadHandler (
    ##     filename = function () { "data.plot.png" },
    ##     content = function (filename) { ggsave (filename, plot=data.plot(), device='png') } 
    ## )

    
    ## ## OUTLIER PLOT
    ## outlier.plot <- reactive ({
    ##     if (is.null (dataset)) { return (NULL) }
    ##     if (dc.out()$dim.nb == 0 || length (dc.out()$data$obs) == 0) { return (NULL) }

    ##     labels <- outlier.labels && sum (dc.out()$data$out != 0) <= 100
    ##     plot <- plot.outliers (dc.out(), display='display', labels=labels) +
    ##         theme (text=element_text (size=20))

    ##     plot <- plot + ggtitle ('') +
    ##         xlab ('Number of comments') +
    ##         ylab ('Ratio of observed comments vs. expected comments') +
    ##         guides (
    ##             fill = guide_colourbar (title='Deviation', order=1),
    ##             size = guide_legend (title='Absolute\ndeviation', order=2),
    ##             shape = guide_legend (title='Outliers', order=3)
    ##         )

    ##     return (plot)
    ## })
    
    ## output$outlier.plot <- renderPlot ({ outlier.plot() }, height=900)
    
    ## output$download.outlier.plot.pdf <- downloadHandler (
    ##     filename = function () { "outlier.plot.pdf" },
    ##     content = function (filename) { ggsave (filename, plot=outlier.plot(), device='pdf') } 
    ## )

    ## output$download.outlier.plot.png <- downloadHandler (
    ##     filename = function () { "outlier.plot.png" },
    ##     content = function (filename) { ggsave (filename, plot=outlier.plot(), device='png') } 
    ## )


    ## ## DISTRIBUTION PLOT
    ## output$distribution.plot <- renderPlot ({
    ##     if (is.null (dataset)) { return (NULL) }
    ##     if (dc.dev2()$dim.nb == 0 || length (dc.dev2()$data$obs) == 0) { return (NULL) }

    ##     plot <- data.distribution (dc.dev2(), data='dev', threshold=outlier.threshold) +
    ##         theme (text=element_text (size=20))

    ##     plot <- plot + ggtitle ('') + xlab ('Deviation') + ylab ('Number of observations')

    ##     return (plot)
    ## }, height=450)


    ## ## OUTLIER LISTS
    ## positive.outlier.list <- reactive ({
    ##     if (is.null (dataset)) { return (NULL) }
    ##     if (dc.out()$dim.nb == 0) { return (NULL) }

    ##     dc <- dc.out()
    ##     dc$data$display <- (dc$data$out == 1)
    ##     dc$data$rank <- rank (-dc$data$dev)

    ##     df <- as.data.frame (dc, display='display', rank='rank')
    ##     df$out <- NULL

    ##     return (df)
    ## })
    
    ## output$positive.outlier.list <- renderDataTable ({ positive.outlier.list() })

    ## output$download.positive.outlier.list.csv <- downloadHandler (
    ##     filename = function () { "positive.outlier.list.csv" },
    ##     content = function (filename) { write.csv (positive.outlier.list(), filename, row.names=FALSE) } 
    ## )

    ## output$download.positive.outlier.list.json <- downloadHandler (
    ##     filename = function () { "positive.outlier.list.json" },
    ##     content = function (filename) { write (toJSON (unname (split (positive.outlier.list(), 1:nrow(positive.outlier.list())))), filename) }
    ## )


    ## negative.outlier.list <- reactive ({
    ##     if (is.null (dataset)) { return (NULL) }
    ##     if (dc.out()$dim.nb == 0) { return (NULL) }

    ##     dc <- dc.out()
    ##     dc$data$display <- (dc$data$out == -1)
    ##     dc$data$rank <- rank (dc$data$dev)

    ##     df <- as.data.frame (dc, display='display', rank='rank')
    ##     df$out <- NULL

    ##     return (df)
    ## })

    ## output$negative.outlier.list <- renderDataTable ({ negative.outlier.list() })

    ## output$download.negative.outlier.list.csv <- downloadHandler (
    ##     filename = function () { "negative.outlier.list.csv" },
    ##     content = function (filename) { write.csv (negative.outlier.list(), filename, row.names=FALSE) } 
    ##     )

    ## output$download.negative.outlier.list.json <- downloadHandler (
    ##     filename = function () { "negative.outlier.list.json" },
    ##     content = function (filename) { write (toJSON (unname (split (negative.outlier.list(), 1:nrow(negative.outlier.list())))), filename) }
    ## )


    ## ## DRAW DATA CUBE
    ## output$draw.cube <- renderRglwidget ({
    ##     dims <- c()
    ##     if (user.selection %in% c('all','some','one') && user.normalisation) { dims <- append(dims,'user') }
    ##     if (topic.selection %in% c('all','some','one') && topic.normalisation) { dims <- append(dims,'topic') }
    ##     if (time.selection %in% c('all','some','one') && time.normalisation) { dims <- append(dims,'time') }

    ##     draw.cube (dc(), dims=dc()$dim.names, dim.dev=dims)
    ##     scene <- scene3d()
    ##     rgl.close()
    ##     rglwidget (scene)
    ## })
