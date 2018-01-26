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

library ('ggplot2')
library ('scales')
library ('ggrepel')
library ('reshape2')
library ('cowplot')

## Build data.cube from data.frame
as.data.cube <- function (obj, ...) { UseMethod ('as.data.cube') }
as.data.cube.data.frame <- function (df, data.cols=c('obs')) {
    dc <- list()
    class (dc) <- 'data.cube'

    dim.names <- names(df)[! names(df) %in% data.cols]
    dc$dim.nb <- length(dim.names)
    dc$dim.names <- dim.names

    elem.names <- lapply (df[,dim.names], function (dim) { return (unique(dim)) })
    dc$elem.nb <- lapply (elem.names, function (dim) { return (length(dim)) })
    dc$elem.names <- elem.names

    dc$cells <- lapply (dim.names, function (dim) { return (match (df[,dim], dc$elem.names[[dim]])) })
    names(dc$cells) <- dim.names

    dc$data <- as.list (df[,data.cols,drop=FALSE])

    dc$margins <- list()
    dc <- compute.margins (dc)

    return (dc)
}

is.data.cube <- function (obj) { inherits (obj, "data.cube") }

as.data.frame.data.cube <- function (dc, display=NULL, rank=NULL) {
    df <- as.data.frame (cbind (as.data.frame (dc$cells), as.data.frame (dc$data)))

    if (! is.null (display))
    {
        df <- df[df[[display]],]
        df[[display]] <- NULL
    }
    
    if (! is.null (rank))
    {
        df <- df[order(df[[rank]]),]
        df[[rank]] <- NULL
    }

    dummy <- lapply (dc$dim.names, function (dim) { return (df[[dim]] <<- dc$elem.names[[dim]][df[[dim]]]) })

    return (df)
}

## Compute margins
compute.margins <- function (obj, ...) { UseMethod ('compute.margins') }
compute.margins.data.cube <- function (dc, dims=dc$dim.names) {
    dc$data.sum <- lapply (dc$data, function (dat) { return (sum(dat)) })
    
    for (dim in dims) {
        agg <- aggregate (dc$data, by=dc$cells[dim], FUN=sum)
        dc$margins[[dim]]$cells[[dim]] <- agg[[dim]]
        dc$margins[[dim]]$data <- as.list (agg[,names(agg) != dim, drop=FALSE])
        dc$elem.nb[[dim]] <- length (dc$elem.names[[dim]])
    }

    return (dc)
}


## Remove elems
remove.elems <- function (obj, ...) { UseMethod ('remove.elems') }
remove.elems.data.cube <- function (dc, elems) {
    for (d in seq_along (elems)) {
        rem.elems <- elems[[d]]
        dim <- names(elems)[[d]]
        
        new.elems <- c()
        new.indices <- c()
        
        current.index <- 1
        for (index in seq_along (dc$elem.names[[dim]])) {
            if (dc$elem.names[[dim]][index] %in% rem.elems) { new.indices[index] <- NA }
            else {
                new.elems[current.index] <- dc$elem.names[[dim]][index]
                new.indices[index] <- current.index
                current.index <- current.index + 1
            }
        }

        dc$elem.names[[dim]] <- new.elems
        dc$cells[[dim]] <- new.indices[dc$cells[[dim]]]

        dc$margins[[dim]]$cells[[dim]] <- new.indices[dc$margins[[dim]]$cells[[dim]]]
        dc$margins[[dim]]$data <- lapply (dc$margins[[dim]]$data, function (data) { return (data [! is.na (dc$margins[[dim]]$cells[[dim]])]) })
        dc$margins[[dim]]$cells[[dim]] <- dc$margins[[dim]]$cells[[dim]] [! is.na (dc$margins[[dim]]$cells[[dim]])]
    }

    ## TODO: successive remove in the loop? (as function below)
    kept.cells <- !apply (as.data.frame (lapply (dc$cells, function (dim) { return (is.na(dim)) } )), 1, FUN=any)

    dc$cells <- lapply (dc$cells, function (dim) {return (as.integer (dim[kept.cells])) })
    dc$data <- lapply (dc$data, function (data) {return (data[kept.cells]) })
    ##dc$elem.nb <- lapply (dc$elem.names, function (dim) { return (length(dim)) })
    
    return (dc)
}


## Select elems
select.elems <- function (obj, ...) { UseMethod ('select.elems') }
select.elems.data.cube <- function (dc, elems) {
    ## TODO: improve
    remove <- list()
    for (d in seq_along (elems)) {
        dim <- names(elems)[[d]]
        remove[[dim]] <- dc$elem.names[[dim]][! dc$elem.names[[dim]] %in% elems[[d]]]
    }
    return (remove.elems (dc, remove))
}


## Aggregate elems
aggregate.elems <- function (obj, ...) { UseMethod ('aggregate.elems') }
aggregate.elems.data.cube <- function (dc, elems) {
    for (d in seq_along (elems)) {
        agg.elems <- elems[[d]]
        dim <- names(elems)[[d]]

        new.elems <- names (agg.elems)
        old.indices <- lapply (agg.elems, function (a) match (a, dc$elem.names[[dim]]))
        agg.indices <- unname (unlist (lapply (old.indices, function (indices) min (indices))))

        new.indices <- seq_along (dc$elem.names[[dim]])

        for (a in seq_along (agg.elems)) {
            new.indices[old.indices[[a]]] <- agg.indices[a]
            dc$elem.names[[dim]][old.indices[[a]]] <- NA
            dc$elem.names[[dim]][agg.indices[a]] <- new.elems[a]
        }

        unique_old.indices <- unique (new.indices)
        unique_old.indices <- unique_old.indices[order(unique_old.indices)]
        new.indices <- match (new.indices, unique_old.indices)
        
        dc$elem.names[[dim]] <- dc$elem.names[[dim]][! is.na (dc$elem.names[[dim]])]
        dc$cells[[dim]] <- new.indices[dc$cells[[dim]]]

        dc$margins[[dim]]$cells[[dim]] <- new.indices[dc$margins[[dim]]$cells[[dim]]]
        dc$margins[[dim]]$data <- as.list (aggregate (dc$margins[[dim]]$data, by=dc$margins[[dim]]$cells, FUN=sum))
    }

    agg <- aggregate (dc$data, by=dc$cells, FUN=sum)
    dc$cells <- agg [dc$dim.names]
    dc$data <- agg [names (dc$data)]
    dc$elem.nb <- lapply (dc$elem.names, function (dim) { return (length(dim)) })

    return (dc)
}


## Remove dims
remove.dims <- function (obj, ...) { UseMethod ('remove.dims') }
remove.dims.data.cube <- function (dc, dims) {
    for (d in seq_along (dims)) {
        dim <- dims[[d]]

        dc$elem.nb[[dim]] <- NULL
        dc$elem.names[[dim]] <- NULL
        dc$cells[[dim]] <- NULL
        dc$margins[[dim]] <- NULL
    }

    dc$dim.names <- dc$dim.names [! dc$dim.names %in% dims]
    dc$dim.nb <- length (dc$dim.names)

    if (dc$dim.nb > 0 && length (dc$cells[[1]]) > 0) {
        agg <- aggregate (dc$data, by=dc$cells, FUN=sum)
        dc$cells <- as.list (agg [dc$dim.names])
        dc$data <- as.list (agg [names (dc$data)])
    } else { for (dat in names (dc$data)) { dc$data[[dat]] <- NULL } }
    
    return (dc)
}


## Select dims
select.dims <- function (obj, ...) { UseMethod ('select.dims') }
select.dims.data.cube <- function (dc, dims) {
    rem.dims <- dc$dim.names [! dc$dim.names %in% dims]    
    return (remove.dims (dc, rem.dims))
}


## Compute expected value
compute.expected <- function (obj, ...) { UseMethod ('compute.expected') }
compute.expected.data.cube <- function (dc, input='obs', output='exp', dims=c()) {
    other.dims <- dc$dim.names[! dc$dim.names %in% dims]

    ## TODO: simplify prod computation
    if (length (dims) > 0 && length (other.dims) > 0) {
        dc$data[[output]] <- apply (as.data.frame (cbind (
            sapply (dims, function (dim) { return (dc$margins[[dim]]$data[[input]] [match (dc$cells[[dim]], dc$margins[[dim]]$cells[[dim]])]) }),
            sapply (as.list (other.dims), function (dim) { return (rep (dc$data.sum[[input]] / dc$elem.nb[[dim]], length (dc$data[[input]]))) })
        )), 1, prod) / dc$data.sum[[input]]^(dc$dim.nb-1)
    }

    else if (length (dims) > 0) {
        dc$data[[output]] <- apply (as.data.frame (
            sapply (dims, function (dim) { return (dc$margins[[dim]]$data[[input]] [match (dc$cells[[dim]], dc$margins[[dim]]$cells[[dim]])]) })
        ), 1, prod) / dc$data.sum[[input]]^(dc$dim.nb-1)
    }

    else {
        dc$data[[output]] <- apply (as.data.frame (
            sapply (as.list (other.dims), function (dim) { return (rep (dc$data.sum[[input]] / dc$elem.nb[[dim]], length (dc$data[[input]]))) })
        ), 1, prod) / dc$data.sum[[input]]^(dc$dim.nb-1)
    }

    return (dc)
}


## Compute divergence
compute.deviated <- function (obj, ...) { UseMethod ('compute.deviated') }
compute.deviated.data.cube <- function (dc, input='obs', model='exp', output='dev', type='KLdiv', sep.dim=NULL) {
    sum <- dc$data.sum[[input]]
    ##if (is.null (sep.dim)) { sum <- dc$data.sum[[input]] }
    ##else { sum <- dc$margins[[sep.dim]]$obs [match (dc$data[[sep.dim]], dc$margins[[sep.dim]]$dim)] }

    if (type == 'KLdiv') { dc$data[[output]] <- dc$data[[input]] / sum * log2 (dc$data[[input]] / dc$data[[model]]) }
    else if (type == 'poisson') {
        dc$data[[output]] <- ifelse (dc$data[[input]] < dc$data[[model]],
                                     ppois (dc$data[[input]], dc$data[[model]], lower.tail=TRUE, log.p=TRUE),
                                     -ppois (dc$data[[input]], dc$data[[model]], lower.tail=FALSE, log.p=TRUE)
                                     )
        if (length (dc$data[[output]]) == 0) { dc$data[[output]] <- numeric(0) }
    }

    return (dc)
}


## Compute outliers
compute.outliers <- function (obj, ...) { UseMethod ('compute.outliers') }
compute.outliers.data.cube <- function (dc, deviation='dev', outlier='out', threshold=3) {
    dev.mean <- mean (dc$data[[deviation]])
    dev.sd <- sd (dc$data[[deviation]])

    if (is.na (dev.sd)) { dc$data[[outlier]] <- rep (0, length(dc$data[[deviation]])) }
    else { dc$data[[outlier]] <- findInterval (dc$data[[deviation]], dev.mean + dev.sd * threshold * c(-1,1)) - 1 }

    return (dc)
}

    
## Print data summary
data.summary <- function (obj, ...) { UseMethod ('data.summary') }
data.summary.data.cube <- function (dc, data='obs') {
    summary (dc$data[[data]])
}


## Plot data distribution
data.distribution <- function (obj, ...) { UseMethod ('data.distribution') }
data.distribution.data.cube <- function (dc, data='obs', log='', threshold=NULL) {
    p <- ggplot (as.data.frame (dc$data), aes (x=get(data))) +
        geom_histogram (bins=100, aes (y=..count..), color="black", fill="blue", alpha=0.3) +
        ## stat_density (aes (y=..count..), color="black", fill="blue", alpha=0.3) +
        theme_bw () #+ theme (text=elem_text (size=20))

    title <- paste ('Distribution of', data)
    
    if (log == "x") {
        logmax <- ceiling(log10(max(dc$data[[data]])))
        breaks <- 10 ^ seq (0, logmax, logmax/10)
        title <- paste (title, '(logarithmic scale)')

        p <- p + scale_x_continuous (trans="log", breaks=breaks) +
            scale_y_continuous (breaks=scales::pretty_breaks (n=10))
    }

    else if (log == "y") {
        title <- paste (title, '(logarithmic scale)')

        p <- p + scale_y_continuous (trans="log")
    }
    
    else if (log == "xy") {
        logmax <- ceiling(log10(max(dc$data[[data]])))
        breaks <- 10 ^ seq (0, logmax, logmax/10)
        title <- paste (title, '(logarithmic scales)')
        
        p <- p + scale_x_continuous (trans="log", breaks=breaks) +
            scale_y_continuous (trans="log")
    }

    if (! is.null (threshold)) {
        data.mean <- mean (dc$data[[data]])
        data.sd <- sd (dc$data[[data]])

        p <- p + geom_vline (xintercept=data.mean + threshold * data.sd, size=2, color='red') +
            geom_vline (xintercept=data.mean - threshold * data.sd, size=2, color='red')
    }
    
    p <- p + labs (title=title) +
        xlab (data) + ylab ('count')

    return (p)
}


## Plot data
plot.data <- function (obj, ...) { UseMethod ('plot.data') }
plot.data.data.cube <- function (dc, data='obs', rank=NULL, display=NULL, sep.dim=NULL) {
    df <- as.data.frame (cbind (as.data.frame (dc$cells), as.data.frame (dc$data)))

    if (! is.null (display)) { df <- df[df[[display]],] }
    if (nrow(df) == 0) { return (NULL); }
    
    if (! is.null (rank)) { df <- df[order(df[[rank]]),] }

    dummy <- lapply (dc$dim.names, function (dim) { return (df[[dim]] <<- dc$elem.names[[dim]][df[[dim]]]) })
    
    ##df <- df [do.call ('order', data[,dc$dim.names,drop=FALSE]),]

    ratio.index <- regexpr ('/', data)

    if (ratio.index != -1) {
        data1 <- substring (data, 1, ratio.index-1)
        data2 <- substring (data, ratio.index+1, nchar(data))
        df$data <- df[[data1]] / df[[data2]] - 1
    }
    else { df$data <- df[[data]] }

    if (is.null (sep.dim)) {
        df$label <- apply (df, 1, function (row) { return (paste (row[dc$dim.names], collapse=', ')) })
        df$label <- factor (df$label, levels=unique(df$label))

        p <- ggplot (df, aes (x=label, y=data)) +
            geom_bar (stat="identity")
    }

    else {
        dim.names <- dc$dim.names[dc$dim.names != sep.dim]
        df$label <- apply (df, 1, function (row) { return (paste (row[dim.names], collapse=', ')) })
        df$label <- factor (df$label, levels=unique(df$label))

        p <- ggplot (df, aes(x=label, y=data)) +
            geom_bar (aes (fill=get(sep.dim)), position="dodge", stat="identity")
    }

    if (ratio.index != -1) { p <- p + scale_y_continuous (labels=function (x) x+1) }

    p <- p +
        ylab (data) +
        theme_bw () +
        theme (axis.text.x = element_text (angle = 90, hjust = 1)) #+
    ##theme (text=elem_text (size=20))
        
    return (p)
}


## Plot outliers
plot.outliers <- function (obj, ...) { UseMethod ('plot.outliers') }
plot.outliers.data.cube <- function (dc, input='obs', model='exp', deviation='dev', outlier='out', display=NULL, labels=FALSE) {
    df <- as.data.frame (cbind (as.data.frame (dc$cells), as.data.frame (dc$data)))

    if (! is.null (display)) { df <- df[df[[display]],] }
    if (nrow(df) == 0) { return (NULL); }

    dummy <- lapply (dc$dim.names, function (dim) { return (df[[dim]] <<- dc$elem.names[[dim]][df[[dim]]]) })
    df$label <- apply (df, 1, function (row) paste (row[dc$dim.names], collapse=' '))

    df$ratio <- df$obs / df$exp
    df$type <- ifelse (df[[outlier]] == 0, 'normal', 'abnormal')

    shape.values <- c(22,21)
    types <- unique (df$type)
    if (length(types) == 1) {
        if (types == 'normal') { shape.values <- c(21) }
        else if (types == 'abnormal') { shape.values <- c(22) }
    }
        
    p <- ggplot (data=df, aes (x=get(input), y=ratio)) +
        scale_x_log10 () + scale_y_log10 () +
        geom_point (aes (size=abs(dev), fill=dev, shape=type)) +
        scale_shape_manual (values=shape.values) +
        scale_fill_gradient2 (low='blue', high='red') +
        labs (title='Outliers') + xlab (input) + ylab (paste (input, '/', model)) +
        theme_bw () ##+ theme (text=elem_text (size=20))

    if (labels) { p <- p + geom_text_repel (data=df[df[[outlier]] != 0,], aes (x=get(input), y=ratio, label=label)) }
    
    return (p)
}


## List outliers
list.outliers <- function (obj, ...) { UseMethod ('list.outliers') }
list.outliers.data.cube <- function (dc, input='obs', model='exp', deviation='dev', outlier='out') {
    
    df <- as.data.frame (cbind (as.data.frame (dc$cells), as.data.frame (dc$data)))
    if (! is.null (display)) { df <- df[df[[display]],] }
    
    df$ratio <- df$obs / df$exp
    df$type <- ifelse (df[[outlier]] == 0, 'normal', 'abnormal')
    
    dummy <- lapply (dc$dim.names, function (dim) { return (df[[dim]] <<- dc$elem.names[[dim]][df[[dim]]]) })
    df$label <- apply (df, 1, function (row) paste (row[dc$dim.names], collapse=' '))

    p <- ggplot (data=df, aes (x=get(input), y=ratio)) +
        scale_x_log10 () + scale_y_log10 () +
        geom_point (aes (size=abs(dev), fill=dev, shape=type)) +
        scale_shape_manual (values=c(22,21)) +
        scale_fill_gradient2 (low='blue', high='red') +
        labs (title='Outliers') + xlab (input) + ylab (paste (input, '/', model)) +
        theme_bw () ##+ theme (text=elem_text (size=20))

    if (labels) { p <- p + geom_text_repel (data=df[df[[outlier]] != 0,], aes (x=get(input), y=ratio, label=label)) }
    
    return (p)
}


