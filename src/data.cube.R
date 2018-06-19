                                        # This file is part of data.cube.
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


#' @title Easy Processing of Multidimensional Data.
#'
#' @description
#' \code{data.cube} is an R package for the exploration of multidimensional
#' datasets and for the detection of statistical outliers within. It is
#' mainly a tool for data exploration, allowing to have a first glance at
#' it and to formulate research hypotheses to be later tested.
#'
#' The package defines a new data structure called data.cube that can be
#' fed with a classical \code{data.frame} encoding a list of numeric
#' variables described according to several categorical dimensions. For
#' example, in the case of Twitter data, it can be the number of tweets
#' (numeric variable) that have been published by a given user (first
#' dimension) about a given topic (second dimension) at a given date (third
#' dimension). The input data.frame hence takes the form of a list of
#' quadruplets (user, topic, date, number of tweets).
#'
#' Statistical outliers can then be identified among the observations by
#' first selecting some dimensions of interest, that is by subsetting or
#' by aggregating the input dimensions. If needed, observations can also be
#' normalised according to the marginal values along the selected
#' dimensions, thus comparing the observed value to an expected value
#' obtained by the uniform redistribution of the selected marginal values.
#' Different statistical tests can then be chosen to measure the deviation
#' between the observed and the expected values. The package finally allows
#' to retrieve a list of positive outliers, that is observations that are
#' significantly higher than expected.
#'
#' Note that the current implementation is optimised for sparse data.
#'
#' This package has been developed by researchers of the Complex Networks
#' team, within the Computer Science Laboratory of Paris 6 (LIP6), for the
#' ODYCCEUS project, founded by the European Commission FETPROACT 2016-2017
#' program under grant 732942.
#'
#' Links:
#' \itemize{
#' \item Complex Networks team: \url{http://www.complexnetworks.fr/}
#' \item LIP6: \url{https://www.lip6.fr/}
#' \item ODYCCEUS project: \url{https://www.odycceus.eu/}
#' }
#'
#' Contact:
#' \itemize{
#' \item Robin Lamarche-Perrin: \email{Robin.Lamarche-Perrin@@lip6.fr}
#'
#' See also my webpage:
#' \url{https://www-complexnetworks.lip6.fr/~lamarche/}
#' }
#'
#' List of main collaborators:
#' \itemize{
#' \item Robin Lamarche-Perrin (CNRS, ISC-PIF, LIP6)
#' \item Audrey Wilmet (UPMC, LIP6)
#' \item Léonard Panichi (UPMC, LIP6)
#' }
#'
#' Copyright 2018 © Robin Lamarche-Perrin
#'
#' \code{data.cube} is free software: you can redistribute it and/or modify
#' it under the terms of the GNU General Public License as published by the
#' Free Software Foundation, either version 3 of the License, or (at your
#' option) any later version. It is distributed in the hope that it will be
#' useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#' Public License for more details. You should have received a copy of the
#' GNU General Public License along with this program. If not, see
#' \url{http://www.gnu.org/licenses/}.
#'
#' @docType package
#'
#' @name data.cube
#'
#' @import tidyverse reshape2 ggrepel cowplot data.table scales cowplot
NULL


library ("tidyverse")
library ("ggrepel")
## library ("scales")
## library ("reshape2")
## library ("cowplot")
## library ("rgl")
## library ("data.table")


as.data.cube_ <- function (obj, ...) {
    UseMethod ("as.data.cube_")
}
as.data.cube_.data.frame <-
    function (df, dim = names(df) [!names(df) %in% var], var) {
        if (is.null (names (dim)))
            names (dim) <- dim
        else
            names (dim) <-
                ifelse (names (dim) == "", unlist (dim), names (dim))
        dim <- unlist (dim)
        
        if (is.null (names (var)))
            names (var) <- var
        else
            names (var) <-
                ifelse (names (var) == "", unlist (var), names (var))
        var <- unlist (var)
        
        ## TODO: aggregate data.frame if needed
        ## TODO: suppress null observations
        
        ## Build data.cuve
        dc <- list()
        class (dc) <- "data.cube"
        
        dc$dim.nb <- length (dim)
        dc$dim.names <- names (dim)
        
        elm.names <- lapply (df[, dim], function (d)
            unique (d))
        dc$elm.nb <- lapply (elm.names, function (d)
            length (d))
        dc$elm.names <- elm.names
        
        dc$var.nb <- length (var)
        dc$var.names <- names (var)
        
        ## Fill observations
        dc$obs <- list()
        dc.name <- paste (dc$dim.names, collapse = ".")
        
        dc$obs[[dc.name]] <- list()
        dc$obs[[dc.name]]$elms <-
            lapply (dim, function (d) {
                return (match (df[, d], dc$elm.names[[d]]))
            })
        
        dc$obs[[dc.name]]$vars <- as.list (df[, var, drop = FALSE])
        
        names (dc$elm.nb) <- dc$dim.names
        names (dc$elm.names) <- dc$dim.names
        names (dc$obs[[dc.name]]$elms) <- dc$dim.names
        names (dc$obs[[dc.name]]$vars) <- dc$var.names
        
        ## Compute margins
        dc <- compute.margin_(dc)
        for (d in dc$dim.names)
            dc <- compute.margin_(dc, d)
        
        return (dc)
    }

as.data.cube <- function (obj, ...) {
    UseMethod ("as.data.cube")
}

#' @title Transform a \code{data.frame} into a \code{data.cube}.
#'
#' @description
#' \code{as.data.cube()} transforms a \code{data.frame} into a
#' \code{data.cube} by interpreting its rows as observations and its
#' columns either as categorical descriptions of the observations (elements within
#' \code{data.cube} dimensions) or as numerical descriptions of the observations (values taken by
#' the \code{data.cube} variables).
#'
#' @param df A \code{data.frame} to be transformed into a \code{data.cube}.
#'
#' @param dim A list of unquoted expressions giving the columns of \code{df} to be used as
#' dimensions for the \code{data.cube}. If names are provided, they are
#' interpreted as names for the corresponding dimensions.
#'
#' @param var A list of unquoted expressions giving the columns of \code{df} to be used as
#' variables for the \code{data.cube}. If names are provided, they are
#' interpreted as names for the corresponding variables.
#'
#' @return A \code{data.cube} resulting from the transformation of \code{df}.
#'
#' @method as.data.cube data.frame
#' @export
as.data.cube.data.frame <- function (df, dim, var) {
    dim <- lapply (substitute (dim), deparse)
    dim <- unlist (dim) [2:length (dim)] ## TODO: if only one dim
    
    var <- lapply (substitute (var), deparse)
    var <- unlist (var) [2:length (var)] ## TODO: if only one var
    
    as.data.cube_(df, dim, var)
}


is.data.cube <- function (obj) {
    inherits (obj, "data.cube")
}

as.data.frame_ <-
    function (obj, ...) {
        UseMethod ("as.data.frame_")
    }
as.data.frame_.data.cube <-
    function (dc,
              dim = dc$dim.names,
              var = dc$var.names,
              complete = FALSE) {
        dim <- unlist (dim)
        var <- unlist (var)
        
        dc.name <- paste (dc$dim.names, collapse = ".")
        
        df <-
            as.data.frame (cbind (as.data.frame (dc$obs[[dc.name]]$elms), as.data.frame (dc$obs[[dc.name]]$vars)))
        
        if (complete) {
                                        # TODO: complete wrt dc$elm.names
            df$row <- 1:nrow(df)
            fill <- as.list (sapply (dc$var.names, function (v)
                0))
            fill$row <- nrow(df) + 1
            df <-
                as.data.frame (complete_(df, cols = dc$dim.names, fill = fill))
            df <- df [order (df$row), ]
            df$row <- NULL
        }
        
        df [, append (dim, var)]
        if (length (dim) > 0)
            dummy <-
                lapply (dim, function (d) {
                    return (df[[d]] <<- dc$elm.names[[d]][df[[d]]])
                })
        
        if (length (df) == 1)
            return (df[, 1])
        else
            return (df)
    }



#' @title Transform a \code{data.cube} into a \code{data.frame}.
#'
#' @description
#' \code{as.data.frame()} transforms a \code{data.cube} into a
#' \code{data.frame} by presenting observations in rows, and dimensions and
#' variables in columns.
#'
#' @param dc A \code{data.cube} to be transformed into a \code{data.frame}.
#'
#' @param complete A logical indicating if observations for which
#' variables all equal zero should be presented in the output
#' \code{data.frame}. If not, the output \code{data.frame} hence provides
#' a sparse representation of the data.
#'
#' @return A \code{data.frame} resulting from the transformation of \code{dc}.
#'
#' @method as.data.frame data.cube
#' @export
as.data.frame.data.cube <-
    function (dc, complete = FALSE)
        dc %>% as.data.frame_(complete = complete)

as.data.frame.dim <-
    function (obj, ...) {
        UseMethod ("as.data.frame.dim")
    }
as.data.frame.dim.data.cube <-
    function (dc, complete = FALSE)
        dc %>% as.data.frame_(var = list(), complete = complete)

as.data.frame.var <-
    function (obj, ...) {
        UseMethod ("as.data.frame.var")
    }
as.data.frame.var.data.cube <-
    function (dc, complete = FALSE)
        dc %>% as.data.frame_(dim = list(), complete = complete)


#' @title Get basic properties.
#'
#' @description
#' A collection of functions to get basic properties of a \code{data.cube}.
#'
#' @param dc A \code{data.cube}.
#' @param ... Optional. Names of the dimensions of interest.
#'
#' @name property
#' @export property
NULL

dim.names <- function (obj, ...) {
    UseMethod ("dim.names")
}

#' @return A character vector giving the names of the dimensions of
#' \code{dc}.
#'
#' @rdname property
#' @method dim.names data.cube
dim.names.data.cube <- function (dc)
    dc$dim.names

elm.names_ <- function (obj, ...) {
    UseMethod ("elm.names_")
}
elm.names_.data.cube <- function (dc, dim = dc$dim.names) {
    if (length (dim) == 1)
        return (dc$elm.names[[unlist(dim)]])
    dc$elm.names[dim]
}

elm.names <- function (obj, ...) {
    UseMethod ("elm.names")
}

#' @return A named list of character vectors giving the names of the
#' elements in the dimensions of \code{dc}.
#'
#' @rdname property
#' @method elm.names data.cube
elm.names.data.cube <- function (dc, ...) {
    dim <- sapply (eval (substitute (alist (...))), deparse)
    if (length (dim) == 0)
        dim <- dc$dim.names
    elm.names_(dc, dim)
}

var.names <- function (obj, ...) {
    UseMethod ("var.names")
}

#' @return A character vector giving the names of the variables of
#' \code{dc}.
#'
#' @rdname property
#' @method var.names data.cube
var.names.data.cube <- function (dc)
    dc$var.names

dim.nb <- function (obj, ...) {
    UseMethod ("dim.nb")
}

#' @return An integer giving the number of dimensions of \code{dc}.
#'
#' @rdname property
#' @method dim.nb data.cube
dim.nb.data.cube <- function (dc)
    dc$dim.nb

elm.nb_ <- function (obj, ...) {
    UseMethod ("elm.nb_")
}
elm.nb_.data.cube <- function (dc, dim = dc$dim.names) {
    if (length (dim) == 1)
        return (dc$elm.nb[[unlist(dim)]])
    dc$elm.nb[dim]
}

elm.nb <- function (obj, ...) {
    UseMethod ("elm.nb")
}

#' @return A named list of integers giving the number of elements in the
#' dimensions of \code{dc}.
#'
#' @rdname property
#' @method elm.nb data.cube
elm.nb.data.cube <- function (dc, ...) {
    dim <- sapply (eval (substitute (alist (...))), deparse)
    if (length (dim) == 0)
        dim <- dc$dim.names
    elm.nb_(dc, dim)
}

var.nb <- function (obj, ...) {
    UseMethod ("var.nb")
}

#' @return An integer giving the number of variables of \code{dc}.
#'
#' @rdname property
#' @method var.nb data.cube
var.nb.data.cube <- function (dc)
    dc$var.nb


summary.data.cube <- function (dc) {
    elm.nchar <- 50
    dc.name <- paste (dc$dim.names, collapse = ".")
    
    cat (
        "data.cube of ",
        dc$dim.nb,
        " dimension",
        ifelse (dc$dim.nb > 1, "s", ""),
        " and ",
        dc$var.nb,
        " variable",
        ifelse (dc$var.nb > 1, "s", ""),
        "\n\n",
        sep = ""
    )
    
    for (var in dc$var.names) {
        cat ("-> '", var, "' variable:\n", sep = "")
        cat (" - total: ", dc$obs[["."]]$vars[[var]], "\n", sep = "")
        obs.nb <- length (dc$obs[[dc.name]]$vars[[var]])
        cat (
            " - divided into ",
            obs.nb,
            " (non-null) observation",
            ifelse (obs.nb > 1, "s", ""),
            "\n",
            sep = ""
        )
        elm.nb <- prod (unlist (dc$elm.nb))
        cat (
            " - among ",
            elm.nb,
            " cell",
            ifelse (elm.nb > 1, "s", ""),
            " (density = ",
            format (obs.nb / elm.nb, digits = 3),
            ")\n",
            sep = ""
        )
        print (summary (dc$obs[[dc.name]]$vars[[var]]))
        cat ("\n")
    }
    
    elm.names <- lapply (dc$elm.names, function (d) {
        i <- 1
        str <- paste ("'", d[i], "'", sep = "")
        while (nchar (str) < elm.nchar && i < length (d)) {
            i <- i + 1
            
            str <- paste (str, ", '", d[i], "'", sep = "")
        }
        if (i < length (d))
            str <- paste (str, "...", sep = "")
        return (str)
    })
    
    for (dim in dc$dim.names) {
        cat ("-> '", dim, "' dimension:\n", sep = "")
        elm.nb <- dc$elm.nb[[dim]]
        cat (" - ",
             elm.nb,
             " element",
             ifelse (elm.nb > 1, "s", ""),
             ": ",
             elm.names[[dim]],
             "\n",
             sep = "")
        dp.name <- dim
        for (var in names (dc$obs[[dp.name]]$vars)) {
            cat (" - '", var, "' variable:\n", sep = "")
            print (summary (dc$obs[[dp.name]]$vars[[var]]))
        }
        cat ("\n")
    }
}



compute.margin_ <-
    function (obj, ...) {
        UseMethod ("compute.margin_")
    }
compute.margin_.data.cube <-
    function (dc,
              dim = list(),
              recursive = FALSE) {
        dc.name <- paste (dc$dim.names, collapse = ".")
        
        ## Compute global margin if necessary
        if (length (dim) == 0 || recursive) {
            dc$obs[["."]] <- list()
            dc$obs[["."]]$elms <- list()
            dc$obs[["."]]$vars <-
                lapply (dc$obs[[dc.name]]$vars, function (var)
                    sum (var))
            if (length (dim) == 0)
                return (dc)
        }
        
        ## List recursive margins if necessary
        if (recursive) {
            dim <-
                unlist (sapply (1:length (dim), function (n)
                    combn (dim, n, simplify = FALSE)), recursive = FALSE)
        } else {
            dim <- list (dim)
        }
        
        ## Compute all requested margins (except global)
        for (d in dim) {
            if (length (d) == dc$dim.nb)
                next
            
            d <- dc$dim.names [dc$dim.names %in% d]
            dp.name <- paste (d, collapse = ".")
            agg <-
                aggregate (dc$obs[[dc.name]]$vars, by = dc$obs[[dc.name]]$elms[d], FUN =
                                                                                       sum)
            rank <-
                aggregate (
                    x = seq_along(dc$obs[[dc.name]]$elms[d][[1]]),
                    by = dc$obs[[dc.name]]$elms[d],
                    FUN = min
                )[, "x"]
            
            dc$obs[[dp.name]] <- list ()
            dc$obs[[dp.name]]$elms <-
                as.list (agg[order(rank), d, drop = FALSE])
            dc$obs[[dp.name]]$vars <-
                as.list (agg[order(rank), !names(agg) %in% d, drop = FALSE])
        }
        
        return (dc)
    }

compute.margin <-
    function (obj, ...) {
        UseMethod ("compute.margin")
    }
compute.margin.data.cube <- function (dc, ..., recursive = FALSE) {
    dim <- sapply (eval (substitute (alist (...))), deparse)
    compute.margin_(dc, dim, recursive)
}



update.margin <- function (obj, ...) {
    UseMethod ("update.margin")
}
update.margin.data.cube <- function (dc) {
    dc$elm.nb <- lapply (dc$elm.names, function (d)
        length (d))
    
    dc.name <- paste (dc$dim.names, collapse = ".")
    for (dp.name in names (dc$obs)) {
        if (dp.name == dc.name)
            next
        if (dp.name == ".")
            dc <- compute.margin_(dc)
        else {
            d <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
            dc <- compute.margin_(dc, d)
        }
    }
    return (dc)
}



select.dim_ <- function (obj, ...) {
    UseMethod ("select.dim_")
}
select.dim_.data.cube <- function (dc, dim) {
    ## Compute corresponding data plane
    old.dim <- dc$dim.names [dc$dim.names %in% dim]
    old.dp.name <- paste (old.dim, collapse = ".")
    if (is.null (dc$obs[[old.dp.name]]))
        dc <- dc %>% compute.margin_(dim)
    
    ## Adjust other data cube attributes
    dc$dim.nb <- length (dim)
    dc$dim.names <- dim
    dc$elm.nb <- dc$elm.nb[dim]
    dc$elm.names <- dc$elm.names[dim]
    
    ## Adjust all data planes
    for (dp.name in names (dc$obs)) {
        if (dp.name == ".")
            next
        d <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
        if (!all (d %in% dim))
            dc$obs[[dp.name]] <- NULL
        else {
            ## Adjust elements
            new.dim <- dc$dim.names[dc$dim.names %in% d]
            new.dp.name <- paste (new.dim, collapse = ".")
            names (dc$obs) [names (dc$obs) == dp.name] <-
                               new.dp.name
            dc$obs[[new.dp.name]]$elms <-
                dc$obs[[new.dp.name]]$elms[new.dim]
        }
    }
    
    return (dc)
}

select.dim <- function (obj, ...) {
    UseMethod ("select.dim")
}

#' @title Select dimensions of interest.
#'
#' @description
#' \code{select.dim()} selects a subset of dimensions within the \code{data.cube} by aggregating together the elements of each unselected dimension.
#' This consists in replacing the datacube by one of its marginal dataplane.
#'
#' @param dc A datacube.
#' @param ... A list of unquoted expressions giving the dimensions to select.
#' @return The resulting datacube.
#'
#' @method select.dim data.cube
#' @export
select.dim.data.cube <- function (dc, ...) {
    dim <- sapply (eval (substitute (alist (...))), deparse)
    select.dim_(dc, dim)
}



select.elm.indices_ <-
    function (obj, ...) {
        UseMethod ("select.elm.indices_")
    }
select.elm.indices_.data.cube <-
    function (dc, dim, elm.indices, suppress=FALSE) {
        new.indices <- rep (NA, length (dc$elm.names[[dim]]))
        new.indices [elm.indices] <- seq_along (elm.indices)
        
        dc$elm.names[[dim]] <- dc$elm.names[[dim]] [elm.indices]
        
        for (dp.name in names (dc$obs)) {
            if (dp.name == ".")
                next
            d <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
            if (dim %in% d) {
                keep <- dc$obs[[dp.name]]$elms[[dim]] %in% elm.indices
                
                dc$obs[[dp.name]]$elms <-
                    lapply (dc$obs[[dp.name]]$elms, function (d)
                        d [keep])
                dc$obs[[dp.name]]$vars <-
                    lapply (dc$obs[[dp.name]]$vars, function (d)
                        d [keep])
                dc$obs[[dp.name]]$elms[[dim]] <-
                    new.indices [dc$obs[[dp.name]]$elms[[dim]]]
            }
        }
        
        if (suppress)
            dc <- update.margin (dc)
        return (dc)
    }


select.elm <- function (obj, ...) {
    UseMethod ("select.elm")
}

#' @title Select, remove, or filter elements within a given dimension.
#'
#' @description
#' \code{select.elm()}, \code{remove.elm()}, and \code{filter.elm()} respectively select, remove, or filter a subset of elements within a particular dimension.
#' This consists in suppressing all observations that correspond to the unselected, removed, or unfiltered elements.
#'
#' @param dc A datacube.
#' @param dim An unquoted expression giving the dimension whose elements are selected, removed, or filtered.
#' @param elm.array Optional. A character vector giving the names of the elements to select or to remove.
#' @param top.nb bot.nb Optional. An integer giving the number of elements to select or to remove among the ones with the highest or the lowest values of the specified variable (see \code{var}).
#' @param var Optional. Can be specified if either \code{top.nb} or \code{bot.nb} is specified. An unquoted expression giving the variable to consider when selecting or removing the top or the bottom elements. If not specified, the datacube's first variable is taken. Can also be \code{model}, \code{ratio}, or \code{deviation} if a marginal model has been computed.
#' @param filter An unquoted expression specifying the condition that is used to filter elements.
#' @param suppress A logical indicating if the corresponding observations should be fully suppressed (recomputing the datacube's marginals) or simply hidden (no longer shown when processing the datacube).
#' @return The resulting datacube.
#'
#' @method select.elm data.cube
#' @export
select.elm.data.cube <-
    function (dc,
              dim,
              elm.array = NULL,
              filter = NULL,
              top.nb = NULL,
              bot.nb = NULL,
              var = NULL,
              suppress = FALSE) {
        dim <- deparse (substitute (dim))

        if (!is.null (elm.array)) {
            elm.indices <-
                seq (length (dc$elm.names[[dim]])) [dc$elm.names[[dim]] %in% elm.array]
        } else if (deparse (substitute (filter)) != "NULL") {
            filter <- substitute (filter)
            
            if (is.null (dc$obs[[dim]]))
                dc <- compute.margin_(dc, dim)
            
            keep <- eval (filter, dc$obs[[dim]]$vars)
            elm.indices <- dc$obs[[dim]]$elms[[dim]] [keep]
        } else {
            var <- deparse (substitute (var))
            if (var == "NULL")
                var <- dc$var.names[1]
            
            if (is.null (dc$obs[[dim]]) ||
                is.null (dc$obs[[dim]]$vars[[var]]))
                dc <- compute.margin_(dc, dim)
            
            if (!is.null (top.nb))
                elm.indices <-
                    head (dc$obs[[dim]]$elms[[dim]] [order (dc$obs[[dim]]$vars[[dc$var.names[1]]], decreasing =
                                                                                                       TRUE)], top.nb)
            else if (!is.null (bot.nb))
                elm.indices <-
                    tail (dc$obs[[dim]]$elms[[dim]] [order (dc$obs[[dim]]$vars[[dc$var.names[1]]], decreasing =
                                                                                                       TRUE)], bot.nb)
            
            elm.indices <- elm.indices [order (elm.indices)]
        }
        select.elm.indices_(dc, dim, elm.indices = elm.indices, suppress = suppress)
    }


select.elm_ <-
    function (obj, ...) {
        UseMethod ("select.elm_")
    }
select.elm_.data.cube <-
    function (dc,
              dim,
              elm.array = NULL,
              ## filter = NULL,
              top.nb = NULL,
              bot.nb = NULL,
              var = NULL,
              suppress = FALSE) {

        if (! is.null (elm.array)) {
            elm.indices <-
                seq (length (dc$elm.names[[dim]])) [dc$elm.names[[dim]] %in% elm.array]
            ## } else if (! is.null (filter)) {
            ##     filter <- parse (text = filter)
            
            ##     if (is.null (dc$obs[[dim]])) { dc <- compute.margin_(dc, dim) }
            
            ##     keep <- eval (filter, dc$obs[[dim]]$vars)
            ##     elm.indices <- dc$obs[[dim]]$elms[[dim]] [keep]
        } else {
            if (is.null (var))
                var <- dc$var.names[1]
            
            if (is.null (dc$obs[[dim]]) ||
                is.null (dc$obs[[dim]]$vars[[var]]))
                dc <- compute.margin_(dc, dim)
            
            if (!is.null (top.nb))
                elm.indices <-
                    head (dc$obs[[dim]]$elms[[dim]] [order (dc$obs[[dim]]$vars[[dc$var.names[1]]], decreasing =
                                                                                                       TRUE)], top.nb)
            else if (!is.null (bot.nb))
                elm.indices <-
                    tail (dc$obs[[dim]]$elms[[dim]] [order (dc$obs[[dim]]$vars[[dc$var.names[1]]], decreasing =
                                                                                                       TRUE)], bot.nb)
            
            elm.indices <- elm.indices [order (elm.indices)]
        }
        select.elm.indices_(dc, dim, elm.indices = elm.indices, suppress = suppress)
    }


remove.elm_ <- function (obj, ...) {
    UseMethod ("remove.elm_")
}
remove.elm_.data.cube <- function (dc, dim, elm, suppress = FALSE) {
    elm.indices <-
        seq (length (dc$elm.names[[dim]])) [!dc$elm.names[[dim]] %in% elm]
    dc %>% select.elm.indices_(dim, elm.indices, suppress)
}

remove.elm <- function (obj, ...) {
    UseMethod ("remove.elm")
}

#' @method remove.elm data.cube
#' @rdname select.elm.data.cube
remove.elm.data.cube <-
    function (dc,
              dim,
              elm = NULL,
              top = NULL,
              bot = NULL,
              var = NULL,
              suppress = FALSE) {
        dim <- deparse (substitute (dim))
        dc %>% remove.elm_(dim, elm, suppress)
    }




#' filter.elm <- function (obj, ...) {
#'   UseMethod ("filter.elm")
#' }
#' 
#' #' @method filter.elm data.cube
#' #' @rdname select.elm.data.cube
#' filter.elm.data.cube <-
#'   function (dc, dim, condition, suppress = FALSE) {
#'     dim <- deparse (substitute (dim))
#'     condition <- substitute (condition)
#'     
#'     dp.name <- paste (dim, collapse = ".")
#'     if (is.null (dc$obs[[dp.name]]))
#'       dc <- compute.margin_(dc, dim)
#'     
#'     keep <- eval (condition, dc$obs[[dp.name]]$vars)
#'     elm.indices <- dc$obs[[dp.name]]$elms[[dim]] [keep]
#'     select.elm.indices_(dc, dim, elm.indices = elm.indices, suppress = suppress)
#'   }




filter.obs <- function (obj, ...) {
    UseMethod ("filter.obs")
}
filter.obs.data.cube <- function (dc, condition) {
    condition <- substitute (condition)
    dc.name <- paste (dc$dim.names, collapse = ".")
    
    keep <- eval (condition, dc$obs[[dc.name]]$vars)
    dc$obs[[dc.name]]$elms <-
        lapply (dc$obs[[dc.name]]$elms, function (dim)
            dim [keep])
    dc$obs[[dc.name]]$vars <-
        lapply (dc$obs[[dc.name]]$vars, function (dim)
            dim [keep])
    
    return (dc)
}




arrange.elm_ <- function (obj, ...) {
    UseMethod ("arrange.elm_")
}
arrange.elm_.data.cube <-
    function (dc,
              dim,
              var = NULL,
              decreasing = FALSE) {
        new.indices <- list ()
        for (d in dim) {
            if (is.null (var)) {
                if (decreasing) {
                    new.indices[[d]] <- rank (desc (dc$elm.names[[d]]))
                }
                else {
                    new.indices[[d]] <- rank (dc$elm.names[[d]])
                }
                dc$elm.names[[d]] <-
                    dc$elm.names[[d]] [order (dc$elm.names[[d]], decreasing = decreasing)]
            } else {
                if (is.null (dc$obs[[d]]))
                    dc$obs[[d]] <- compute.margin_(dc, d)
                if (decreasing) {
                    new.indices[[d]] <-
                        rank (desc (dc$obs[[d]]$vars[[var]]), ties.method = "first")
                }
                else {
                    new.indices[[d]] <-
                        rank (dc$obs[[d]]$vars[[var]], ties.method = "first")
                }
                new.indices[[d]] <-
                    new.indices[[d]] [order (dc$obs[[d]]$elms[[d]])]
                dc$elm.names[[d]] <-
                    dc$elm.names[[d]] [dc$obs[[d]]$elms[[d]] [order (dc$obs[[d]]$vars[[var]], decreasing =
                                                                                                  decreasing)]]
            }
        }
        
        for (dp.name in names (dc$obs)) {
            if (dp.name == ".")
                next
            dim2 <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
            for (d in rev (dim)) {
                if (!d %in% dim2)
                    next
                dc$obs[[dp.name]]$elms[[d]] <-
                    new.indices[[d]] [dc$obs[[dp.name]]$elms[[d]]]
                order <- order (dc$obs[[dp.name]]$elms[[d]])
                dc$obs[[dp.name]]$elms <-
                    lapply (dc$obs[[dp.name]]$elms, function (dim)
                        dim [order])
                dc$obs[[dp.name]]$vars <-
                    lapply (dc$obs[[dp.name]]$vars, function (dim)
                        dim [order])
            }
        }
        
        return (dc)
    }

arrange.elm <- function (obj, ...) {
    UseMethod ("arrange.elm")
}
arrange.elm.data.cube <-
    function (dc,
              ...,
              var = NULL,
              decreasing = FALSE) {
        dim <- sapply (eval (substitute (alist (...))), deparse)
        var <- deparse (substitute (var))
        if (var == "NULL")
            var <- NULL
        arrange.elm_(dc,
                     dim = dim,
                     var = var,
                     decreasing = decreasing)
    }


arrange.obs_<- function (obj, ...) { UseMethod ("arrange.obs_") }
arrange.obs_.data.cube <- function (dc, var=dc$var.names[1], decreasing=TRUE) {
    dc.name <- paste (dc$dim.names, collapse=".")
    order <- order (dc$obs[[dc.name]]$vars[[var]], decreasing=decreasing)
    dc$obs[[dc.name]]$elms <- lapply (dc$obs[[dc.name]]$elms, function (dim) dim [order])
    dc$obs[[dc.name]]$vars <- lapply (dc$obs[[dc.name]]$vars, function (dim) dim [order])

    return (dc)
}

arrange.obs <- function (obj, ...) { UseMethod ("arrange.obs") }
arrange.obs.data.cube <- function (dc, var=NULL, decreasing=TRUE) {
    var <- deparse (substitute (var))
    if (var == "NULL") var <- dc$var.names[1]
    arrange.obs_(dc, var=var, decreasing=decreasing)
}



plot.var_ <- function (obj, ...) {
    UseMethod ("plot.var_")
}
plot.var_.data.cube <-
    function (dc,
              var,
              type = "col",
              sep.dim = NULL) {
        df <- as.data.frame (dc, complete = TRUE)
        if (nrow (df) == 0)
            return (NULL)
        
        unsep.dim <- dc$dim.names
        if (!is.null (sep.dim))
            unsep.dim <- dc$dim.names [dc$dim.names != sep.dim]
        
        uniq.dim <- c()
        for (d in unsep.dim)
            if (length (unique (df[, d])) == 1)
                uniq.dim <- append (uniq.dim, d)
        ununiq.dim <- unsep.dim [!unsep.dim %in% uniq.dim]
        
        df$label <-
            apply (df, 1, function (row)
                paste (row[ununiq.dim], collapse = " / "))
        df$label <- factor (df$label, levels = unique (df$label))
        
        if (type == "col") {
            if (var == "ratio") { df$ratio <- df$ratio - 1 }
            if (var == "deviation" && dc$model$type == "ratio") {df$deviation <- df$deviation - 1 }
            
            p <- ggplot (df, aes (x = label, y = get (var)))
            
            if (is.null (sep.dim)) {
                p <- p + geom_col ()
            } else {
                p <- p +
                    geom_col (aes (fill = factor (get (sep.dim), levels = dc$elm.names[[sep.dim]])), position = "dodge") +
                    guides (fill = guide_legend (title = sep.dim))
            }
            
            if (var == "ratio" || var == "deviation" && dc$model$type == 'ratio') { p <- p + scale_y_continuous (labels = function (x) x + 1) }
        }
        
        if (type == "line") {
            indices <- unique (df[, ununiq.dim, drop = FALSE])
            indices$row <- 1:nrow(indices)
            indices$label <-
                apply (indices, 1, function (row)
                    paste (row[ununiq.dim], collapse = " / "))
            df$index <-
                merge (df[, ununiq.dim, drop = FALSE], indices)$row
            
            if (is.null (sep.dim)) {
                p <- ggplot (df, aes (x = index, y = get (var)))
            } else {
                p <- ggplot (df, aes (
                                     x = index,
                                     y = get (var),
                                     color = factor (get (sep.dim), levels = rev (dc$elm.names[[sep.dim]]))
                                 )) +
                    guides (color = guide_legend (title = sep.dim))
            }
            
            p <- p + geom_line () +
                scale_x_continuous (breaks = 1:nrow(indices),
                                    labels = indices$label)
            
            if (var == "ratio" || var == "deviation" && dc$model$type == 'ratio') { p <- p + geom_hline (yintercept = 1) }
        }
        
        p <- p + ylab (var) +
            xlab (paste (ununiq.dim, collapse = " x ")) +
            theme (axis.text.x = element_text (angle = 90, hjust = 1))
        
        if (length (uniq.dim) > 0) {
            title1 <- paste (uniq.dim, collapse = " x ")
            title2 <- paste (df[1, uniq.dim], collapse = " / ")
            p <- p + labs (subtitle = paste (title1, "=", title2))
        }
        
        return (p)
    }


plot.var <- function (obj, ...) {
    UseMethod ("plot.var")
}
plot.var.data.cube <-
    function (dc,
              var = NULL,
              sep.dim = NULL,
              type = "col") {
        var <- deparse (substitute (var))
        if (var == "NULL")
            var <- dc$var.names[1]
        
        sep.dim <- deparse (substitute (sep.dim))
        if (sep.dim == "NULL")
            sep.dim <- NULL
        
        plot.var_(dc, var, type = type, sep.dim = sep.dim)
    }


biplot.var_ <- function (obj, ...) {
    UseMethod ("biplot.var_")
}
biplot.var_.data.cube <- function (dc, x.dim, y.dim, var) {
    ## Get data
    df <- as.data.frame (dc)
    
    uniq.dim <- c()
    for (d in dc$dim.names)
        if (length (unique (df[, d])) == 1)
            uniq.dim <- append (uniq.dim, d)
    ununiq.dim <- dc$dim.names [!dc$dim.names %in% uniq.dim]
    
    ## Build plot
    p <-
        ggplot (data = df, aes (y = factor (get (x.dim), levels = rev (dc$elm.names[[x.dim]])), x = factor (get (y.dim)))) +
        xlab (y.dim) + ylab (x.dim)
    
    if (var != "ratio" && var != "deviation") {
        p <-
            p + geom_point (aes (size = get (var)),
                            pch = 21,
                            color = "black",
                            fill = "grey") +
            guides (size = guide_legend (title = var)) +
            scale_size (range = c(1, 20))
    } else {
        var.name <- dc$var.names[1]
        p <-
            p + geom_point (aes (fill = get (var), size = get (var.name)),
                            pch = 21,
                            color = "black") +
            guides (fill = guide_legend (title = var),
                    size = guide_legend (title = var.name)) +
            scale_size (range = c(1, 20))
        
        if (var == "ratio" ||
            var == "deviation" &&
            dc$model$type == "ratio")
            p <-
                p + scale_fill_gradient2 (
                        low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 1
                    )
        else
            p <-
                p + scale_fill_gradient2 (
                        low = "blue",
                        mid = "white",
                        high = "red",
                        midpoint = 0
                    )
    }
    
    p <-
        p + theme (axis.text.x = element_text (angle = 90, hjust = 1))
    
    if (length (uniq.dim) > 0) {
        title1 <- paste (uniq.dim, collapse = " x ")
        title2 <- paste (df[1, uniq.dim], collapse = " / ")
        p <- p + labs (subtitle = paste (title1, "=", title2))
    }
    
    return (p)
}


biplot.var <- function (obj, ...) {
    UseMethod ("biplot.var")
}
biplot.var.data.cube <- function (dc, x.dim, y.dim, var = NULL) {
    x.dim <- deparse (substitute (x.dim))
    y.dim <- deparse (substitute (y.dim))
    
    var <- deparse (substitute (var))
    if (var == "NULL")
        var <- dc$var.names[1]
    
    biplot.var_(dc, x.dim, y.dim, var = var)
}



plot.outliers <- function (obj, ...) {
    UseMethod ("plot.outliers")
}
plot.outliers.data.cube <- function (dc, labels = TRUE) {
    dc.name <- paste (dc$dim.names, collapse = ".")
    var.name <- dc$var.names[1]
    
    ## Get data
    df <- as.data.frame (dc)
    
    uniq.dim <- c()
    for (d in dc$dim.names)
        if (length (unique (df[, d])) == 1)
            uniq.dim <- append (uniq.dim, d)
    ununiq.dim <- dc$dim.names [!dc$dim.names %in% uniq.dim]
    
    df$label <-
        apply (df, 1, function (row)
            paste (row[ununiq.dim], collapse = " / "))
    
    df$type <- ifelse (df$outlier == 0, "normal", "outlier")
    types <- unique (df$type)
    
    shape.values <- c(21, 22)
    if (length (types) == 1) {
        if (types == "normal") {
            shape.values <- c(21)
        }
        else if (types == "outlier") {
            shape.values <- c(22)
        }
    }
    
    ## Build plot
    p <- ggplot (data = df, aes (x = get (var.name), y = ratio)) +
        scale_x_log10 () + scale_y_log10 () +
        geom_point (aes (
            fill = deviation,
            size = abs (deviation),
            shape = type
        )) +
        scale_shape_manual (values = shape.values) +
        scale_fill_gradient2 (low = "blue", high = "red") +
        geom_hline (yintercept = 1) +
        xlab (var.name) + ylab (paste ("deviation wrt ratio"))
    
    if (length (uniq.dim) > 0) {
        title1 <- paste (uniq.dim, collapse = " x ")
        title2 <- paste (df[1, uniq.dim], collapse = " / ")
        p <- p + labs (subtitle = paste (title1, "=", title2))
    }
    
    if (labels)
        p <-
            p + geom_text_repel (data = df [df$outlier != 0, ], aes (
                                                                    x = get (var.name),
                                                                    y = ratio,
                                                                    label = label
                                                                ))
    
    return (p)
}




compute.model_ <-
    function (obj, ...) {
        UseMethod ("compute.model_")
    }
compute.model_.data.cube <-
    function (dc,
              dim,
              deviation.type = "ratio",
              deviation.threshold = 3) {
        ## TODO: allow to use multidim data planes for normalisation
        dim <- dc$dim.names [dc$dim.names %in% dim]
        dc.name <- paste (dc$dim.names, collapse = ".")
        var.name <- dc$var.names[1]
        
        for (d in dim) {
            dp.name <- paste (d, collapse = ".")
            if (is.null (dc$obs[[dp.name]]))
                dc <- dc %>% compute.margin_(d)
        }
        
        if (is.null (dc$obs[["."]]))
            dc <- dc %>% compute.margin_()
        sum <- dc$obs[["."]]$vars[[var.name]]
        
        ## Compute model
        model <- rep (sum, length (dc$obs[[dc.name]]$vars[[var.name]]))
        
        for (d in dim) {
            dp.name <- paste (d, collapse = ".")
            dc.data <-
                append (list (1:length(dc$obs[[dc.name]]$var[[var.name]])), dc$obs[[dc.name]]$elms[d])
            names (dc.data) [1] <- "row"
            dp.data <-
                append (dc$obs[[dp.name]]$elms, dc$obs[[dp.name]]$vars[var.name])
            
            dist <- merge (dc.data, dp.data)
            dist <- dist [order (dist$row), var.name] / sum
            model <- model * dist
        }
        
        other.dim <- dc$dim.names [!dc$dim.names %in% dim]
        for (d in other.dim)
            model <- model / dc$elm.nb[[d]]
        
        dc$obs[[dc.name]]$vars[["model"]] <- model
        dc$var.names <- unique (append (dc$var.names, "model"))
        
        ## Compute deviation
        dc$obs[[dc.name]]$vars[["ratio"]] <-
            dc$obs[[dc.name]]$vars[[var.name]] / dc$obs[[dc.name]]$vars[["model"]]
        dc$var.names <- unique (append (dc$var.names, "ratio"))
        
        if (deviation.type == "ratio") {
            dc$obs[[dc.name]]$vars[["deviation"]] <-
                dc$obs[[dc.name]]$vars[["ratio"]]
        }
        
        if (deviation.type == "poisson") {
            dc$obs[[dc.name]]$vars[["deviation"]] <- ifelse (
                dc$obs[[dc.name]]$vars[[var.name]] < dc$obs[[dc.name]]$vars[["model"]],
                ppois (
                    dc$obs[[dc.name]]$vars[[var.name]],
                    dc$obs[[dc.name]]$vars[["model"]],
                    lower.tail = TRUE,
                    log.p = TRUE
                ),
                -ppois (
                     dc$obs[[dc.name]]$vars[[var.name]],
                     dc$obs[[dc.name]]$vars[["model"]],
                     lower.tail = FALSE,
                     log.p = TRUE
                 )
            )
        }
        
        if (deviation.type == "KLdiv") {
            dc$obs[[dc.name]]$vars[["deviation"]] <-
                dc$obs[[dc.name]]$vars[[var.name]] / sum * log2 (dc$obs[[dc.name]]$vars[[var.name]] / dc$obs[[dc.name]]$vars[["model"]])
        }
        
        dc$var.names <- unique (append (dc$var.names, "deviation"))
        
        ## Apply threshold
        dev.mean <- mean (dc$obs[[dc.name]]$vars[["deviation"]])
        dev.sd <- sd (dc$obs[[dc.name]]$vars[["deviation"]])
        
        if (is.na (dev.sd)) {
            df$outlier <-
                rep (0, length (dc$obs[[dc.name]]$vars[["deviation"]]))
        }
        else {
            dc$obs[[dc.name]]$vars[["outlier"]] <-
                findInterval (dc$obs[[dc.name]]$vars[["deviation"]],
                              dev.mean + dev.sd * deviation.threshold * c(-1, 1)) - 1
        }
        
        dc$var.names <- unique (append (dc$var.names, "outlier"))
        
        ## Store model parameters
        dc$model <- list()
        dc$model$dim <- dim
        dc$model$type <- deviation.type
        dc$model$threshold <- deviation.threshold
        
        return (dc)
    }


compute.model <- function (obj, ...) {
    UseMethod ("compute.model")
}
compute.model.data.cube <-
    function (dc, ..., deviation.type = "ratio", deviation.threshold = 3) {
        dim <- sapply (eval (substitute (alist (...))), deparse)
        compute.model_(dc, dim, deviation.type = deviation.type, deviation.threshold = deviation.threshold)
    }





## Print data summary
data.summary <- function (obj, ...) {
    UseMethod ("data.summary")
}
data.summary.data.cube <- function (dc, data = "obs") {
    dc.name <- paste (dc$dim.names, collapse = ".")
    summary (dc$obs[[dc.name]]$vars[[data]])
}


## Plot data distribution
data.distribution <-
    function (obj, ...) {
        UseMethod ("data.distribution")
    }
data.distribution.data.cube <-
    function (dc,
              data = "obs",
              log = "",
              threshold = NULL) {
        dc.name <- paste (dc$dim.names, collapse = ".")
        p <-
            ggplot (as.data.frame (dc$obs[[dc.name]]$vars), aes (x = get(data))) +
            geom_histogram (
                bins = 100,
                aes (y = ..count..),
                color = "black",
                fill = "blue",
                alpha = 0.3
            )
        
        title <- paste ("Distribution of", data)
        
        if (log == "x") {
            logmax <- ceiling(log10(max(dc$obs[[dc.name]]$vars[[data]])))
            breaks <- 10 ^ seq (0, logmax, logmax / 10)
            title <- paste (title, "(logarithmic scale)")
            
            p <- p + scale_x_continuous (trans = "log", breaks = breaks) +
                scale_y_continuous (breaks = scales::pretty_breaks (n = 10))
        }
        
        else if (log == "y") {
            title <- paste (title, "(logarithmic scale)")
            
            p <- p + scale_y_continuous (trans = "log")
        }
        
        else if (log == "xy") {
            logmax <- ceiling(log10(max(dc$obs[[dc.name]]$vars[[data]])))
            breaks <- 10 ^ seq (0, logmax, logmax / 10)
            title <- paste (title, "(logarithmic scales)")
            
            p <- p + scale_x_continuous (trans = "log", breaks = breaks) +
                scale_y_continuous (trans = "log")
        }
        
        if (!is.null (threshold)) {
            data.mean <- mean (dc$obs[[dc.name]]$vars[[data]])
            data.sd <- sd (dc$obs[[dc.name]]$vars[[data]])
            
            p <-
                p + geom_vline (
                        xintercept = data.mean + threshold * data.sd,
                        size = 2,
                        color = "red"
                    ) +
                geom_vline (
                    xintercept = data.mean - threshold * data.sd,
                    size = 2,
                    color = "red"
                )
        }
        
        p <- p + labs (title = title) +
            xlab (data) + ylab ("count")
        
        return (p)
    }




## List outliers
list.outliers <- function (obj, ...) {
    UseMethod ("list.outliers")
}
list.outliers.data.cube <-
    function (dc,
              input = "obs",
              model = "exp",
              deviation = "dev",
              outlier = "out") {
        data.frame <-
            as.data.frame (cbind (as.data.frame (dc$obs[[dc.name]]$elms), as.data.frame (dc$obs[[dc.name]]$vars)))
        if (!is.null (display)) {
            data.frame <- data.frame[data.frame[[display]],]
        }
        
        data.frame$ratio <- data.frame$obs / data.frame$exp
        data.frame$type <-
            ifelse (data.frame[[outlier]] == 0, "normal", "abnormal")
        
        dummy <-
            lapply (dc$dim.names, function (dim) {
                return (data.frame[[dim]] <<-
                            dc$elm.names[[dim]][data.frame[[dim]]])
            })
        data.frame$label <-
            apply (data.frame, 1, function (row)
                paste (row[dc$dim.names], collapse = " "))
        
        p <-
            ggplot (data = data.frame, aes (x = get(input), y = ratio)) +
            scale_x_log10 () + scale_y_log10 () +
            geom_point (aes (
                size = abs(dev),
                fill = dev,
                shape = type
            )) +
            scale_shape_manual (val.obs = c(22, 21)) +
            scale_fill_gradient2 (low = "blue", high = "red") +
            labs (title = "Outliers") + xlab (input) + ylab (paste (input, "/", model)) +
            theme_bw () ##+ theme (text=elm_text (size=20))
        
        if (labels) {
            p <-
                p + geom_text_repel (data = data.frame[data.frame[[outlier]] != 0,], aes (
                                                                                         x = get(input),
                                                                                         y = ratio,
                                                                                         label = label
                                                                                     ))
        }
        
        return (p)
    }


## Draw cube
draw.cube <- function (obj, ...) {
    UseMethod ("draw.cube")
}
draw.cube.data.cube <-
    function (dc,
              dims = dc$dim.names,
              dim.dev = c()) {
        open3d (userMatrix = rotationMatrix (pi / 5, 1, 0, 0) %*% rotationMatrix (pi /
                                                                                  4, 0,-1, 0))
        cell3d <-
            function (x,
                      y,
                      z,
                      dx,
                      dy,
                      dz,
                      color = "green",
                      alpha = 1,
                      lwd = 5) {
                shade3d (translate3d (scale3d (
                    cube3d (color = color, alpha = alpha), dx / 2, dy / 2, dz / 2
                ), x + dx / 2, y + dy / 2, z + dz / 2))
                wire3d (translate3d (scale3d (
                    cube3d (color = "black"), dx / 2, dy / 2, dz / 2
                ), x + dx / 2, y + dy / 2, z + dz / 2), lwd = lwd)
            }
        
        cv <- list()
        dv <- list()
        
        for (i in 1:3) {
            if (dims[i] %in% dc$dim.names) {
                cv[[i]] <- 1:min(length(dc$elm.names[[dims[i]]]), 5)
                dv[[i]] <- 1
            } else {
                cv[[i]] <- c(1)
                dv[[i]] <- 5
            }
            
            if (dims[i] %in% dim.dev) {
                cv[[i]] <- append(-1, cv[[i]])
            }
        }
        
        for (x in cv[[1]]) {
            for (y in cv[[2]]) {
                for (z in cv[[3]]) {
                    if (x == -1) {
                        dx <- 1
                    } else {
                        dx <- dv[[1]]
                    }
                    if (y == -1) {
                        dy <- 1
                    } else {
                        dy <- dv[[2]]
                    }
                    if (z == -1) {
                        dz <- 1
                    } else {
                        dz <- dv[[3]]
                    }
                    if (x == -1 ||
                        y == -1 ||
                        z == -1) {
                        color <- "forestgreen"
                    } else {
                        color <- "green"
                    }
                    cell3d (x, y, z, dx, dy, dz, color = color)
                }
            }
        }
        
        cell3d (1,
                1,
                1,
                5,
                5,
                5,
                color = "grey",
                alpha = 0.1,
                lwd = 3)
    }
