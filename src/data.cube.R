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


library ("tidyverse")
library ("ggrepel")



as.data.cube_ <- function (obj, ...) UseMethod ("as.data.cube_")
as.data.cube_.data.frame <-
    function (df, dim.names, var.names) {
        ## Rename dims and vars
        if (is.null (names (dim.names))) {
            names (dim.names) <- dim.names
        } else {
            names (dim.names) <-
                ifelse (names (dim.names) == "", unlist (dim.names), names (dim.names))
        }
        dim.names <- unlist (dim.names)
        
        if (is.null (names (var.names))) {
            names (var.names) <- var.names
        } else {
            names (var.names) <-
                ifelse (names (var.names) == "", unlist (var.names), names (var.names))
        }
        var.names <- unlist (var.names)
        
        ## TODO: aggregate data.frame if needed
        if (nrow (unique (df [, dim.names, drop = FALSE])) < nrow (df)) {
            stop ("input data.frames first need to be aggregated")
        }
        ## TODO: suppress null observations
        
        ## Build data.cube
        dc <- list()
        class (dc) <- "data.cube"
        
        dc$dim.nb <- length (dim.names)
        dc$dim.names <- names (dim.names)
        
        elm.names <- lapply (df[, dim.names, drop=FALSE], function (d) unique (d))
        dc$elm.nb <- lapply (elm.names, function (d) length (d))
        dc$elm.names <- elm.names

        dc$var.nb <- length (var.names)
        dc$var.names <- names (var.names)
        dc$var.dim.names <- rep (list (dc$dim.names), dc$var.nb)
                
        dc$var.NA <- lapply (df [, var.names, drop = FALSE], function (list) ifelse (class (list) %in% c ("integer", "numeric"), 0, NA))
        dc$var.FUN <- lapply (df [, var.names, drop = FALSE], function (list) ifelse (class (list) %in% c ("integer", "numeric"), sum, function (...) paste (..., collapse = ","))) ## TODO: simpler form?

        ## Fill observations
        dc$dp <- list()
        dc.name <- paste (dc$dim.names, collapse = ".")
        
        dc$dp[[dc.name]] <- list()
        
        dc$dp[[dc.name]]$elms <-
            lapply (dim.names, function (d) {
                return (match (df[, d], dc$elm.names[[d]]))
            })

        dc$dp[[dc.name]]$vars <- as.list (df [, var.names, drop = FALSE])
        
        names (dc$elm.nb) <- dc$dim.names
        names (dc$elm.names) <- dc$dim.names
        names (dc$var.dim.names) <- dc$var.names
        names (dc$var.NA) <- dc$var.names
        names (dc$var.FUN) <- dc$var.names
        names (dc$dp[[dc.name]]$elms) <- dc$dim.names
        names (dc$dp[[dc.name]]$vars) <- dc$var.names
        
        ## Compute margins
        ## dc <- compute.margin_(dc)
        ## for (d in dc$dim.names)
        ##   dc <- compute.margin_(dc, d)
        
        return (dc)
    }


as.data.cube <- function (obj, ...) UseMethod ("as.data.cube")
as.data.cube.data.frame <- function (df, dim.names, var.names) {
    dim.names <- lapply (substitute (dim.names), deparse)
    dim.names <- unlist (dim.names) [2:length (dim.names)] ## TODO: if only one dim
    
    var.names <- lapply (substitute (var.names), deparse)
    var.names <- unlist (var.names) [2:length (var.names)] ## TODO: if only one var
    
    as.data.cube_(df, dim.names, var.names)
}


merge.data.cube <- function (...) {
    dc.list <- list (...)
    dc <- list()
    class (dc) <- "data.cube"

    dim.names <- unique (unlist (lapply (dc.list, function (dc) dc$dim.names)))
    dc$dim.nb <- length (dim.names)
    dc$dim.names <- dim.names

    elm.names <- lapply (dim.names, function (d) unique (unlist (lapply (dc.list, function (dc) dc$elm.names[[d]]))))
    dc$elm.nb <- lapply (elm.names, function (d) length (d))
    dc$elm.names <- elm.names
    names (dc$elm.nb) <- dc$dim.names
    names (dc$elm.names) <- dc$dim.names

    ## TODO: check if variables are unique
    var.names <- unlist (lapply (dc.list, function (dc) dc$var.names))
    dc$var.nb <- length (var.names)
    dc$var.names <- var.names
    dc$var.dim.names <- unlist (lapply (dc.list, function (dp) dp$var.dim.names), recursive = FALSE)
    dc$var.dim.names <- lapply (dc$var.dim.names, function (dim.names) dc$dim.names [dc$dim.names %in% dim.names])
    dc$var.NA <- unlist (lapply (dc.list, function (dp) dp$var.NA), recursive = FALSE)
    dc$var.FUN <- unlist (lapply (dc.list, function (dp) dp$var.FUN), recursive = FALSE)
    
    for (dp in dc.list) {
        dp.name <- paste (dp$dim.names, collapse = ".")

        dp$dp[[dp.name]]$elms <- lapply (dp$dim.names, function (d) match (dp$elm.names[[d]], dc$elm.names[[d]]) [dp$dp[[dp.name]]$elms[[d]]])
        names (dp$dp[[dp.name]]$elms) <- dp$dim.names

        new.dp.name <- paste (dc$dim.names [dc$dim.names %in% dp$dim.names], collapse = ".")
        dc$dp[[new.dp.name]] <- dp$dp[[dp.name]]
    }

    return (dc)
}


is.data.cube <- function (obj) {
    inherits (obj, "data.cube")
}


as.data.frame_ <- function (obj, ...) UseMethod ("as.data.frame_")
as.data.frame_.data.cube <-
    function (dc,
              dim.names,
              complete = FALSE) {
        dim.names <- unlist (dim.names)
        dp.name <- paste (dc$dim.names [dc$dim.names %in% dim.names], collapse = ".")
        var.names <- names (dc$dp[[dp.name]]$vars)

        df <- as.data.frame (cbind (as.data.frame (dc$dp[[dp.name]]$elms), as.data.frame (dc$dp[[dp.name]]$vars)))

        if (complete) {
            ## TODO: complete wrt dc$elm.names
            df$row <- 1:nrow(df)
            fill <- as.list (sapply (dc$var.names, function (v)
                0))
            fill$row <- nrow(df) + 1
            df <-
                as.data.frame (complete_(df, cols = dc$dim.names, fill = fill))
            df <- df [order (df$row), ]
            df$row <- NULL
        }

        df <- df [, append (dim.names, var.names)]
        if (length (dim.names) > 0) {
            dummy <-
                lapply (dim.names, function (d) {
                    return (df[[d]] <<- dc$elm.names[[d]][df[[d]]])
                })
        }
        if (length (df) == 1) {
            return (df[, 1])
        } else {
            return (df)
        }
    }

as.data.frame.data.cube <-
    function (dc, ..., complete = FALSE) {
        dim.names <- sapply (eval (substitute (alist (...))), deparse)
        if (length (dim.names) == 0) { dim.names <- dc$dim.names }
        as.data.frame_(dc, dim.names, complete = complete)
    }



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
dim.names.data.cube <- function (dc) {
    dc$dim.names
}

elm.names_ <- function (obj, ...) {
    UseMethod ("elm.names_")
}
elm.names_.data.cube <- function (dc, dim.names = dc$dim.names) {
    if (length (dim.names) == 1) { return (dc$elm.names[[unlist(dim.names)]]) }
    dc$elm.names[dim.names]
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
    dim.names <- sapply (eval (substitute (alist (...))), deparse)
    if (length (dim.names) == 0) { dim.names <- dc$dim.names }
    elm.names_(dc, dim.names)
}

var.names <- function (obj, ...) {
    UseMethod ("var.names")
}

#' @return A character vector giving the names of the variables of
#' \code{dc}.
#'
#' @rdname property
#' @method var.names data.cube
var.names.data.cube <- function (dc, ...) {
    dim.names <- sapply (eval (substitute (alist (...))), deparse)
    if (length (dim.names) == 0) { return (dc$var.names) }
    dp.name <- paste (dc$dim.names [dc$dim.names %in% dim.names], collapse = ".")
    if (! is.null (dc$dp[[dp.name]])) {
        return (names (dc$dp[[dp.name]]$vars))
    } else {
        return (character())
    }
}

dim.nb <- function (obj, ...) {
    UseMethod ("dim.nb")
}

#' @return An integer giving the number of dimensions of \code{dc}.
#'
#' @rdname property
#' @method dim.nb data.cube
dim.nb.data.cube <- function (dc) {
    dc$dim.nb
}

elm.nb_ <- function (obj, ...) {
    UseMethod ("elm.nb_")
}
elm.nb_.data.cube <- function (dc, dim.names = dc$dim.names) {
    if (length (dim.names) == 1) { return (dc$elm.nb[[unlist(dim.names)]]) }
    dc$elm.nb[dim.names]
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
    dim.names <- sapply (eval (substitute (alist (...))), deparse)
    if (length (dim.names) == 0) { dim.names <- dc$dim.names }
    elm.nb_(dc, dim.names)
}

var.nb <- function (obj, ...) {
    UseMethod ("var.nb")
}

#' @return An integer giving the number of variables of \code{dc}.
#'
#' @rdname property
#' @method var.nb data.cube
var.nb.data.cube <- function (dc, ...) {
    dim.names <- sapply (eval (substitute (alist (...))), deparse)
    if (length (dim.names) == 0) { return (dc$var.nb) }
    dp.name <- paste (dc$dim.names [dc$dim.names %in% dim.names], collapse = ".")
    if (! is.null (dc$dp[[dp.name]])) {
        return (length (dc$dp[[dp.name]]$vars))
    } else {
        return (0)
    }
}



## TODO : rewrite this function
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
        cat (" - total: ", dc$dp[["."]]$vars[[var]], "\n", sep = "")
        dp.nb <- length (dc$dp[[dc.name]]$vars[[var]])
        cat (
            " - divided into ",
            dp.nb,
            " (non-null) dpervation",
            ifelse (dp.nb > 1, "s", ""),
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
            format (dp.nb / elm.nb, digits = 3),
            ")\n",
            sep = ""
        )
        print (summary (dc$dp[[dc.name]]$vars[[var]]))
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
        for (var in names (dc$dp[[dp.name]]$vars)) {
            cat (" - '", var, "' variable:\n", sep = "")
            print (summary (dc$dp[[dp.name]]$vars[[var]]))
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
              var.name,
              dim.names) {
        dp.dim.names <- dc$var.dim.names[[var.name]]
        dp.name <- paste (dp.dim.names, collapse = ".")
        new.dp.name <- paste (dc$dim.names [dc$dim.names %in% dim.names], collapse = ".")

        agg <- aggregate (
            list (var.name = dc$dp[[dp.name]]$vars[[var.name]]),
            by = dc$dp[[dp.name]]$elms[dim.names],
            FUN = dc$var.FUN[[var.name]]
        )
        names (agg) [names (agg) == "var.name"] <- var.name

        agg <- merge (agg, cbind (as.data.frame (dc$dp[[new.dp.name]]$elms), as.data.frame (dc$dp[[new.dp.name]]$vars)), by = dim.names, all = TRUE)

        dc$dp[[new.dp.name]]$elms <- as.list (agg [, dim.names, drop = FALSE])
        dc$dp[[new.dp.name]]$vars <- as.list (agg [, ! names (agg) %in% dim.names, drop = FALSE])

        for (var.name in names (dc$dp[[new.dp.name]]$vars)) {
            dc$dp[[new.dp.name]]$vars[[var.name]] [is.na (dc$dp[[new.dp.name]]$vars[[var.name]])] <- dc$var.NA[[var.name]]
        }
        
        return (dc)
    }

compute.margin <-
    function (obj, ...) {
        UseMethod ("compute.margin")
    }
compute.margin.data.cube <- function (dc, var.name, ..., recursive = FALSE) {
    var.name <- deparse (substitute (var.name))
    dim.names <- sapply (eval (substitute (alist (...))), deparse)
    compute.margin_(dc, var.name, dim.names)
}


## compute.margin_ <-
##     function (obj, ...) {
##         UseMethod ("compute.margin_")
##     }
## compute.margin_.data.cube <-
##     function (dc,
##               dim = list(),
##               recursive = FALSE) {
##         dc.name <- paste (dc$dim.names, collapse = ".")
        
##         ## Compute global margin if necessary
##         if (length (dim) == 0 || recursive) {
##             dc$dp[["."]] <- list()
##             dc$dp[["."]]$elms <- list()
##             dc$dp[["."]]$vars <-
##                 lapply (dc$dp[[dc.name]]$vars, function (var)
##                     sum (var))
##             if (length (dim) == 0)
##                 return (dc)
##         }
        
##         ## List recursive margins if necessary
##         if (recursive) {
##             dim <-
##                 unlist (sapply (1:length (dim), function (n)
##                     combn (dim, n, simplify = FALSE)), recursive = FALSE)
##         } else {
##             dim <- list (dim)
##         }
        
##         ## Compute all requested margins (except global)
##         for (d in dim) {
##             if (length (d) == dc$dim.nb)
##                 next
            
##             d <- dc$dim.names [dc$dim.names %in% d]
##             dp.name <- paste (d, collapse = ".")
##             agg <-
##                 aggregate (dc$dp[[dc.name]]$vars, by = dc$dp[[dc.name]]$elms[d], FUN =
##                                                                                      sum)
##             rank <-
##                 aggregate (
##                     x = seq_along(dc$dp[[dc.name]]$elms[d][[1]]),
##                     by = dc$dp[[dc.name]]$elms[d],
##                     FUN = min
##                 )[, "x"]
            
##             dc$dp[[dp.name]] <- list ()
##             dc$dp[[dp.name]]$elms <-
##                 as.list (agg[order(rank), d, drop = FALSE])
##             dc$dp[[dp.name]]$vars <-
##                 as.list (agg[order(rank), !names(agg) %in% d, drop = FALSE])
##         }
        
##         return (dc)
##     }

## compute.margin <-
##     function (obj, ...) {
##         UseMethod ("compute.margin")
##     }
## compute.margin.data.cube <- function (dc, ..., recursive = FALSE) {
##     dim <- sapply (eval (substitute (alist (...))), deparse)
##     compute.margin_(dc, dim, recursive)
## }




str(dc)
dim.names <- c('b','a')
var.names <- c('v3','v2')

select_.data.cube <- function (dc, dim.names, var.names) {
    dp.name <- paste (dc$dim.names [dc$dim.names %in% dim.names], collapse = ".")
    for (var.name in var.names) {
        if (! var.name %in% names (dc$dp[[dp.name]]$vars)) { dc <- compute.margin_(dc, var.name, dim.names) }
    }

    dc$dim.nb <- length (dim.names)
}


?select_
select.dim_ <- function (obj, ...) {
    UseMethod ("select.dim_")
}
select.dim_.data.cube <- function (dc, dim) {
    ## Compute corresponding data plane
    old.dim <- dc$dim.names [dc$dim.names %in% dim]
    old.dp.name <- paste (old.dim, collapse = ".")
    if (is.null (dc$dp[[old.dp.name]]))
        dc <- dc %>% compute.margin_(dim)
    
    ## Adjust other data cube attributes
    dc$dim.nb <- length (dim)
    dc$dim.names <- dim
    dc$elm.nb <- dc$elm.nb[dim]
    dc$elm.names <- dc$elm.names[dim]
    
    ## Adjust all data planes
    for (dp.name in names (dc$dp)) {
        if (dp.name == ".")
            next
        d <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
        if (!all (d %in% dim))
            dc$dp[[dp.name]] <- NULL
        else {
            ## Adjust elements
            new.dim <- dc$dim.names[dc$dim.names %in% d]
            new.dp.name <- paste (new.dim, collapse = ".")
            names (dc$dp) [names (dc$dp) == dp.name] <-
                              new.dp.name
            dc$dp[[new.dp.name]]$elms <-
                dc$dp[[new.dp.name]]$elms[new.dim]
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
        
        for (dp.name in names (dc$dp)) {
            if (dp.name == ".")
                next
            d <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
            if (dim %in% d) {
                keep <- dc$dp[[dp.name]]$elms[[dim]] %in% elm.indices
                
                dc$dp[[dp.name]]$elms <-
                    lapply (dc$dp[[dp.name]]$elms, function (d)
                        d [keep])
                dc$dp[[dp.name]]$vars <-
                    lapply (dc$dp[[dp.name]]$vars, function (d)
                        d [keep])
                dc$dp[[dp.name]]$elms[[dim]] <-
                    new.indices [dc$dp[[dp.name]]$elms[[dim]]]
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
            
            if (is.null (dc$dp[[dim]]))
                dc <- compute.margin_(dc, dim)
            
            keep <- eval (filter, dc$dp[[dim]]$vars)
            elm.indices <- dc$dp[[dim]]$elms[[dim]] [keep]
        } else {
            var <- deparse (substitute (var))
            if (var == "NULL")
                var <- dc$var.names[1]
            
            if (is.null (dc$dp[[dim]]) ||
                is.null (dc$dp[[dim]]$vars[[var]]))
                dc <- compute.margin_(dc, dim)
            
            if (!is.null (top.nb))
                elm.indices <-
                    head (dc$dp[[dim]]$elms[[dim]] [order (dc$dp[[dim]]$vars[[dc$var.names[1]]], decreasing =
                                                                                                     TRUE)], top.nb)
            else if (!is.null (bot.nb))
                elm.indices <-
                    tail (dc$dp[[dim]]$elms[[dim]] [order (dc$dp[[dim]]$vars[[dc$var.names[1]]], decreasing =
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
    
    keep <- eval (condition, dc$dp[[dc.name]]$vars)
    dc$dp[[dc.name]]$elms <-
        lapply (dc$dp[[dc.name]]$elms, function (dim)
            dim [keep])
    dc$dp[[dc.name]]$vars <-
        lapply (dc$dp[[dc.name]]$vars, function (dim)
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
                if (is.null (dc$dp[[d]]))
                    dc$dp[[d]] <- compute.margin_(dc, d)
                if (decreasing) {
                    new.indices[[d]] <-
                        rank (desc (dc$dp[[d]]$vars[[var]]), ties.method = "first")
                }
                else {
                    new.indices[[d]] <-
                        rank (dc$dp[[d]]$vars[[var]], ties.method = "first")
                }
                new.indices[[d]] <-
                    new.indices[[d]] [order (dc$dp[[d]]$elms[[d]])]
                dc$elm.names[[d]] <-
                    dc$elm.names[[d]] [dc$dp[[d]]$elms[[d]] [order (dc$dp[[d]]$vars[[var]], decreasing =
                                                                                                decreasing)]]
            }
        }
        
        for (dp.name in names (dc$dp)) {
            if (dp.name == ".")
                next
            dim2 <- strsplit (dp.name, ".", fixed = TRUE)[[1]]
            for (d in rev (dim)) {
                if (!d %in% dim2)
                    next
                dc$dp[[dp.name]]$elms[[d]] <-
                    new.indices[[d]] [dc$dp[[dp.name]]$elms[[d]]]
                order <- order (dc$dp[[dp.name]]$elms[[d]])
                dc$dp[[dp.name]]$elms <-
                    lapply (dc$dp[[dp.name]]$elms, function (dim)
                        dim [order])
                dc$dp[[dp.name]]$vars <-
                    lapply (dc$dp[[dp.name]]$vars, function (dim)
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


                                        # arrange.obs_<- function (obj, ...) { UseMethod ("arrange.obs_") }
                                        # arrange.obs_.data.cube <- function (dc, var=dc$var.names[1], decreasing=TRUE) {
                                        #     dc.name <- paste (dc$dim.names, collapse=".")
                                        #     order <- order (dc$dp[[dc.name]]$vars[[var]], decreasing=decreasing)
                                        #     dc$dp[[dc.name]]$elms <- lapply (dc$dp[[dc.name]]$elms, function (dim) dim [order])
                                        #     dc$dp[[dc.name]]$vars <- lapply (dc$dp[[dc.name]]$vars, function (dim) dim [order])
                                        #
                                        #     return (dc)
                                        # }
                                        #
                                        # arrange.obs <- function (obj, ...) { UseMethod ("arrange.obs") }
                                        # arrange.obs.data.cube <- function (dc, var=NULL, decreasing=TRUE) {
                                        #     var <- deparse (substitute (var))
                                        #     if (var == "NULL") var <- dc$var.names[1]
                                        #     arrange.obs_(dc, var=var, decreasing=decreasing)
                                        # }



plot.obs_ <- function (obj, ...) {
    UseMethod ("plot.obs_")
}
plot.obs_.data.cube <-
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
            if (var == "ratio")
                df$ratio <- df$ratio - 1
            if (var == "deviation" &&
                dc$model$type == "ratio")
                df$deviation <- df$deviation - 1
            
            p <- ggplot (df, aes (x = label, y = get (var)))
            
            if (is.null (sep.dim))
                p <- p + geom_col ()
            
            else
                p <-
                    p + geom_col (aes (fill = factor (get (sep.dim), levels = dc$elm.names[[sep.dim]])), position = "dodge") +
                    guides (fill = guide_legend (title = sep.dim))
            
            if (var == "ratio" ||
                var == "deviation" && dc$model$type == 'ratio')
                p <-
                    p + scale_y_continuous (
                            labels = function (x)
                                x + 1
                        )
        }
        
        if (type == "line") {
            indices <- unique (df[, ununiq.dim, drop = FALSE])
            indices$row <- 1:nrow(indices)
            indices$label <-
                apply (indices, 1, function (row)
                    paste (row[ununiq.dim], collapse = " / "))
            df$index <-
                merge (df[, ununiq.dim, drop = FALSE], indices)$row
            
            if (is.null (sep.dim))
                p <- ggplot (df, aes (x = index, y = get (var)))
            
            else
                p <-
                    ggplot (df, aes (
                                    x = index,
                                    y = get (var),
                                    color = factor (get (sep.dim), levels = rev (dc$elm.names[[sep.dim]]))
                                )) +
                    guides (color = guide_legend (title = sep.dim))
            
            p <- p + geom_line () +
                scale_x_continuous (breaks = 1:nrow(indices),
                                    labels = indices$label)
            
            if (var == "ratio" ||
                var == "deviation" && dc$model$type == 'ratio')
                p <- p + geom_hline (yintercept = 1)
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


plot.obs <- function (obj, ...) {
    UseMethod ("plot.obs")
}
plot.obs.data.cube <-
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
        
        plot.obs_(dc, var, type = type, sep.dim = sep.dim)
    }


biplot.obs_ <- function (obj, ...) {
    UseMethod ("biplot.obs_")
}
biplot.obs_.data.cube <- function (dc, x.dim, y.dim, var) {
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


biplot.obs <- function (obj, ...) {
    UseMethod ("biplot.obs")
}
biplot.obs.data.cube <- function (dc, x.dim, y.dim, var = NULL) {
    x.dim <- deparse (substitute (x.dim))
    y.dim <- deparse (substitute (y.dim))
    
    var <- deparse (substitute (var))
    if (var == "NULL")
        var <- dc$var.names[1]
    
    biplot.obs_(dc, x.dim, y.dim, var = var)
}



plot.outlier <- function (obj, ...) {
    UseMethod ("plot.outlier")
}
plot.outlier.data.cube <- function (dc, labels = TRUE) {
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
            if (is.null (dc$dp[[dp.name]]))
                dc <- dc %>% compute.margin_(d)
        }
        
        if (is.null (dc$dp[["."]]))
            dc <- dc %>% compute.margin_()
        sum <- dc$dp[["."]]$vars[[var.name]]
        
        ## Compute model
        model <- rep (sum, length (dc$dp[[dc.name]]$vars[[var.name]]))
        
        for (d in dim) {
            dp.name <- paste (d, collapse = ".")
            dc.data <-
                append (list (1:length(dc$dp[[dc.name]]$var[[var.name]])), dc$dp[[dc.name]]$elms[d])
            names (dc.data) [1] <- "row"
            dp.data <-
                append (dc$dp[[dp.name]]$elms, dc$dp[[dp.name]]$vars[var.name])
            
            dist <- merge (dc.data, dp.data)
            dist <- dist [order (dist$row), var.name] / sum
            model <- model * dist
        }
        
        other.dim <- dc$dim.names [!dc$dim.names %in% dim]
        for (d in other.dim)
            model <- model / dc$elm.nb[[d]]
        
        dc$dp[[dc.name]]$vars[["model"]] <- model
        dc$var.names <- unique (append (dc$var.names, "model"))
        
        ## Compute deviation
        dc$dp[[dc.name]]$vars[["ratio"]] <-
            dc$dp[[dc.name]]$vars[[var.name]] / dc$dp[[dc.name]]$vars[["model"]]
        dc$var.names <- unique (append (dc$var.names, "ratio"))
        
        if (deviation.type == "ratio") {
            dc$dp[[dc.name]]$vars[["deviation"]] <-
                dc$dp[[dc.name]]$vars[["ratio"]]
        }
        
        if (deviation.type == "poisson") {
            dc$dp[[dc.name]]$vars[["deviation"]] <- ifelse (
                dc$dp[[dc.name]]$vars[[var.name]] < dc$dp[[dc.name]]$vars[["model"]],
                ppois (
                    dc$dp[[dc.name]]$vars[[var.name]],
                    dc$dp[[dc.name]]$vars[["model"]],
                    lower.tail = TRUE,
                    log.p = TRUE
                ),
                -ppois (
                     dc$dp[[dc.name]]$vars[[var.name]],
                     dc$dp[[dc.name]]$vars[["model"]],
                     lower.tail = FALSE,
                     log.p = TRUE
                 )
            )
        }
        
        if (deviation.type == "KLdiv") {
            dc$dp[[dc.name]]$vars[["deviation"]] <-
                dc$dp[[dc.name]]$vars[[var.name]] / sum * log2 (dc$dp[[dc.name]]$vars[[var.name]] / dc$dp[[dc.name]]$vars[["model"]])
        }
        
        dc$var.names <- unique (append (dc$var.names, "deviation"))
        
        ## Apply threshold
        dev.mean <- mean (dc$dp[[dc.name]]$vars[["deviation"]])
        dev.sd <- sd (dc$dp[[dc.name]]$vars[["deviation"]])
        
        if (is.na (dev.sd)) {
            df$outlier <-
                rep (0, length (dc$dp[[dc.name]]$vars[["deviation"]]))
        }
        else {
            dc$dp[[dc.name]]$vars[["outlier"]] <-
                findInterval (dc$dp[[dc.name]]$vars[["deviation"]],
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
    function (dc, ..., deviation.type = "ratio") {
        dim <- sapply (eval (substitute (alist (...))), deparse)
        compute.model_(dc, dim, deviation.type = deviation.type)
    }





## Print data summary
data.summary <- function (obj, ...) {
    UseMethod ("data.summary")
}
data.summary.data.cube <- function (dc, data = "obs") {
    summary (dc$dp[[dc.name]]$vars[[data]])
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
        p <-
            ggplot (as.data.frame (dc$dp[[dc.name]]$vars), aes (x = get(data))) +
            geom_histogram (
                bins = 100,
                aes (y = ..count..),
                color = "black",
                fill = "blue",
                alpha = 0.3
            ) +
            ## stat_density (aes (y=..count..), color="black", fill="blue", alpha=0.3) +
            theme_bw () #+ theme (text=elm_text (size=20))
        
        title <- paste ("Distribution of", data)
        
        if (log == "x") {
            logmax <- ceiling(log10(max(dc$dp[[dc.name]]$vars[[data]])))
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
            logmax <- ceiling(log10(max(dc$dp[[dc.name]]$vars[[data]])))
            breaks <- 10 ^ seq (0, logmax, logmax / 10)
            title <- paste (title, "(logarithmic scales)")
            
            p <- p + scale_x_continuous (trans = "log", breaks = breaks) +
                scale_y_continuous (trans = "log")
        }
        
        if (!is.null (threshold)) {
            data.mean <- mean (dc$dp[[dc.name]]$vars[[data]])
            data.sd <- sd (dc$dp[[dc.name]]$vars[[data]])
            
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
            as.data.frame (cbind (as.data.frame (dc$dp[[dc.name]]$elms), as.data.frame (dc$dp[[dc.name]]$vars)))
        if (!is.null (display)) {
            data.frame <- data.frame[data.frame[[display]],]
        }
        
        data.frame$ratio <- data.frame$dp / data.frame$exp
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
