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
library ("reshape2")
library ("rlang")
library ("ggrepel")

#library ("sticky")
#append <- base::append


arg.names <- function (args) {
    if (deparse (args) == "c()") { return (character(0)) }
    if (deparse (args) == "list()") { return (character(0)) }
    if (is.null (args)) { return (NULL) }
    names <- sapply (args, deparse)
    if (length (names) > 1) { names <- names [-1] }
    return (names)
}


dot.names <- function (dots) {
    if (length (dots) > 0 && is.null (get_expr (dots [[1]]))) { return (NULL) }
    names <- sapply (dots, rlang::quo_name)
    if (length (names) == 0) { return (character(0)) }
    return (names)
}



dim.names <- function (obj, ...) { UseMethod ("dim.names") }
dim.names.data.cube <- function (dc) { append (character(0), names (attr (dc, "dims"))) }

dim.nb <- function (obj, ...) { UseMethod ("dim.nb") }
dim.nb.data.cube <- function (dc) { length (attr (dc, "dims")) }

dims <- function (obj, ...) { UseMethod ("dims") }
dims.data.cube <- function (dc, dim.names = dim.names.data.cube (dc), drop = TRUE)
{
    dims <- attr (dc, "dims") [dim.names]
    if (length (dims) == 1 && drop) { return (dims[[1]]) } else { return (dims) }
}

`dims<-` <- function (obj, ...) { UseMethod ("dims<-") }
`dims<-.data.cube` <- function (dc, dim.names = dim.names.data.cube (dc), value) {
    
    if (length (dim.names) == 0) {
        attr (dc, "dims") <- value
    } else if (length (dim.names) == 1) {
        attr (dc, "dims") [[dim.names]] <- value
    } else {
        for (dim.name in dim.names) {
            attr (dc, "dims") [[dim.name]] <- value [[dim.name]]
        }
    }
    return (dc)
}


vars <- function (obj, ...) { UseMethod ("vars") }
vars.data.cube <- function (dc, var.names = var.names.data.cube (dc), drop = TRUE)
{
    vars <- attr (dc, "vars") [var.names]
    if (length (vars) == 1 && drop) { return (vars[[1]]) } else { return (vars) }
}

`vars<-` <- function (obj, ...) { UseMethod ("vars<-") }
`vars<-.data.cube` <- function (dc, var.names = var.names.data.cube (dc), value) {
    
    if (length (var.names) == 0) {
        attr (dc, "vars") <- value
    } else if (length (var.names) == 1) {
        attr (dc, "vars") [[var.names]] <- value
    } else {
        for (var.name in var.names) {
            attr (dc, "vars") [[var.name]] <- value [[var.name]]
        }
    }
    return (dc)
}

var.names <- function (obj, ...) { UseMethod ("var.names") }
var.names.data.cube <- function (dc) { append (character(0), names (attr (dc, "vars"))) }

var.nb <- function (obj, ...) { UseMethod ("var.nb") }
var.nb.data.cube <- function (dc) { length (attr (dc, "vars")) }


plane.name <- function (obj, ...) { UseMethod ("plane.name") }
plane.name.data.cube <- function (dc, dim.names = dim.names.data.cube (dc)) {
    if (length (dim.names) == 0) { return ("$") }
    paste (dim.names (dc) %>% intersect (dim.names), collapse = "$")
}

plane <- function (obj, ...) { UseMethod ("plane") }
plane.data.cube <- function (dc, dim.names = dim.names.data.cube (dc)) { dc [[plane.name (dc, dim.names)]] }

`plane<-` <- function (obj, ...) { UseMethod ("plane<-") }
`plane<-.data.cube` <- function (dc, dim.names = dim.names.data.cube (dc), value) {
    dc [[plane.name (dc, dim.names)]] <- value
    return (dc)
}


plane.attributes <- function (dp) {
    attrs <- attributes (dp)
    attrs [names (attrs) %>% intersect (c ("class", "dim.names", "arrange.var.names"))]
}

`plane.attributes<-` <- function (dp, value) {
    for (attr.name in names (value)) {
        attr (dp, attr.name) <- value [[attr.name]]
    }
    return (dp)
}


data.plane.dim.names <- function (dp.name) {
    if (dp.name == "$") { return (character(0)) }
    strsplit (dp.name, "$", fixed = TRUE) [[1]]
}

data.plane.dim <- function (dp.name) {
    if (dp.name == "$") { return (0) }
    str_count (dp.name, fixed ("$")) + 1
}


empty.plane <- function (dim.names = character(0)) {
    dp <- list()
    if (length (dim.names) > 0) { dp <- dp %>% as_tibble }
    for (dim.name in dim.names) { dp[[dim.name]] <- integer(0) }
    class (dp) <- append ("data.plane", class (dp))
    attr (dp, "dim.names") <- dim.names
    return (dp)
}
    
var.NA <- function (var.class) { sapply (1, var.class) }

var.FUN <- function (var.class) {
    switch (var.class,
            integer = sum,
            double = sum,
            numeric = sum,
            logical = all,
            character = function (...) paste (..., collapse = ","),
            ## character = paste (..., collapse = ","),
            NA
            )
}


as.data.cube <- function (obj, ...) { UseMethod ("as.data.cube") }
as.data.cube.data.frame <-
    function (df,
              dim.names = NULL,
              var.names = NULL) {
        
        str.dim.names <- arg.names (substitute (dim.names))
        if (is.null (str.dim.names)) { str.dim.names <- character(0) }

        str.var.names <- arg.names (substitute (var.names))
        if (is.null (str.var.names)) { str.var.names <- names (df) [! names (df) %in% str.dim.names] }

        df %>% as.data.cube_(str.dim.names, str.var.names)
    }


as.data.cube_ <- function (obj, ...) { UseMethod ("as.data.cube_") }
as.data.cube_.data.frame <-
    function (df,
              dim.names = character(0),
              var.names = names (df) [! names (df) %in% dim.names]) {
        
        if (length (dim.names) == 0) { dim.names <- character(0) }
        
        df <- as_tibble (df) %>% select (append (dim.names, var.names))
        if (length (dim.names) > 0 && nrow (unique (df [, dim.names, drop = FALSE])) < nrow (df)) {
            stop ("input data.frames first need to be aggregated")
        }
        
        ## Build data.cube
        dc <- list ()
        class (dc) <- append ("data.cube", class (dc))

        ## Set attributes
        dims (dc) <- list ()
        for (dim.name in dim.names) {
            dim <- dim.name
            class (dim) <- "dimension"            
            dims (dc, dim.name) <- dim
        }
        
        vars (dc) <- list ()
        for (var.name in var.names) {
            var <- var.name
            class (var) <- "variable"
            attr (var, "dim.names") <- dim.names (dc)
            attr (var, "NA.value") <- var.NA (class (df [, var.name, drop = TRUE]))
            attr (var, "FUN.value") <- var.FUN (class (df [, var.name, drop = TRUE]))            
            vars (dc, var.name) <- var
        }

        ## Fill observations
        for (dim.name in dim.names (dc)) {
            elm.list <- df [[dim.name]]
            fct.list <- factor (elm.list, levels = unique (elm.list))

            ## Build min data.plane
            dp <- list (name = levels (fct.list)) %>%
                as_tibble () %>% tibble::rowid_to_column (dim.name)

            class (dp) <- append ("data.plane", class (dp))
            attr (dp, "dim.names") <- dim.name

            plane (dc, dim.name) <- dp
            
            ## Update data.frame indices
            df [, dim.name] <- as.integer (fct.list)
        }
        
        ## Build or merge max data.plane
        if (dim.nb (dc) == 0) {
            dp <- as.list (df)
            class (dp) <- append ("data.plane", class (dp))
        } else if (dim.nb (dc) == 1) {
            dp <- inner_join (plane (dc), df, by = dim.names)
        } else {
            dp <- df
            class (dp) <- append ("data.plane", class (dp))
        }

        attr (dp, "dim.names") <- unname (dim.names)
        plane (dc) <- dp

        if (! is.null (names (dim.names))) { dc <- dc %>% rename.dim_(dim.names [names (dim.names) != ""]) }
        if (! is.null (names (var.names))) { dc <- dc %>% rename.var_(var.names [names (var.names) != ""]) }

        return (dc)
    }




reorder.dim <- function (obj, ...) { UseMethod ("reorder.dim") }
reorder.dim.data.cube <-
    function (dc, ...) {
        str.dim.names <- dot.names (enquos (...))
        dc %>% reorder.dim_(str.dim.names)
    }


reorder.dim_ <- function (obj, ...) { UseMethod ("reorder.dim_") }
reorder.dim_.data.cube <- function (dc, dim.names = character(0)) {
    
    dim.names <- dim.names %>% intersect (dim.names (dc)) %>% append (dim.names (dc) %>% setdiff (dim.names))
    
    ## Reorder dims
    dims (dc, character(0)) <- dims (dc, dim.names, drop = FALSE)

    ## Reorder dim.names in vars
    for (var.name in var.names (dc)) {
        attr (vars (dc, var.name), "dim.names") <- dim.names %>% intersect (attr (vars (dc, var.name), "dim.names"))
    }

    ## Reorder dp.names
    names (dc) <- sapply (names (dc), function (dp.name) plane.name (dc, data.plane.dim.names (dp.name)))

    ## Reorder dim.names in dp
    for (dp.name in names (dc)) {
        if (dp.name != "$") {
            attr (dc[[dp.name]], "dim.names") <- dim.names %>% intersect (attr (dc[[dp.name]], "dim.names"))
            dc[[dp.name]] <- dc[[dp.name]] %>% select (!! dim.names (dc) %>% intersect (names (dc[[dp.name]])) %>% append (names (dc[[dp.name]]) %>% setdiff (dim.names (dc))))
        }
    }
    
    ## Reorder dp
    dp.names <- names (dc)
    for (d in seq_along (dim.names (dc))) { dp.names <- str_replace (dp.names, dim.names (dc)[[d]], as.character (d)) }
    dp.names <- names (dc) [order (dp.names)]
    dp.names <- dp.names [order (sapply (dp.names, data.plane.dim))]

    attrs <- attributes (dc)
    dc <- dc [dp.names]
    attrs[["names"]] <- names (dc)
    attributes (dc) <- attrs
    
    ## if (! is.null (dc$dp.arrange.var.names)) {
    ##     names (dc$dp.arrange.var.names) <- sapply (names (dc$dp.arrange.var.names), function (dp.name) plane.name (dc, data.plane.dim.names (dp.name)))
    ## }

    return (dc)
}


reorder.var <- function (obj, ...) { UseMethod ("reorder.var") }
reorder.var.data.cube <-
    function (dc, ...) {
        str.var.names <- dot.names (enquos (...))
        dc %>% reorder.var_(str.var.names)
    }


reorder.var_ <- function (obj, ...) { UseMethod ("reorder.var_") }
reorder.var_.data.cube <- function (dc, var.names = character(0)) {
    
    var.names <- var.names %>% intersect (var.names (dc)) %>% append (var.names (dc) %>% setdiff (var.names))
    
    ## Reorder vars
    vars (dc, character(0)) <- vars (dc, var.names, drop = FALSE)

    ## Reorder var.names in data.planes
    for (dp.name in names (dc)) {
        new.var.names <- var.names (dc) %>% intersect (names (dc[[dp.name]]))

        if (dp.name == "$") {
            attrs <- plane.attributes (dc[[dp.name]])
            dc[[dp.name]] <- dc[[dp.name]] [new.var.names]
            plane.attributes (dc[[dp.name]]) <- attrs
        } else {
            dc[[dp.name]] <- dc[[dp.name]] %>%
                select (names (dc[[dp.name]]) %>% setdiff (var.names (dc)) %>% append (new.var.names))
        }
    }

    return (dc)
}


rename.dim <- function (obj, ...) { UseMethod ("rename.dim") }
rename.dim.data.cube <-
    function (dc, ...) {
        str.dim.names <- dot.names (enquos (...))
        dc %>% rename.dim_(str.dim.names)
    }


rename.dim_ <- function (obj, ...) { UseMethod ("rename.dim_") }
rename.dim_.data.cube <- function (dc, dim.names) {
    
    ## Get renaming table
    new.dim.names <- setNames (dim.names (dc), dim.names (dc))
    new.dim.names <- sapply (new.dim.names, function (dim.name) ifelse (dim.name %in% dim.names, names (dim.names) [dim.names == dim.name], dim.name))
    
    ## Rename dim attributes
    for (dim.name in dim.names (dc)) {
        dim <- unname (new.dim.names [dim.name])
        attributes (dim) <- attributes (dims (dc, dim.name))
        dims (dc, dim.name) <- dim
    }
    names (attr (dc, "dims")) <- unname (new.dim.names [dim.names (dc)])

    ## Rename var attributes
    for (var.name in var.names (dc)) {
        old.dim.names <- attr (vars (dc, var.name), "dim.names")
        attr (vars (dc, var.name), "dim.names") <- unname (new.dim.names [old.dim.names])
    }

    ## Rename dim.names in dp.names    
    names (dc) <- sapply (names (dc), function (dp.name) plane.name (dc, unname (new.dim.names [data.plane.dim.names (dp.name)])))

    ## if (! is.null (dc$dp.arrange.var.names)) {
    ##     names (dc$dp.arrange.var.names) <- sapply (names (dc$dp.arrange.var.names), function (dp.name) plane.name (dc, unname (new.dim.names [data.plane.dim.names (dp.name)])))
    ## }

    ## Rename dim.names in dp
    for (dp.name in names (dc)) {
        if (dp.name != "$") {            
            attr (dc[[dp.name]], "dim.names") <- unname (new.dim.names [attr (dc[[dp.name]], "dim.names")])
            dc[[dp.name]] <- dc[[dp.name]] %>% rename (!!! dim.names [dim.names %in% names (dc[[dp.name]])])
        }
    }

    return (dc)
}


rename.var <- function (obj, ...) { UseMethod ("rename.var") }
rename.var.data.cube <-
    function (dc, ...) {
        str.var.names <- dot.names (enquos (...))        
        dc %>% rename.var_(str.var.names)
    }


rename.var_ <- function (obj, ...) { UseMethod ("rename.var_") }
rename.var_.data.cube <- function (dc, var.names) {

    ## Get renaming tables
    new.var.names <- setNames (var.names (dc), var.names (dc))
    new.var.names <- sapply (new.var.names, function (var.name) ifelse (var.name %in% var.names, names (var.names) [var.names == var.name], var.name))

    ## Rename attributes
    for (var.name in var.names (dc)) {
        var <- unname (new.var.names [var.name])
        attributes (var) <- attributes (vars (dc, var.name))
        vars (dc, var.name) <- var
    }
    names (attr (dc, "vars")) <- unname (new.var.names [var.names (dc)])

    ## Rename var.names in dp
    for (dp.name in names (dc)) {
        if (dp.name != "$") {
            dc[[dp.name]] <- dc[[dp.name]] %>% rename (!!! var.names [var.names %in% names (dc[[dp.name]])])
        } else {
            names (dc[[dp.name]]) <- new.var.names [names (dc[[dp.name]])]
        }

        ## Rename arrange.var.names
        if (! is.null (attr (dc[[dp.name]], "arrange.var.names"))) {
            for (i in seq_along (attr (dc[[dp.name]], "arrange.var.names"))) {
                var.name <- attr (dc[[dp.name]], "arrange.var.names") [i]
                f <- function () {}
                body (f) <- parse_expr (var.name)
                struct <- codetools::findGlobals (f, merge = FALSE)
                struct$variables <- ifelse (struct$variable %in% names (new.var.names), new.var.names [struct$variable], struct$variable)
                if (length (struct$functions) > 0) {
                    attr (dc[[dp.name]], "arrange.var.names") [i] <- paste0 (struct$functions, "(", paste (struct$variables, collapse = ","), ")")
                } else {
                    attr (dc[[dp.name]], "arrange.var.names") [i] <- paste (struct$variables, collapse = ",")
                }
            }
        }
    }

    return (dc)
}


join <- function (obj, ...) { UseMethod ("join") }
join.data.cube <- function (dc, ...) {

    for (add.dc in list (...)) {
        
        ## Update attributes
        ## TODO: check if variables are unique
        
        dims (dc, character(0)) <- append (dims (dc, drop = FALSE), dims (add.dc, drop = FALSE) [dim.names (add.dc) %>% setdiff (dim.names (dc))])
        vars (dc, character(0)) <- append (vars (dc, drop = FALSE), vars (add.dc, drop = FALSE))
        
        ## Join min data.planes
        for (dim.name in dim.names (add.dc)) {
            if (is.null (plane (dc, dim.name))) {
                plane (dc, dim.name) <- plane (add.dc, dim.name)
            } else {
                joint.df <- full_join (plane (dc, dim.name), plane (add.dc, dim.name), by = "name", suffix = c ("", ".mod"))
                attr.names <- names (attributes (plane (dc, dim.name))) %>% setdiff (c ("names", "row.names", "class"))
                attr (joint.df, attr.names) <- attr (plane (dc, dim.name), attr.names)
                
                mod.dim.name <- paste0 (dim.name, ".mod")
                plane (dc, dim.name) <- joint.df %>% select (names (joint.df) %>% setdiff (mod.dim.name))
                indice.table <- joint.df %>% arrange_(mod.dim.name) %>% pull (dim.name)

                ## Update element indices
                for (dp.name in names (attr (add.dc, "planes"))) {
                    dp.dim.names <- data.plane.dim.names (dp.name)

                    if (dim.name %in% dp.dim.names && length (dp.dim.names) > 1) {
                        expr <- parse_expr (paste0 ("indice.table [", dim.name, "]"))
                        plane (add.dc, dp.name) <- plane (add.dc, dp.name) %>% mutate (!! dim.name := !! expr)
                    }
                }
            }
        }
        
        ## Join other data.planes
        for (dp.name in names (add.dc)) {
            dp.dim.names <- data.plane.dim.names (dp.name)

            if (length (dp.dim.names) != 1) {
                
                new.dp.name <- plane.name (dc, dp.dim.names)

                if (is.null (plane (dc, dp.dim.names))) {
                    plane (dc, dp.dim.names) <- add.dc[[dp.name]]
                } else {
                    if (length (dp.dim.names) == 0) {
                        plane (dc, dp.dim.names) <- append (plane (dc, dp.dim.names), add.dc[[dp.name]])
                    } else {
                        plane (dc, dp.dim.names) <- full_join (plane (dc, dp.dim.names), add.dc[[dp.name]], by = dp.dim.names)
                    }
                    
                    ## Replace NA values that appeared
                    ## TODO: replace only NA value that appeared (not genuine NA values)
                    new.var.names <- names (plane (dc, dp.dim.names)) %>% intersect (var.names (dc))
                    plane (dc, dp.dim.names) <- plane (dc, dp.dim.names) %>% replace_na (lapply (vars (dc, new.var.names, drop = FALSE), function (var) attr (var, "NA.value")))
                }
            }
        }
    }

    dc <- dc %>% reorder.dim_() %>% reorder.var_()

    return (dc)
}


is.data.cube <- function (obj) {
    inherits (obj, "data.cube")
}


as.data.frame <- function (obj, ...) UseMethod ("as.data.frame")
as.data.frame.data.cube <- function (dc, complete = FALSE, stringsAsFactors = FALSE) {
    
    if (complete) { dc <- dc %>% complete.elm_(dim.names (dc)) }
    dc <- dc %>% apply.arrange.elm_(dim.names (dc))

    dp.name <- plane.name (dc)

    if (is.null (dc[[dp.name]])) { df <- empty.plane (dim.names (dc)) } else { df <- dc[[dp.name]] }

    ## Replace element indices by element names
    for (dim.name in dim.names (dc)) {
        elm.names <- plane (dc, dim.name)$name [order (plane (dc, dim.name)[[dim.name]])]
        if (stringsAsFactors) {
            df[[dim.name]] <- factor (elm.names [df[[dim.name]]], levels = plane (dc, dim.name)$name)
        } else {
            df[[dim.name]] <- elm.names [df[[dim.name]]]
        }
    }

    return (df)
}


compute.var <- function (obj, ...) { UseMethod ("compute.var") }
compute.var.data.cube <-
    function (dc, dim.names, ...) {
        str.dim.names <- arg.names (substitute (dim.names))

        str.var.names <- dot.names (enquos (...))
        if (length (str.var.names) == 0) { str.var.names <- var.names (dc) }

        dc %>% compute.var_(str.dim.names, str.var.names)
    }


compute.var_ <- function (obj, ...) { UseMethod ("compute.var_") }
compute.var_.data.cube <-
    function (dc,
              dim.names = character(0),
              var.names = var.names.data.cube (dc)) {
        
        ## Get var.names
        if (is.null (names (var.names))) { names (var.names) <- var.names }
        names (var.names) <- ifelse (names (var.names) != "", names (var.names), var.names)
        
        sup.var.names <- var.names [var.names %in% var.names (dc) [sapply (vars (dc, drop = FALSE), function (var) all (dim.names %in% attr (var, "dim.names")) && ! all (attr (var, "dim.names") %in% dim.names))]]
        sub.var.names <- var.names [var.names %in% var.names (dc) [sapply (vars (dc, drop = FALSE), function (var) all (attr (var, "dim.names") %in% dim.names) && ! all (dim.names %in% attr (var, "dim.names")))]]

        if (is.null (plane (dc, dim.names))) { plane (dc, dim.names) <- empty.plane (dim.names) }
        
        if (length (dim.names) == 0) { ## If grand total

            for (to.var.name in names (sup.var.names)) {
                if (to.var.name %in% names (plane (dc, character(0)))) { next }

                from.var.name <- sup.var.names [to.var.name]
                dp.name <- plane.name (dc, attr (vars (dc, from.var.name), "dim.names"))

                ## Aggregate and store data
                plane (dc, character(0)) [[to.var.name]] <- unname (attr (vars (dc, from.var.name), "FUN.value") (dc[[dp.name]][[from.var.name]]))

                ## Update attributes
                if (from.var.name != to.var.name) {
                    var <- vars (dc, from.var.name)
                    attr (var, "dim.names") <- dim.names (dc) %>% intersect (dim.names)
                    vars (dc, to.var.name) <- var
                }
            }

        } else { ## If other data.plane
            new.dp.name <- plane.name (dc, dim.names)

            for (to.var.name in names (sup.var.names)) {
                if (to.var.name %in% names (dc[[new.dp.name]])) { next }

                from.var.name <- unname (sup.var.names [to.var.name])
                dp.name <- plane.name (dc, attr (vars (dc, from.var.name), "dim.names"))
                
                ## Aggregate data
                expr <- parse_expr (paste0 ("attr (vars (dc, from.var.name), 'FUN.value') (", from.var.name, ")"))
                new.dp <- dc[[dp.name]] %>%
                    group_by (.dots = dim.names) %>%
                    summarise (!! to.var.name := !! expr) %>%
                    ungroup ()

                ## Store data
                attrs <- plane.attributes (dc[[new.dp.name]])
                dc[[new.dp.name]] <- dc[[new.dp.name]] %>% full_join (new.dp, by = dim.names)
                plane.attributes (dc[[new.dp.name]]) <- attrs

                if (from.var.name != to.var.name) {
                ## Update attributes
                    var <- vars (dc, from.var.name)
                    attr (var, "dim.names") <- dim.names (dc) %>% intersect (dim.names)
                    vars (dc, to.var.name) <- var
                }
            }

            for (to.var.name in names (sub.var.names)) {
                if (to.var.name %in% names (dc[[new.dp.name]])) { next }

                from.var.name <- unname (sub.var.names [to.var.name])
                dp.name <- plane.name (dc, attr (vars (dc, from.var.name), "dim.names"))

                ## Store data
                attrs <- plane.attributes (dc[[new.dp.name]])
                if (dp.name == "$") {
                    dc[[new.dp.name]] <- dc[[new.dp.name]] %>% mutate (!! to.var.name := !! plane (dc, character(0))[[from.var.name]])
                } else {
                    dc[[new.dp.name]] <- dc[[new.dp.name]] %>% left_join (dc[[dp.name]] %>% select (attr (vars (dc, from.var.name), "dim.names"), from.var.name) %>% rename (!! to.var.name := !! from.var.name), by = attr (vars (dc, from.var.name), "dim.names"))
                }
                plane.attributes (dc[[new.dp.name]]) <- attrs
                
                ## Update attributes
                if (from.var.name != to.var.name) {
                    var <- vars (dc, from.var.name)
                    attr (var, "dim.names") <- dim.names (dc) %>% intersect (dim.names)
                    vars (dc, to.var.name) <- var
                }
            }

            ## Replace NA values that appeared
            ## TODO: replace only NA value that appeared (not genuine NA values)
            new.var.names <- names (dc[[new.dp.name]]) [names (dc[[new.dp.name]]) %in% var.names (dc)]
            dc[[new.dp.name]] <- dc[[new.dp.name]] %>% replace_na (lapply (vars (dc, new.var.names, drop = FALSE), function (var) attr (var, "NA.value")))

            ## Remove dp if empty
            # if (nrow (dc[[new.dp.name]]) == 0) { dc[[new.dp.name]] <- NULL }
        }

        dc <- dc %>% reorder.dim_() %>% reorder.var_()

        return (dc)
    }



reset.var <- function (obj, ...) { UseMethod ("reset.var") }
reset.var.data.cube <-
    function (dc, dim.names = NULL, ...) {
        str.dim.names <- arg.names (substitute (dim.names))
        if (is.null (str.dim.names)) { str.dim.names <- character(0) }

        str.var.names <- dot.names (enquos (...))
        if (length (str.var.names) == 0) { str.var.names <- var.names (dc) }

        dc %>% reset.var_(str.dim.names, str.var.names)
    }


reset.var_ <- function (obj, ...) { UseMethod ("reset.var_") }
reset.var_.data.cube <-
    function (dc, dim.names = character(0), var.names = var.names.data.cube (dc)) {
        dp.name <- plane.name (dc, dim.names)
        for (var.name in var.names) {
            if (! all (attr (vars (dc, var.names), "dim.names") %in% dim.names)) {
                if (dp.name == "$") {
                    dc[[dp.name]][[var.name]] <- NULL
                } else {
                    dc[[dp.name]] <- dc[[dp.name]] %>% select (- one_of (var.name))
                }
            }
        }
        return (dc)
    }



mutate.var <- function (obj, ...) { UseMethod ("mutate.var") }
mutate.var.data.cube <-
    function (dc, dim.names = NULL, ...) {
        str.dim.names <- arg.names (substitute (dim.names))
        if (is.null (str.dim.names)) { str.dim.names <- dim.names (dc) }

        str.var.mutates <- dot.names (enquos (...))
        str.var.mutates <- paste (names (str.var.mutates), "=", str.var.mutates)
        
        dc %>% mutate.var_(str.dim.names, str.var.mutates)
    }


mutate.var_ <- function (obj, ...) { UseMethod ("mutate.var_") }
mutate.var_.data.cube <-
    function (dc, dim.names = attr (dc, "dim.names"), var.mutates) {
        dp.name <- plane.name (dc, dim.names)
        
        for (var.mutate in var.mutates) {
            pos <- regexpr ("=", var.mutate) [1]
            new.var.name <- var.mutate %>% substring (0, pos-1) %>% trimws ()
            expr <- parse_expr (var.mutate %>% substring (pos+1) %>% trimws ())

            ## Compute variable
            f <- function () {}
            body (f) <- expr
            var.names <- codetools::findGlobals (f, merge = FALSE) $variables %>% intersect (var.names (dc))
            for (var.name in var.names) { dc <- dc %>% compute.var_(dim.names, var.name) }
            dc[[dp.name]] <- dc[[dp.name]] %>% mutate (!! new.var.name := !! expr)
            
            ## Update attributes
            var.class <- class (dc[[dp.name]] %>% pull (new.var.name))

            var <- new.var.name
            class (var) <- "variable"
            attr (var, "dim.names") <- dim.names (dc) %>% intersect (dim.names)
            attr (var, "NA.value") <- var.NA (var.class)
            attr (var, "FUN.value") <- var.FUN (var.class)
            vars (dc, new.var.name) <- var
        }

        return (dc)
    }


transmute.var <- function (obj, ...) { UseMethod ("transmute.var") }
transmute.var.data.cube <-
    function (dc, dim.names = NULL, ...) {
        str.dim.names <- arg.names (substitute (dim.names))
        if (is.null (str.dim.names)) { str.dim.names <- dim.names (dc) }

        str.var.mutates <- dot.names (enquos (...))
        str.var.mutates <- paste (names (str.var.mutates), "=", str.var.mutates)
        
        dc %>% transmute.var_(str.dim.names, str.var.mutates)
    }


transmute.var_ <- function (obj, ...) { UseMethod ("transmute.var_") }
transmute.var_.data.cube <-
    function (dc, dim.names = attr (dc, "dim.names"), var.mutates) {

        new.var.names <- unname (sapply (var.mutates, function (var.mutate) {
            pos <- regexpr ("=", var.mutate) [1]
            var.mutate %>% substring (0, pos-1) %>% trimws ()
        }))
        
        dc %>%
            mutate.var_(dim.names, var.mutates) %>%
            select.dim_(dim.names) %>%
            select.var_(new.var.names)
    }



compute.var.model <- function (obj, ...) { UseMethod ("compute.var.model") }
compute.var.model.data.cube <-
    function (dc, dim.names, var.name, ...) {
        str.dim.names <- arg.names (substitute (dim.names))

        str.var.name <- deparse (substitute (var.name))
            
        str.model.var.names <- dot.names (enquos (...))
        
        dc %>% compute.var.model_(str.dim.names, str.var.name, str.model.var.names)
    }


compute.var.model_ <- function (obj, ...) { UseMethod ("compute.var.model_") }
compute.var.model_.data.cube <-
    function (dc, dim.names = attr (dc, "dim.names"), var.name, model.var.names = character(0)) {

        final.model.var.name <- paste0 (var.name, ".model")
        final.deviation.var.name <- paste0 (var.name, ".deviation")

        dc <- dc %>% compute.var_(character(0), var.name %>% setNames (paste0 (".", var.name)))
        model.var.mutate <- paste0 (final.model.var.name, " = .", var.name)
        
        for (dim.name in dim.names) {
            if (dim.name %in% names (model.var.names)) {
                model.var.name <- model.var.names[dim.name]
                dc <- dc %>%
                    compute.var_(dim.name, model.var.name %>% setNames (paste0 (dim.name, ".", model.var.name))) %>%
                    compute.var_(character(0), model.var.name %>% setNames (paste0 (".", model.var.name)))
                model.var.mutate <- paste0 (model.var.mutate, "*", dim.name, ".", model.var.name, "/.", model.var.name)
            } else {
                dc <- dc %>% mutate.var_(dim.names, paste0 (dim.name, ". = 1 / nrow (plane (dc, '", dim.name, "')"))
                model.var.mutate <- paste0 (model.var.mutate, " * ", dim.name, ".")
            }
        }

        deviation.var.mutate <- paste0 (final.deviation.var.name, " = ", var.name, " / ", final.model.var.name)
        dc <- dc %>% mutate.var_(dim.names, c (model.var.mutate, deviation.var.mutate))

        attr (vars (dc, final.model.var.name), "NA.value") <- NA
        attr (vars (dc, final.deviation.var.name), "NA.value") <- 1

        return (dc)
    }



select.dim <- function (obj, ...) { UseMethod ("select.dim") }
select.dim.data.cube <-
    function (dc, ...) {
        str.dim.names <- dot.names (enquos (...))
        dc %>% select.dim_(str.dim.names)
    }


select.dim_ <- function (obj, ...) { UseMethod ("select.dim_") }
select.dim_.data.cube <-
    function (dc, dim.names = character(0)) {
        
        dp.name <- plane.name (dc, dim.names)

        ## Compute variables
        for (var.name in var.names (dc)) {
            new.var.dim.names <- attr (vars (dc, var.name), "dim.names") %>% intersect (dim.names)
            dc <- dc %>% compute.var_(new.var.dim.names, var.name)
            attr (vars (dc, var.name), "dim.names") <- new.var.dim.names
        }

        ## Adjust attributes
        dims (dc, character(0)) <- dims (dc, dim.names, drop = FALSE)
        
        ## Adjust data.planes
        for (dp.name in names (dc)) {
            dp.dim.names <- data.plane.dim.names (dp.name)
            if (! all (dp.dim.names %in% dim.names)) { dc[[dp.name]] <- NULL }
        }

        ## ## Adjust dp.arrange.var.names
        ## if (! is.null (dc$dp.arrange.var.names)) {
        ##     for (dp.name in names (dc$dp.arrange.var.names)) {
        ##         dp.dim.names <- data.plane.dim.names (dp.name)
        ##         if (! all (dp.dim.names %in% dim.names)) { attr (dc[[dp.name]], "arrange.var.names") <- NULL }
        ##     }
        ##     if (length (dc$dp.arrange.var.names) == 0) { dc$dp.arrange.var.names <- NULL }
        ## }
        

        if (! is.null (names (dim.names))) { dc <- dc %>% rename.dim_(dim.names [names (dim.names) != ""]) }
        dc <- dc %>% reorder.dim_()

        return (dc)
    }



select.var <- function (obj, ...) { UseMethod ("select.var") }
select.var.data.cube <-
    function (dc, ...) {
        str.var.names <- dot.names (enquos (...))
        dc %>% select.var_(str.var.names)
    }


select.var_ <- function (obj, ...) { UseMethod ("select.var_") }
select.var_.data.cube <-
    function (dc, var.names = character (0)) {
        
        old.var.names <- var.names (dc)
        
        ## Adjust vars
        vars (dc, character(0)) <- vars (dc, var.names, drop = FALSE)

        ## Adjust data.planes
        for (dp.name in names (dc)) {
            if (dp.name == "$") {
                attrs <- plane.attributes (dc[[dp.name]])
                dc[[dp.name]] <- dc[[dp.name]] [names (dc[[dp.name]]) %in% var.names]
                plane.attributes (dc[[dp.name]]) <- attrs
            } else {
                new.var.names <- names (dc[[dp.name]]) %>% setdiff (names (dc[[dp.name]]) %>% intersect (old.var.names) %>% setdiff (var.names))
                dc[[dp.name]] <- dc[[dp.name]] %>% select (new.var.names)
            }

            ## Remove data.plane if empty
            if (length (dc[[dp.name]]) == data.plane.dim (dp.name)) {
                dc[[dp.name]] <- NULL
            } else {
                ## Remove empty observations
                if (dp.name != "$") {
                    new.var.names <- names (dc[[dp.name]]) %>% setdiff (dim.names (dc))
                    if (all (new.var.names %in% var.names (dc))) {
                        expr.str <- "FALSE"
                        for (var.name in new.var.names) {
                            if (is.na (attr (vars (dc, var.name), "NA.value"))) {
                                expr.str <- paste0 (expr.str, " | ! is.na (", var.name, ")")
                            } else {
                                expr.str <- paste0 (expr.str, " | ", var.name, " != attr (vars (dc, '", var.name, "'), 'NA.value')")
                            }
                        }
                        expr <- parse_expr (expr.str)
                        dc[[dp.name]] <- dc[[dp.name]] %>% filter (!! expr)
                    }
                }
            }
        }

        ## Adjust arrange.var.names
        for (dp.name in names (dc)) {
            for (i in seq_along (attr (dc[[dp.name]], "arrange.var.names"))) {
                var.name <- attr (dc[[dp.name]], "arrange.var.names") [[i]]
                f <- function () {}
                body (f) <- parse_expr (var.name)
                struct <- codetools::findGlobals (f, merge = FALSE)
                if (any (struct$variables %in% (old.var.names %>% setdiff (var.names)))) { attr (dc[[dp.name]], "arrange.var.names") [[i]] <- "" }
            }

            attr (dc[[dp.name]], "arrange.var.names") <- attr (dc[[dp.name]], "arrange.var.names") [attr (dc[[dp.name]], "arrange.var.names") != ""]

            ## Remove if empty
            if (length (attr (dc[[dp.name]], "arrange.var.names")) == 0) {
                attr (dc[[dp.name]], "arrange.var.names") <- NULL
            }
        }

        if (! is.null (names (var.names))) { dc <- dc %>% rename.var_(var.names [names (var.names) != ""]) }
        dc <- dc %>% reorder.var_()
        
        return (dc)
    }


complete.elm <- function (obj, ...) { UseMethod ("complete.elm") }
complete.elm.data.cube <-
    function (dc, ...) {
        str.dim.names <- dot.names (enquos (...))
        if (length (str.dim.names) == 0) { str.dim.names <- dim.names (dc) }
        
        dc %>% complete.elm_(str.dim.names)
    }


complete.elm_ <- function (obj, ...) { UseMethod ("complete.elm_") }
complete.elm_.data.cube <-
    function (dc, dim.names = attr (dc, "dim.names")) {
        dp.name <- plane.name (dc, dim.names)
        var.names <- names (dc[[dp.name]]) %>% intersect (var.names (dc))

        df <- tibble()
        for (dim.name in dim.names) { df <- df %>% crossing (plane (dc, dim.name) %>% select (dim.name)) }

        if (is.null (dc[[dp.name]])) { dc[[dp.name]] <- empty.plane (dim.names) }
        
        attrs <- plane.attributes (dc[[dp.name]])
        dc[[dp.name]] <- dc[[dp.name]] %>% right_join (df, by = dim.names) %>% replace_na (lapply (vars (dc, var.names, drop = FALSE), function (var) attr (var, "NA.value")))
        plane.attributes (dc[[dp.name]]) <- attrs

        return (dc)
    } 


## TODO : suppress = TRUE ?
filter.elm.indices <- function (obj, ...) { UseMethod ("filter.elm.indices") }
filter.elm.indices.data.cube <-
    function (dc, dim.name, elm.indices) {
        str.dim.name <- arg.names (substitute (dim.name))
        dc %>% filter.elm.indices_(str.dim.name, elm.indices)
    }


filter.elm.indices_ <- function (obj, ...) { UseMethod ("filter.elm.indices_") }
filter.elm.indices_.data.cube <-
    function (dc, dim.name, elm.indices) {

        ## Build transformation table for indices
        indice.table <- rep (NA, nrow (plane (dc, dim.name)))
        indice.table [elm.indices] <- seq_along (elm.indices)

        ## Update data.planes
        filter_expr <- parse_expr (paste0 (dim.name, " %in% elm.indices"))
        for (dp.name in names (dc)) {
            if (dim.name %in% data.plane.dim.names (dp.name)) {
                mutate_expr <- parse_expr (paste0 ("indice.table [", dim.name, "]"))
                dc[[dp.name]] <- dc[[dp.name]] %>% filter (!! filter_expr) %>% mutate (!! dim.name := !! mutate_expr)
            }
        }

        ## Reset modified variables
        for (dp.name in names (dc)) {
            dim.names <- data.plane.dim.names (dp.name)
            var.names <- names (dc[[dp.name]]) %>% intersect (var.names (dc))

            for (var.name in var.names) {
                if (dim.name %in% (attr (vars (dc, var.name), "dim.names") %>% setdiff (dim.names))) {
                    dc <- dc %>% reset.var_(dim.names, var.names)
                }
            }
        }

        return (dc)
    }



filter.elm <- function (obj, ...) { UseMethod ("filter.elm") }
filter.elm.data.cube <-
    function (dc, dim.names, ...) {
        str.dim.names <- arg.names (substitute (dim.names))
        str.elm.filters <- dot.names (enquos (...))

        dc %>% filter.elm_(str.dim.names, str.elm.filters)
    }


filter.elm_ <- function (obj, ...) { UseMethod ("filter.elm_") }
filter.elm_.data.cube <- function (dc, dim.names, elm.filters) {

    ## Compute involved variables
    elm.filter <- paste (elm.filters, collapse = " & ")
    f <- function () {}
    body (f) <- parse_expr (elm.filter)
    var.names <- codetools::findGlobals (f, merge = FALSE) $variables %>% intersect (var.names (dc))
    for (var.name in var.names) { dc <- dc %>% compute.var_(dim.names, var.name) }

    ## Filter element by indices
    dp.name <- plane.name (dc, dim.names)
    elm.indices <- dc[[dp.name]] %>% filter (!! parse_expr (elm.filter)) %>% select (dim.names)    
    for (dim.name in dim.names) {
        dc <- dc %>% filter.elm.indices_(dim.name, unique (elm.indices %>% pull (dim.name)))
    }
    
    return (dc)
}


arrange.elm <- function (obj, ...) { UseMethod ("arrange.elm") }
arrange.elm.data.cube <-
    function (dc, dim.names, ...) {
        str.dim.names <- arg.names (substitute (dim.names))
        str.var.names <- dot.names (enquos (...))

        dc %>% arrange.elm_(str.dim.names, str.var.names)
    }


arrange.elm_ <- function (obj, ...) { UseMethod ("arrange.elm_") }
arrange.elm_.data.cube <- function (dc, dim.names, var.names) {
    
    dp.name <- plane.name (dc, dim.names)

    if (is.null (plane (dc, dim.names))) { plane (dc, dim.names) <- empty.plane (dim.names) }
        
    if (is.null (attr (dc[[dp.name]], "arrange.var.names"))) { attr (dc[[dp.name]], "arrange.var.names") <- character(0) }
    attr (dc[[dp.name]], "arrange.var.names") <- append (unname (var.names), attr (dc[[dp.name]], "arrange.var.names"))

    return (dc)
}


apply.arrange.elm <- function (obj, ...) { UseMethod ("apply.arrange.elm") }
apply.arrange.elm.data.cube <-
    function (dc, ...) {
        str.dim.names <- dot.names (enquos (...))
        dc %>% apply.arrange.elm_(str.dim.names)
    }


apply.arrange.elm_ <- function (obj, ...) { UseMethod ("apply.arrange.elm_") }
apply.arrange.elm_.data.cube <- function (dc, dim.names = attr (dc, "dim.names")) {

    dp.name <- plane.name (dc, dim.names)

    for (dp.name.2 in rev (names (dc))) {
        dim.names.2 <- data.plane.dim.names (dp.name.2)

        if (all (dim.names.2 %in% dim.names) && ! is.null (attr (dc[[dp.name.2]], "arrange.var.names"))) {
            arrange.var.names <- attr (dc[[dp.name.2]], "arrange.var.names")

            ## Compute involved variables
            for (var.name in arrange.var.names) {
                f <- function () {}
                body (f) <- parse_expr (var.name)
                struct <- codetools::findGlobals (f, merge = FALSE)
                dc <- dc %>% compute.var_(dim.names.2, struct$variables %>% intersect (var.names (dc)))
            }
                        
            ## Arrange elements
            dc[[dp.name.2]] <- dc[[dp.name.2]] %>% arrange_(.dots = arrange.var.names)
            dc[[dp.name]] <- dc[[dp.name.2]] %>%
                select (dim.names.2) %>%
                inner_join (dc[[dp.name]], by = dim.names.2)
        }
    }

    dc <- dc %>% reorder.dim ()
    
    return (dc)
}



## TODO : rewrite this function
summary.data.cube <- function (dc) {
    elm.nchar <- 50
    dc.name <- paste (dim.names (dc), collapse = ".")
    
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
    
    for (var.name in var.names (dc)) {
        cat ("-> '", var.name, "' variable:\n", sep = "")
        cat (" - total: ", dc[["."]]$vars[[var.name]], "\n", sep = "")
        dp.nb <- length (plane (dc, dc.name)$vars[[var.name]])
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
        print (summary (plane (dc, dc.name)$vars[[var.name]]))
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
    
    for (dim in dim.names (dc)) {
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
        for (var.name in names (dc[[dp.name]]$vars)) {
            cat (" - '", var.name, "' variable:\n", sep = "")
            print (summary (dc[[dp.name]]$vars[[var.name]]))
        }
        cat ("\n")
    }
}




plot.var <- function (obj, ...) { UseMethod ("plot.var") }
plot.var.data.cube <-
    function (dc, ..., sep.dim.names = NULL, type = "bar") {
        str.var.names <- dot.names (enquos (...))
        if (length (str.var.names) == 0) { str.var.names <- var.names (dc) }
        
        str.sep.dim.names <- arg.names (substitute (sep.dim.names))
        if (is.null (str.sep.dim.names)) { str.sep.dim.names <- character(0) }

        dc %>% plot.var_(str.var.names, sep.dim.names = str.sep.dim.names, type = type)
    }


plot.var_ <- function (obj, ...) { UseMethod ("plot.var_") }
plot.var_.data.cube <-
    function (dc, var.names = var.names.data.cube (dc), sep.dim.names = character(0), type = "bar") {
        
        dp.name <- plane.name (dc)
        var.names <- var.names %>% intersect (names (dc[[dp.name]]))
                                                     
        ## Get data.frame
        dim.names <- Reduce (intersect, lapply (vars (dc, var.names, drop = FALSE), function (var) attr (var, "dim.names")))
        if (is.null (dim.names)) { return (NULL) }
        
        dc %>% select.dim_(dim.names) %>% select.var_(var.names) %>% str
        df <- dc %>% select.dim_(dim.names) %>% select.var_(var.names) %>% as.data.frame (complete = TRUE)
        if (nrow (df) == 0) { return (NULL) }

        ## Get element labels
        unique.dim.names <- dim.names [sapply (dim.names, function (dim.name) length (unique (df[[dim.name]])) == 1)]
        multiple.dim.names <- dim.names %>% setdiff (unique.dim.names)

        if (length (sep.dim.names) > 0) {
            multiple.dim.names <- multiple.dim.names %>% setdiff (sep.dim.names)
            sep.dim.names <- sep.dim.names %>% setdiff (unique.dim.names)
        }

        df$label <- apply (df, 1, function (row) paste (row [multiple.dim.names], collapse = " / "))
        df$label <- factor (df$label, levels = unique (df$label))

        if (length (sep.dim.names) > 0) {
            df$sep.label <- apply (df, 1, function (row) paste (row [sep.dim.names], collapse = " / "))
            df$sep.label <- factor (df$sep.label, levels = unique (df$sep.label))
        }

        ## Check options compatibility
        if (length (var.names) > 1 && length (sep.dim.names) > 0) {
            stop ("cannot plot multiple variables with separate dimensions")
        }

        ## Create plot
        if (length (var.names) == 1) { ## If only one variable
            if (length (sep.dim.names) == 0) { ## If not separate dimension
                if (type == "bar") {
                    p <- ggplot (df, aes (x = label, y = get (var.names))) + 
                        geom_bar (stat = "identity")
                }

                if (type == "line") {
                    p <- ggplot (df, aes (x = label, y = get (var.names), group = 1)) + 
                        geom_line ()
                }

                if (type == "point") {
                    dfm <- df %>% select ("label", var.names) %>% melt (id.vars = "label")
                    p <- ggplot (data = dfm, aes (x = label, y = variable)) +
                        geom_point (aes (size = value), pch = 21, color = "black", fill = "grey") +
                        guides (size = guide_legend (title = var.names))
                }
            } else { ## If separate dimensions
                if (type == "bar") {
                    p <- ggplot (df, aes (x = label, y = get (var.names))) + 
                        geom_bar (aes (fill = sep.label), stat = "identity", position = "dodge") +
                        guides (fill = guide_legend (title = paste (sep.dim.names, collapse = " x ")))
                }

                if (type == "line") {
                    p <- ggplot (df, aes (x = label, y = get (var.names), group = sep.label, colour = sep.label)) + 
                        geom_line () +
                        guides (colour = guide_legend (title = paste (sep.dim.names, collapse = " x ")))
                }

                if (type == "point") {
                    p <- ggplot (data = df, aes (x = label, y = factor (sep.label, level = rev (unique (sep.label))))) +
                        geom_point (aes (size = get (var.names)), pch = 21, color = "black", fill = "grey") +
                        guides (size = guide_legend (title = var.names))
                }
            }
        } else { ## If multiple variables
            dfm <- df %>% select ("label", var.names) %>% melt (id.vars = "label")

            if (type == "bar") {
                p <- ggplot (dfm, aes (x = label, y = value)) + 
                    geom_bar (aes (fill = variable), stat = "identity", position = "dodge") +
                    guides (fill = guide_legend (title = paste (sep.dim.names, collapse = " x ")))
            }

            if (type == "line") {
                p <- ggplot (dfm, aes (x = label, y = value, group = variable, colour = variable)) +
                    geom_line ()
            }

            if (type == "point") {
                dfm <- df %>% select ("label", var.names) %>% melt (id.vars = "label")
                p <- ggplot (data = dfm, aes (x = label, y = factor (variable, level = rev (var.names)))) +
                    geom_point (aes (size = value), pch = 21, color = "black", fill = "grey")
            }
        }

        ## Add origin line
        if (type %in% c ("line", "bar") && all (sapply (attr (vars (dc, var.names), "NA.value"), is.numeric))) {
            NA.values <- unique (unlist (attr (vars (dc, var.names), "NA.value")))
            if (length (NA.values) == 1) {
                p <- p + geom_hline (yintercept = NA.values)

                if (type == "bar") {
                    shift.yaxis <- scales::trans_new ("shift",
                                                      transform = function (x) {x - NA.values},
                                                      inverse = function (x) {x + NA.values})
                    p <- p + scale_y_continuous (trans = shift.yaxis)
                }
            }
        }
        
        ## Adjust axis labels
        p <- p + xlab (paste (multiple.dim.names, collapse = " x "))

        if (type == "point") {
            if (length (var.names) == 1) {
                p <- p + ylab (paste (sep.dim.names, collapse = " x "))
            } else {
                p <- p + ylab ("variable")
            }
        } else {
            if (length (var.names) == 1) { p <- p + ylab (var.names) } else { p <- p + ylab ("value") }
        }
        
        ## p <- p + theme (axis.text.x = element_text (angle = 90, hjust = 1))

        ## Adjust subtitle
        if (length (unique.dim.names) > 0) {
            subtitle <- paste (unique.dim.names, "=", df [1, unique.dim.names]) %>% paste (collapse = " / ")
            p <- p + labs (subtitle = subtitle)
        }

        return (p)
    }



## plot.var_ <- function (obj, ...) { UseMethod ("plot.var_") }
## plot.var_.data.cube <-
##     function (dc,
##               var.name = NULL,
##               dim.names = NULL,
##               sep.dim.name = NULL,
##               type = "col") {

##         ## str(dc)
##         ## var.name = NULL
##         ## dim.names = c('b','a')
##         ## sep.dim.name = NULL
##         ## type = "col"
        
##         if (is.null (dim.names)) { dim.names <- dim.names (dc) }
##         if (is.null (var.name)) {
##             var.name <- names (dc$var.dim.names) [sapply (dc$var.dim.names, function (var) all (dim.names %in% var))]
##             if (length (var.name) == 0) { return (NULL) }
##             var.name <- var.name[1]
##         }
        
##         df <- dc %>%
##             compute.var_(dim.names, var.name) %>%
##             as.data.frame_(dim.names, var.name, complete = TRUE)
##         if (nrow (df) == 0) { return (NULL) }
        
##         unsep.dim.names <- dim.names
##         if (! is.null (sep.dim.name)) { unsep.dim.names <- dim.names [dim.names != sep.dim.name] }
        
##         uniq.dim.names <- c()
##         for (dim.name in unsep.dim.names) {
##             if (length (unique (df[, dim.name])) == 1) { uniq.dim.names <- append (uniq.dim.names, dim.name) }
##         }
##         ununiq.dim.names <- unsep.dim.names [!unsep.dim.names %in% uniq.dim.names]
        
##         df$label <- apply (df, 1, function (row) paste (row [ununiq.dim.names], collapse = " / "))
##         df$label <- factor (df$label, levels = unique (df$label))
        
##         if (type == "col") {
##             if (var.name == "ratio") {df$ratio <- df$ratio - 1 }
##             if (var.name == "deviation" && dc$model$type == "ratio") { df$deviation <- df$deviation - 1 }
            
##             p <- ggplot (df, aes (x = label, y = get (var.name)))
            
##             if (is.null (sep.dim.name)) {
##                 p <- p + geom_col ()
##             } else {
##                 p <-
##                     p + geom_col (aes (fill = factor (get (sep.dim.name), levels = dc$elm.names[[sep.dim.name]])), position = "dodge") +
##                     guides (fill = guide_legend (title = sep.dim.name))
##             }
            
##             if (var.name == "ratio" || var.name == "deviation" && dc$model$type == 'ratio') {
##                 p <- p + scale_y_continuous (labels = function (x) x + 1)
##             }
##         }
        
##         if (type == "line") {
##             indices <- unique (df[, ununiq.dim.names, drop = FALSE])
##             indices$row <- 1:nrow(indices)
##             indices$label <- apply (indices, 1, function (row) paste (row[ununiq.dim.names], collapse = " / "))
##             df$index <- merge (df[, ununiq.dim.names, drop = FALSE], indices)$row
            
##             if (is.null (sep.dim.name)) {
##                 p <- ggplot (df, aes (x = index, y = get (var.name)))
##             } else {
##                 p <-
##                     ggplot (df, aes (
##                                     x = index,
##                                     y = get (var.name),
##                                     color = factor (get (sep.dim.name), levels = rev (dc$elm.names[[sep.dim.name]]))
##                                 )) +
##                     guides (color = guide_legend (title = sep.dim.name))
##             }
            
##             p <- p + geom_line () +
##                 scale_x_continuous (breaks = 1:nrow(indices),
##                                     labels = indices$label)
            
##             if (var.name == "ratio" ||
##                 var.name == "deviation" && dc$model$type == 'ratio') {
##                 p <- p + geom_hline (yintercept = 1)
##             }
##         }
        
##         p <- p + ylab (var.name) +
##             xlab (paste (ununiq.dim.names, collapse = " x ")) +
##             theme (axis.text.x = element_text (angle = 90, hjust = 1))
        
##         if (length (uniq.dim.names) > 0) {
##             title1 <- paste (uniq.dim.names, collapse = " x ")
##             title2 <- paste (df[1, uniq.dim.names], collapse = " / ")
##             p <- p + labs (subtitle = paste (title1, "=", title2))
##         }
        
##         return (p)
##     }


## plot.var <- function (obj, ...) {
##     UseMethod ("plot.var")
## }
## plot.var.data.cube <-
##     function (dc,
##               var.name = NULL,
##               dim.names = NULL,
##               sep.dim.name = NULL,
##               type = "col") {
        
##         var.name <- deparse (substitute (var.name))

##         ## var.names <- lapply (substitute (var.names), deparse)
##         ## if (length (var.names) == 1) { var.names <- unlist (var.names) } else { var.names <- unlist (var.names) [2:length (var.names)] }

##         dim.names <- lapply (substitute (dim.names), deparse)
##         if (length (dim.names) == 1) { dim.names <- unlist (dim.names) } else { dim.names <- unlist (dim.names) [2:length (dim.names)] }

##         sep.dim.name <- deparse (substitute (sep.dim.name))
        
##         plot.var_(dc, var.name = var.name, dim.names = dim.names, sep.dim.name = sep.dim.name, type = type)
##     }


## biplot.var_ <- function (obj, ...) { UseMethod ("biplot.var_") }
## biplot.var_.data.cube <- function (dc, x.dim, y.dim, var.name) {
##     ## Get data
##     df <- as.data.frame (dc)
    
##     uniq.dim.names <- c()
##     for (d in dim.names (dc))
##         if (length (unique (df[, d])) == 1)
##             uniq.dim.names <- append (uniq.dim.names, d)
##     ununiq.dim.names <- dim.names (dc) [!dim.names (dc) %in% uniq.dim.names]
    
##     ## Build plot
##     p <-
##         ggplot (data = df, aes (y = factor (get (x.dim), levels = rev (dc$elm.names[[x.dim]])), x = factor (get (y.dim)))) +
##         xlab (y.dim) + ylab (x.dim)
    
##     if (var.name != "ratio" && var.name != "deviation") {
##         p <-
##             p + geom_point (aes (size = get (var.name)),
##                             pch = 21,
##                             color = "black",
##                             fill = "grey") +
##             guides (size = guide_legend (title = var.name)) +
##             scale_size (range = c(1, 20))
##     } else {
##         var2.name <- var.names (dc)[1]
##         p <-
##             p + geom_point (aes (fill = get (var.name), size = get (var2.name)),
##                             pch = 21,
##                             color = "black") +
##             guides (fill = guide_legend (title = var.name),
##                     size = guide_legend (title = var2.name)) +
##             scale_size (range = c(1, 20))
        
##         if (var.name == "ratio" ||
##             var.name == "deviation" &&
##             dc$model$type == "ratio")
##             p <-
##                 p + scale_fill_gradient2 (
##                         low = "blue",
##                         mid = "white",
##                         high = "red",
##                         midpoint = 1
##                     )
##         else
##             p <-
##                 p + scale_fill_gradient2 (
##                         low = "blue",
##                         mid = "white",
##                         high = "red",
##                         midpoint = 0
##                     )
##     }
    
##     p <-
##         p + theme (axis.text.x = element_text (angle = 90, hjust = 1))
    
##     if (length (uniq.dim.names) > 0) {
##         title1 <- paste (uniq.dim.names, collapse = " x ")
##         title2 <- paste (df[1, uniq.dim.names], collapse = " / ")
##         p <- p + labs (subtitle = paste (title1, "=", title2))
##     }
    
##     return (p)
## }



plot.outlier <- function (obj, ...) {
    UseMethod ("plot.outlier")
}
plot.outlier.data.cube <- function (dc, labels = TRUE) {
    dc.name <- paste (dim.names (dc), collapse = ".")
    var.name <- var.names (dc)[1]
    
    ## Get data
    df <- as.data.frame (dc)
    
    uniq.dim.names <- c()
    for (d in dim.names (dc))
        if (length (unique (df[, d])) == 1)
            uniq.dim.names <- append (uniq.dim.names, d)
    ununiq.dim.names <- dim.names (dc) [!dim.names (dc) %in% uniq.dim.names]
    
    df$label <-
        apply (df, 1, function (row)
            paste (row[ununiq.dim.names], collapse = " / "))
    
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
    
    if (length (uniq.dim.names) > 0) {
        title1 <- paste (uniq.dim.names, collapse = " x ")
        title2 <- paste (df[1, uniq.dim.names], collapse = " / ")
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
        dim <- dim.names (dc) [dim.names (dc) %in% dim]
        dc.name <- paste (dim.names (dc), collapse = ".")
        var.name <- var.names (dc)[1]
        
        for (d in dim) {
            dp.name <- paste (d, collapse = ".")
            if (is.null (dc[[dp.name]]))
                dc <- dc %>% compute.margin_(d)
        }
        
        if (is.null (dc[["."]]))
            dc <- dc %>% compute.margin_()
        sum <- dc[["."]]$vars[[var.name]]
        
        ## Compute model
        model <- rep (sum, length (plane (dc, dc.name)$vars[[var.name]]))
        
        for (d in dim) {
            dp.name <- paste (d, collapse = ".")
            dc.data <-
                append (list (1:length(plane (dc, dc.name)$var[[var.name]])), plane (dc, dc.name)$elms[d])
            names (dc.data) [1] <- "row"
            dp.data <-
                append (dc[[dp.name]]$elms, dc[[dp.name]]$vars[var.name])
            
            dist <- merge (dc.data, dp.data)
            dist <- dist [order (dist$row), var.name] / sum
            model <- model * dist
        }
        
        other.dim <- dim.names (dc) [!dim.names (dc) %in% dim]
        for (d in other.dim)
            model <- model / dc$elm.nb[[d]]
        
        plane (dc, dc.name)$vars[["model"]] <- model
        attr (dc, "var.names") <- unique (append (var.names (dc), "model"))
        
        ## Compute deviation
        plane (dc, dc.name)$vars[["ratio"]] <-
            plane (dc, dc.name)$vars[[var.name]] / plane (dc, dc.name)$vars[["model"]]
        attr (dc, "var.names") <- unique (append (var.names (dc), "ratio"))
        
        if (deviation.type == "ratio") {
            plane (dc, dc.name)$vars[["deviation"]] <-
                plane (dc, dc.name)$vars[["ratio"]]
        }
        
        if (deviation.type == "poisson") {
            plane (dc, dc.name)$vars[["deviation"]] <- ifelse (
                plane (dc, dc.name)$vars[[var.name]] < plane (dc, dc.name)$vars[["model"]],
                ppois (
                    plane (dc, dc.name)$vars[[var.name]],
                    plane (dc, dc.name)$vars[["model"]],
                    lower.tail = TRUE,
                    log.p = TRUE
                ),
                -ppois (
                     plane (dc, dc.name)$vars[[var.name]],
                     plane (dc, dc.name)$vars[["model"]],
                     lower.tail = FALSE,
                     log.p = TRUE
                 )
            )
        }
        
        if (deviation.type == "KLdiv") {
            plane (dc, dc.name)$vars[["deviation"]] <-
                plane (dc, dc.name)$vars[[var.name]] / sum * log2 (plane (dc, dc.name)$vars[[var.name]] / plane (dc, dc.name)$vars[["model"]])
        }
        
        attr (dc, "var.names") <- unique (append (var.names (dc), "deviation"))
        
        ## Apply threshold
        dev.mean <- mean (plane (dc, dc.name)$vars[["deviation"]])
        dev.sd <- sd (plane (dc, dc.name)$vars[["deviation"]])
        
        if (is.na (dev.sd)) {
            df$outlier <-
                rep (0, length (plane (dc, dc.name)$vars[["deviation"]]))
        }
        else {
            plane (dc, dc.name)$vars[["outlier"]] <-
                findInterval (plane (dc, dc.name)$vars[["deviation"]],
                              dev.mean + dev.sd * deviation.threshold * c(-1, 1)) - 1
        }
        
        attr (dc, "var.names") <- unique (append (var.names (dc), "outlier"))
        
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
    summary (plane (dc, dc.name)$vars[[data]])
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
            ggplot (as.data.frame (plane (dc, dc.name)$vars), aes (x = get(data))) +
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
            logmax <- ceiling(log10(max(plane (dc, dc.name)$vars[[data]])))
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
            logmax <- ceiling(log10(max(plane (dc, dc.name)$vars[[data]])))
            breaks <- 10 ^ seq (0, logmax, logmax / 10)
            title <- paste (title, "(logarithmic scales)")
            
            p <- p + scale_x_continuous (trans = "log", breaks = breaks) +
                scale_y_continuous (trans = "log")
        }
        
        if (!is.null (threshold)) {
            data.mean <- mean (plane (dc, dc.name)$vars[[data]])
            data.sd <- sd (plane (dc, dc.name)$vars[[data]])
            
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
            as.data.frame (cbind (as.data.frame (plane (dc, dc.name)$elms), as.data.frame (plane (dc, dc.name)$vars)))
        if (!is.null (display)) {
            data.frame <- data.frame[data.frame[[display]],]
        }
        
        data.frame$ratio <- data.frame$obs / data.frame$exp
        data.frame$type <-
            ifelse (data.frame[[outlier]] == 0, "normal", "abnormal")
        
        dummy <-
            lapply (dim.names (dc), function (dim) {
                return (data.frame[[dim]] <<-
                            dc$elm.names[[dim]][data.frame[[dim]]])
            })
        data.frame$label <-
            apply (data.frame, 1, function (row)
                paste (row[dim.names (dc)], collapse = " "))
        
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
              dims = attr (dc, "dim.names"),
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
            if (dims[i] %in% dim.names (dc)) {
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
