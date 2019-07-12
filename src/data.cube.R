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


library ("crayon")
library ("tidyverse")
library ("reshape2")
library ("rlang")
library ("ggrepel")

##library ("sticky")
##append <- base::append


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
    attrs [names (attrs) %>% intersect (c ("class", "dim.names"))]
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

data.plane.dim.nb <- function (dp.name) {
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
        if (is.null (str.dim.names)) { str.dim.names <- c () }
        ## if (is.null (str.dim.names)) { str.dim.names <- names (df) [sapply (df, class) != "numeric"] }

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
        
        df <- as_tibble (df) %>% select (append (unname (dim.names), unname (var.names)))
        df <- df %>% dplyr::mutate_if (sapply (df, is.factor), as.character)
        warning ("Observations are assumed to be unique. Check for potential duplicates in the input data.frame if unsure.")

        ## if (length (dim.names) > 0 && nrow (unique (df [, dim.names, drop = FALSE])) < nrow (df)) {
        ##     stop ("duplicate observations should first be removed")
        ## }
        
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
            var <- unname (var.name)
            class (var) <- "variable"
            attr (var, "dim.names") <- dim.names (dc)
            attr (var, "NA.value") <- var.NA (class (df [, var.name, drop = TRUE]))
            attr (var, "FUN.value") <- var.FUN (class (df [, var.name, drop = TRUE]))            
            vars (dc, var.name) <- var
        }
        
        ## Fill observations
        for (dim.name in dim.names (dc)) {
            
            elm.list <- df [[dim.name]]
            ## fct.list <- factor (elm.list, levels = unique (elm.list))
            fct.list <- unique (elm.list)

            ## Build min data.plane
            ## dp <- list (name = levels (fct.list)) %>%
            ##     as_tibble () %>% tibble::rowid_to_column (dim.name)
            dp <- list (name = fct.list) %>%
                as_tibble () %>% tibble::rowid_to_column (dim.name)

            class (dp) <- append ("data.plane", class (dp))
            attr (dp, "dim.names") <- dim.name

            plane (dc, dim.name) <- dp
            
            ## Update data.frame indices
            ## df [, dim.name] <- as.integer (fct.list)
            df [, dim.name] <- df %>% pull (dim.name) %>% match (fct.list)
        }
        
        ## Build or merge max data.plane
        if (dim.nb (dc) == 0) {
            dp <- as.list (df)
            class (dp) <- append ("data.plane", class (dp))
        } else if (dim.nb (dc) == 1) {
            dp <- inner_join (plane (dc), df, by = unname (dim.names))
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


elm.nb <- function (obj, ...) { UseMethod ("elm.nb") }
elm.nb.data.cube <- function (dc, dim.name) { dc %>% plane (dim.name) %>% nrow }

elm.names <- function (obj, ...) { UseMethod ("elm.names") }
elm.names.data.cube <- function (dc, dim.name) { dc %>% plane (dim.name) %>% pull (name) }


summary.data.cube <- function (dc) {
    elm.nchar <- 50
    cat (
        "data.cube of ",
        dim.nb (dc),
        " dimension",
        ifelse (dim.nb (dc) > 1, "s", ""),
        " and ",
        var.nb (dc),
        " variable",
        ifelse (var.nb (dc) > 1, "s", ""),
        "\n\n",
        sep = ""
    )

    for (dim.name in dim.names (dc)) {
        cat ("-> Dimension ", cyan (dim.name), "\n", sep = "")

        elm.nb <- dc %>% elm.nb (dim.name)
        elm.names <- dc %>% elm.names (dim.name)
        
        i <- 1
        elm.str <- elm.names[i]
        ## elm.str <- paste0 ("'", elm.names[i], "'")
        while (nchar (elm.str) < elm.nchar && i < length (elm.names)) {
            i <- i + 1        
            elm.str <- paste0 (elm.str, ", ", elm.names[i])
            ## elm.str <- paste0 (elm.str, ", '", elm.names[i], "'")
        }
        if (i < length (elm.names)) {
            elm.str <- paste0 (elm.str, ", ...")
        }

        cat (" - Element number: ", elm.nb, "\n", sep = "")
        cat (" - Class (type):   ", elm.names %>% class, " (", elm.names %>% typeof, ")\n", sep = "")
        cat (" - Element names:  ", elm.str, "\n", sep = "")

        ## dp.name <- dim
        ## for (var.name in names (dc[[dp.name]]$vars)) {
        ##     cat (" - '", var.name, "' variable:\n", sep = "")
        ##     print (summary (dc[[dp.name]]$vars[[var.name]]))
        ## }

        cat ("\n")
    }

    for (var.name in var.names (dc)) {
        cat ("-> Variable ", green (var.name), "\n", sep = "")

        var <- dc %>% vars (var.name)

        if (var %>% attr ("dim.names") %>% length > 0) {
            cat (" - Dimensions:   ", paste (cyan (var %>% attr ("dim.names")), collapse = " x "), "\n", sep = "")
        } else {
            cat (" - Dimensions:   null\n", sep = "")
        }
        
        cat (" - Class (type): ", var %>% attr ("NA.value") %>% class, " (", var %>% attr ("NA.value") %>% typeof, ")\n", sep = "")

        cat (" - NA value:    ")
        var %>% attr ("NA.value") %>% str

        ## cat (" - FUN value:  ", var %>% attr ("FUN.value") %>% str, "\n", sep = "")

        ## cat (" - total: ", dc[["."]]$vars[[var.name]], "\n", sep = "")
        ## dp.nb <- length (plane (dc, dc.name)$vars[[var.name]])
        ## cat (
        ##     " - divided into ",
        ##     dp.nb,
        ##     " (non-null) dpervation",
        ##     ifelse (dp.nb > 1, "s", ""),
        ##     "\n",
        ##     sep = ""
        ## )
        ## elm.nb <- prod (unlist (dc$elm.nb))
        ## cat (
        ##     " - among ",
        ##     elm.nb,
        ##     " cell",
        ##     ifelse (elm.nb > 1, "s", ""),
        ##     " (density = ",
        ##     format (dp.nb / elm.nb, digits = 3),
        ##     ")\n",
        ##     sep = ""
        ## )
        ## print (summary (plane (dc, dc.name)$vars[[var.name]]))

        cat ("\n")
    }
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
    dp.names <- dp.names [order (sapply (dp.names, data.plane.dim.nb))]

    attrs <- attributes (dc)
    dc <- dc [dp.names]
    attrs[["names"]] <- names (dc)
    attributes (dc) <- attrs
    
    if (! is.null (attr (dc, "arrange.var.names"))) {
        names (attr (dc, "arrange.var.names")) <- sapply (names (attr (dc, "arrange.var.names")), function (dp.name) plane.name (dc, data.plane.dim.names (dp.name)))
    }

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

    if (! is.null (attr (dc, "arrange.var.names"))) {
        names (attr (dc, "arrange.var.names")) <- sapply (names (attr (dc, "arrange.var.names")), function (dp.name) plane.name (dc, unname (new.dim.names [data.plane.dim.names (dp.name)])))
    }

    ## Rename dim.names in dp
    for (dp.name in names (dc)) {
        if (dp.name != "$") {            
            attr (dc[[dp.name]], "dim.names") <- unname (new.dim.names [attr (dc[[dp.name]], "dim.names")])
            
            dc[[dp.name]] <- dc[[dp.name]] %>% dplyr::rename (!!! dim.names [dim.names %in% names (dc[[dp.name]])])
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
            dc[[dp.name]] <- dc[[dp.name]] %>% dplyr::rename (!!! var.names [var.names %in% names (dc[[dp.name]])])
        } else {
            names (dc[[dp.name]]) <- new.var.names [names (dc[[dp.name]])]
        }
    }

    ## Rename arrange.var.names
    if (! is.null (attr (dc, "arrange.var.names"))) {
        for (dp.name in names (attr (dc, "arrange.var.names"))) {
            for (i in seq_along (attr (dc, "arrange.var.names") [[dp.name]])) {
                var.name <- attr (dc, "arrange.var.names") [[dp.name]][i]
                f <- function () {}
                body (f) <- parse_expr (var.name)
                struct <- codetools::findGlobals (f, merge = FALSE)
                struct$variables <- ifelse (struct$variable %in% names (new.var.names), new.var.names [struct$variable], struct$variable)
                if (length (struct$functions) > 0) {
                    attr (dc, "arrange.var.names") [[dp.name]][i] <- paste0 (struct$functions, "(", paste (struct$variables, collapse = ","), ")")
                } else {
                    attr (dc, "arrange.var.names") [[dp.name]][i] <- paste (struct$variables, collapse = ",")
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

        ## TODO: join arrange.var.names ?
        
        ## Join min data.planes
        for (dim.name in dim.names (add.dc)) {
            if (is.null (plane (dc, dim.name))) {
                plane (dc, dim.name) <- plane (add.dc, dim.name)
            } else {
                ## Join the two planes
                joint.df <- full_join (plane (dc, dim.name), plane (add.dc, dim.name), by = "name", suffix = c ("", ".mod"))

                ## Compute new indices if missing elements in the original plane
                mod.dim.name <- paste0 (dim.name, ".mod")
                new.indices <- seq (1, joint.df %>% pull (!! rlang::sym (dim.name)) %>% is.na %>% sum) + joint.df %>% pull (!! rlang::sym (dim.name)) %>% max (na.rm = TRUE)
                expr <- parse_expr (paste0 ("ifelse (is.na (", dim.name, "), new.indices, ", dim.name, ")"))
                joint.df <- joint.df %>% dplyr::mutate (!! dim.name := !! expr) %>% dplyr::arrange (!! rlang::sym (dim.name))
                indice.table <- joint.df %>% dplyr::arrange (!! rlang::sym (mod.dim.name)) %>% pull (dim.name)

                ## Copy plane attributes
                attr.names <- names (attributes (plane (dc, dim.name))) %>% setdiff (c ("names", "row.names", "class"))
                attr (joint.df, attr.names) <- attr (plane (dc, dim.name), attr.names)

                ## Replace orgininal plane byt its joint
                plane (dc, dim.name) <- joint.df %>% select (names (joint.df) %>% setdiff (mod.dim.name))
                
                ## Update element indices in added plane
                for (dp.name in names (add.dc)) {
                    dp.dim.names <- data.plane.dim.names (dp.name)

                    if (dim.name %in% dp.dim.names && length (dp.dim.names) > 1) {
                        expr <- parse_expr (paste0 ("indice.table [", dim.name, "]"))
                        plane (add.dc, dp.dim.names) <- plane (add.dc, dp.dim.names) %>% dplyr::mutate (!! dim.name := !! expr)
                    }
                }
            }
        }
        
        ## Join other data.planes
        for (dp.name in names (add.dc)) {
            dp.dim.names <- data.plane.dim.names (dp.name)

            if (length (dp.dim.names) != 1) {
                if (is.null (plane (dc, dp.dim.names))) {
                    plane (dc, dp.dim.names) <- plane (add.dc, dp.dim.names)
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
    
    ## Remove name variable if present (and attributes)
    df$name <- NULL
    class (df) <- class (df) [class (df) != "data.plane"]
    attr (df, "dim.names") <- NULL

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
        same.var.names <- var.names [var.names %in% var.names (dc) [sapply (vars (dc, drop = FALSE), function (var) all (attr (var, "dim.names") %in% dim.names) && all (dim.names %in% attr (var, "dim.names")))]]

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

            for (to.var.name in names (same.var.names)) {
                if (to.var.name %in% names (plane (dc, character(0)))) { next }
                from.var.name <- same.var.names [to.var.name]

                plane (dc, character(0)) [[to.var.name]] <- plane (dc, character(0)) [[from.var.name]]

                var <- to.var.name
                attributes (var) <- attributes (vars (dc, from.var.name))
                attr (var, "dim.names") <- dim.names
                vars (dc, to.var.name) <- var
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
                    dplyr::group_by (.dots = dim.names) %>%
                    dplyr::summarise (!! to.var.name := !! expr) %>%
                    ungroup ()

                ## Store data
                attrs <- plane.attributes (dc[[new.dp.name]])
                dc[[new.dp.name]] <- dc[[new.dp.name]] %>% full_join (new.dp, by = dim.names)
                plane.attributes (dc[[new.dp.name]]) <- attrs

                ## Update attributes
                if (from.var.name != to.var.name) {
                    var <- to.var.name
                    attributes (var) <- attributes (vars (dc, from.var.name))
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
                    dc[[new.dp.name]] <- dc[[new.dp.name]] %>% dplyr::mutate (!! to.var.name := !! plane (dc, character(0))[[from.var.name]])
                } else {
                    dc[[new.dp.name]] <- dc[[new.dp.name]] %>% left_join (dc[[dp.name]] %>% select (attr (vars (dc, from.var.name), "dim.names"), from.var.name) %>% dplyr::rename (!! to.var.name := !! from.var.name), by = attr (vars (dc, from.var.name), "dim.names"))
                }
                plane.attributes (dc[[new.dp.name]]) <- attrs
                
                ## Update attributes
                if (from.var.name != to.var.name) {
                    var <- to.var.name
                    attributes (var) <- attributes (vars (dc, from.var.name))
                    attr (var, "dim.names") <- dim.names (dc) %>% intersect (dim.names)
                    vars (dc, to.var.name) <- var
                }
            }

            for (to.var.name in names (same.var.names)) {
                if (to.var.name %in% names (dc[[new.dp.name]])) { next }
                from.var.name <- unname (same.var.names [to.var.name])
                
                dc[[new.dp.name]][[to.var.name]] <- dc[[new.dp.name]][[from.var.name]]
                
                var <- to.var.name
                attributes (var) <- attributes (vars (dc, from.var.name))
                attr (var, "dim.names") <- dim.names
                vars (dc, to.var.name) <- var
            }

            ## Replace NA values that appeared
            ## TODO: replace only NA value that appeared (not genuine NA values)
            new.var.names <- names (dc[[new.dp.name]]) [names (dc[[new.dp.name]]) %in% var.names (dc)]
            dc[[new.dp.name]] <- dc[[new.dp.name]] %>% replace_na (lapply (vars (dc, new.var.names, drop = FALSE), function (var) attr (var, "NA.value")))

            ## Remove dp if empty
            # if (nrow (dc[[new.dp.name]]) == 0) { dc[[new.dp.name]] <- NULL }
        }

        ## Reorder
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

            if (is.null (plane (dc, dim.names))) {
                plane (dc, dim.names) <- empty.plane (dim.names)
                dc <- dc %>% reorder.dim_()
            }
            
            attrs <- plane.attributes (dc[[dp.name]])
            if (dp.name == "$") { dc[[dp.name]] <- dc[[dp.name]] %>% as_tibble }

            dc[[dp.name]] <- dc[[dp.name]] %>% dplyr::mutate (!! new.var.name := !! expr)
            var.class <- class (dc[[dp.name]] %>% pull (new.var.name))

            if (dp.name == "$") { dc[[dp.name]] <- dc[[dp.name]] %>% as.list }
                plane.attributes (dc[[dp.name]]) <- attrs
            
            ## Update attributes

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



spread.dim.to.vars <- function (obj, ...) { UseMethod ("spread.dim.to.vars") }
spread.dim.to.vars.data.cube <-
    function (dc, ...) {
        str.dim.names <- dot.names (enquos (...))
        if (length (str.dim.names) == 0) { str.dim.names <- dim.names (dc) }
  
        dc %>% spread.dim.to.vars_(str.dim.names)
    }


## TODO: check and improve performances
spread.dim.to.vars_ <- function (obj, ...) { UseMethod ("spread.dim.to.vars_") }
spread.dim.to.vars_.data.cube <-
    function (dc, dim.names = dim.names.data.cube (dc)) {

        valid.names.warning <- FALSE
        for (dim.name in dim.names) {
            
            dim.elm.names <- plane (dc, dim.name) %>% pull (name) %>% as.character

            ## Adjust all data.planes
            for (dp.name in rev (names (dc))) {
                dp.dim.names <- data.plane.dim.names (dp.name)

                if (dim.name %in% dp.dim.names) {
                    dp.elm.names <- plane (dc, dim.name)$name [dc[[dp.name]][[dim.name]]] %>% as.character
                    dp.var.names <- dc[[dp.name]] %>% names %>% intersect (vars (dc, drop = FALSE) %>% names)

                    if (length (dp.var.names) > 0) {
                        ## Get NA.values of variables
                        NA.values <- lapply (dp.var.names, function (var.name) { vars (dc, var.name) %>% attr ("NA.value") })
                        NA.values <- rep (NA.values, each = length (dim.elm.names))
                        names (NA.values) <- paste0 (rep (dp.var.names, each = length (dim.elm.names)), ".", dim.elm.names)

                        ## Modify data.plane
                        dc[[dp.name]] <-
                            dc[[dp.name]] %>%
                            dplyr::mutate (temp.dim := dp.elm.names) %>%
                            dplyr::mutate (!! dim.name := 0)

                        if (length (dp.dim.names) == 1) {
                            dc[[dp.name]] <- dc[[dp.name]] %>% dplyr::select (-name)
                        }
                        
                        dc[[dp.name]] <-
                            dc[[dp.name]] %>%
                            tidyr::gather (variable, value, dp.var.names) %>%
                            tidyr::unite (temp, variable, temp.dim, sep = ".") %>%
                            tidyr::spread (temp, value)
                        
                        ## Add missing variables
                        missing.elm.names <- dim.elm.names %>% setdiff (dp.elm.names %>% unique)
                        if (length (missing.elm.names) > 0) {
                            missing.var.names <- paste0 (rep (dp.var.names, length (missing.elm.names)), ".", missing.elm.names)
                            dc[[dp.name]][[missing.var.names]] <- NA
                        }

                        ## Adjust type of columns
                        current.NA.classes <- dc[[dp.name]] %>% select (names (NA.values)) %>% lapply (class) %>% unlist
                        NA.classes <- list (lapply (NA.values, class)) %>% unlist

                        for (var.name in names (NA.classes)) {
                            if (current.NA.classes[var.name] != NA.classes[var.name]) {
                                expr <- parse_expr (paste0 ("type.convert (`", var.name, "`, \"", NA.classes[var.name], "\")"))
                                dc[[dp.name]] <- dc[[dp.name]] %>% dplyr::mutate (!! var.name := !! expr)             
                            }
                        }
                        
                        ## Replace NA values
                        dc[[dp.name]] <- dc[[dp.name]] %>% tidyr::replace_na (NA.values %>% as.list)

                        ## Check new variable names
                        valid.names <- make.names (names (dc[[dp.name]]))
                        if (! all (names (dc[[dp.name]]) != valid.names)) {
                            names (dc[[dp.name]]) <- valid.names
                            valid.names.warning <- TRUE
                        }
                        
                        ## Aggregate impacted variables
                        for (var.name in dp.var.names) {
                            FUN.value <- vars (dc, var.name) %>% attr ("FUN.value")
                            list.str <-
                                paste0 (var.name, ".", dim.elm.names) %>%
                                make.names %>%
                                paste0 ("[i]") %>%
                                paste0 (collapse = ", ")
                            
                            expr <- parse_expr (paste0 ("sapply (seq_along (", dim.name, "), function (i) { FUN.value (", list.str, ") })"))
                            dc[[dp.name]] <- dc[[dp.name]] %>% dplyr::mutate (!! var.name := !! expr)
                        }
                    }
                }
            }

            ## Add new variables
            for (var in vars (dc, drop = FALSE)) {
                if (dim.name %in% attr (var, "dim.names")) {
                    new.vars <-
                        lapply (dim.elm.names, function (elm.name) {
                            new.var <- paste0 (var[1], ".", elm.name) %>% make.names
                            attributes (new.var) <- attributes (var)
                            return (new.var)
                        })
                    names (new.vars) <- paste0 (var[1], ".", dim.elm.names) %>% make.names
                    attr (dc, "vars") <- append (attr (dc, "vars"), new.vars)
                }
            }

            ## Remove dimension
            dc <- dc %>% select.dim_(dc %>% dim.names %>% setdiff (dim.name))
        }

        if (valid.names.warning) {
                warning ("The name of some elements of the spread dimensions are not valid R identifiers. They have hence been adjusted to constitute valid variable names.")
        }
        
        return (dc)
    }



multiset.table <- function (mset1, mset2) {
    mset1 %>% table %>% as.data.frame %>% transmute (elm = as.character (.), occ1 = Freq) %>%
        full_join (
            mset2 %>% table %>% as.data.frame %>% transmute (elm = as.character (.), occ2 = Freq),
            by = "elm"
        ) %>%
        dplyr::mutate (
            occ1 = ifelse (is.na (occ1), 0, occ1),
            occ2 = ifelse (is.na (occ2), 0, occ2),
            diff = occ1 - occ2
        )
}

multiset.inclusion <- function (mset1, mset2) {    
    all (multiset.table (mset1, mset2) %>% pull (diff) >= 0)
}

multiset.equality <- function (mset1, mset2) {    
    all (multiset.table (mset1, mset2) %>% pull (diff) == 0)
}

multiset.difference <- function (mset1, mset2) {   
    table <- multiset.table (mset1, mset2) %>% filter (diff > 0)
    rep (table %>% pull (elm), table %>% pull (diff))
}


parse.var.form <- function (obj, ...) { UseMethod ("parse.var.form") }
parse.var.form.data.cube <- function (dc, var.form) {

    if (is.null (var.form)) { return (NULL) }
    error <- FALSE
    
    var.form.str <- var.form
    var.form <- list ()
    class (var.form) <- "var.form"
    
    ## Compress formula
    ## var.form.str <- "v1 (a * b / c) * v1 * v2 (NULL) * NULL (b)"
    var.form.str <- var.form.str %>% str_remove_all (" ")

    ## Get variables and dimensions
    var.form.str <-
        var.form.str %>%
        str_replace_all (c ("\\(\\)" = "(NULL)")) %>%
        str_split ("\\*(?![^\\(]*\\))") %>% first %>%
        str_split ("[()]") %>%
        lapply (function (x) {x [x != ""]})

    var.form$var.names <-
        var.form.str %>%
        lapply (function (x) {x[1]})
    
    var.form.dim.names <-
        var.form.str %>%
        lapply (function (x) {
            if (is.na (x[2])) { return (vars (dc, x[1]) %>% attr ("dim.names") %>% paste (collapse = "*")) }
            if (x[2] == "NULL") { return (NULL) }
            return (x[2] %>% str_split ("/") %>% first)
        })

    var.form$top.dim.names <-
        var.form.dim.names %>%
        lapply (function (x) {
            if (! is.null (x[1]) && ! is.na (x[1])) { x[1] %>% str_split ("\\*") %>% first }
        })
    
    var.form$bot.dim.names <-
        var.form.dim.names %>%
        lapply (function (x) {
            if (! is.null (x[2]) && ! is.na (x[2])) { x[2] %>% str_split ("\\*") %>% first }
        })

    ## Compute dimensionality
    if (! multiset.inclusion (var.form$top.dim.names %>% unlist, var.form$bot.dim.names %>% unlist)) {
        message ("Bottom dimensions should be included in top dimensions.")
        error <- TRUE
    } else {
        var.form$dim.names <- multiset.difference (var.form$top.dim.names %>% unlist, var.form$bot.dim.names %>% unlist)
    }

    ## Decompress formula
    var.form$str <-
        var.form$var.names %>% unlist %>%
        paste0 ("(") %>% paste0 (var.form$top.dim.names %>% lapply (paste, collapse = "*")) %>%
        paste0 (var.form$bot.dim.names %>% lapply (function (x) { if (is.null (x)) { "" } else { "/" %>% paste0 (x %>% paste (collapse = "*"))}})) %>%
        paste0 (")") %>%
        paste (collapse = "*") %>%
        str_replace_all (c ("\\*" = " * ", "/" = " / ", "\\(" = " ("))

    ## Check variables and dimensions
    for (var.name in unique (unlist (var.form$var.names))) {
        if (var.name != "NULL" && ! var.name %in% var.names (dc)) {
            message (paste0 ("Variable '", var.name, "' not found"))
            error <- TRUE
        }
    }
    
    for (dim.name in unique (append (unlist (var.form$top.dim.names), unlist (var.form$bot.dim.names)))) {
        if (dim.name != "NULL" && ! dim.name %in% dim.names (dc)) {
            message (paste0 ("Dimension '", dim.name, "' not found"))
            error <- TRUE
        }
    }

    if (error) { stop (paste0 ("Formula '", var.form$str, "' is invalid.")) }

    ## Return variable formula
    return (var.form)
}


compute.var.model <- function (obj, ...) { UseMethod ("compute.var.model") }
compute.var.model.data.cube <-
    function (dc, var.form, var.model.form = NULL, keep.inter.var = FALSE) {
        var.form.str <- deparse (substitute (var.form))        

        var.model.form.str <- deparse (substitute (var.model.form))
        if (var.model.form.str == "NULL") { var.model.form.str <- NULL }
            
        dc %>% compute.var.model_(var.form.str, var.model.form.str, keep.inter.var = keep.inter.var)
    }


compute.var.model_ <- function (obj, ...) { UseMethod ("compute.var.model_") }
compute.var.model_.data.cube <-
    function (dc, var.form, var.model.form = NULL, keep.inter.var = FALSE) {
        
        ## Check variable formula
        var.form <- parse.var.form (dc, var.form)

        if (length (var.form$var.names) > 1 || ! is.null (var.form$bot.dim.names[[1]])) {
            stop ("Only models of univariate data are currently implented. Variable formula should contain only one term.")
        }

        ## Check model formula
        var.model.form <- parse.var.form (dc, var.model.form)
        
        missing.dim.names <- var.form$dim.names
        if (! is.null (var.model.form)) {
            missing.dim.names <- multiset.difference (var.form$dim.names, var.model.form$dim.names)
        }

        if (length (missing.dim.names) > 0) {
            var.model.form.str <- paste0 ("NULL (", missing.dim.names, ")") %>% paste (collapse = " * ")

            if (! is.null (var.model.form)) {
                var.model.form.str <- paste0 (var.model.form$str, " * ", var.model.form.str)
            }

            var.model.form <- parse.var.form (dc, var.model.form.str)
        }
        
        ## Check dimensionality
        if (! multiset.equality (var.form$dim.names, var.model.form$dim.names)) {
            stop ("Variable dimensions and model dimensions should be equal.")
        }

        ## Reprint form
        message (paste0 ("Computing model '", var.form$str, " ~ ", var.model.form$str, "'"))

        ## Variable names
        var.name <- var.form$var.names[[1]]
        data.dim.names <- var.form$top.dim.names[[1]]
        
        base.var.name <- paste0 (var.name, ".")
        data.var.name <- var.name
        model.var.name <- paste0 (var.name, ".model")

        ## COMPUTE MODEL
        ## Compute base variable
        model.var.mutate <- paste0 (model.var.name, " = ", base.var.name)
        dc <- dc %>% compute.var_(character(0), var.name %>% setNames (base.var.name))
        intermediary.var.names <- base.var.name
        
        ## TODO: check if intermediate variables are not computed several times
        for (i in seq_along (var.model.form$var.names)) {
            current.var.name <- var.model.form$var.names[[i]]
            current.top.dim.names <- var.model.form$top.dim.names[[i]]
            current.bot.dim.names <- var.model.form$bot.dim.names[[i]]

            if (current.var.name != "NULL") {
                current.top.var.name <- paste0 (current.var.name, ".", paste (current.top.dim.names, collapse = "."))
                current.bot.var.name <- paste0 (current.var.name, ".", paste (current.bot.dim.names, collapse = "."))

                dc <- dc %>%
                    compute.var_(current.top.dim.names, current.var.name %>% setNames (current.top.var.name)) %>%
                    compute.var_(current.bot.dim.names, current.var.name %>% setNames (current.bot.var.name))

                model.var.mutate <- paste0 (model.var.mutate, " * ", current.top.var.name, " / ", current.bot.var.name)
                intermediary.var.names <- c (intermediary.var.names, current.top.var.name, current.bot.var.name)
            } else {
                current.dim.names <- current.top.dim.names %>% setdiff (current.bot.dim.names)
                for (current.dim.name in current.dim.names) {
                    current.var.name <- paste0 (current.dim.name, ".elm.nb")
                    current.var.mutate <- paste0 (current.var.name, " = nrow (plane (dc, '", current.dim.name, "'))")
                    dc <- dc %>% mutate.var_(current.dim.name, current.var.mutate)

                    model.var.mutate <- paste0 (model.var.mutate, " * 1 / ", current.var.name)
                    intermediary.var.names <- c (intermediary.var.names, current.var.name)
                }
            }
        }
        
        dc <- dc %>% mutate.var_(data.dim.names, model.var.mutate)

        attr (vars (dc, model.var.name), "NA.value") <- 0 # as.numeric (NA)

        
        ## Remove intermediary variables
        if (! keep.inter.var) {
            dc <- dc %>% select.var (- one_of (intermediary.var.names))
        }
        
        return (dc)
    }


        
compute.var.deviation <- function (obj, ...) { UseMethod ("compute.var.deviation") }
compute.var.deviation.data.cube <-
    function (dc, var.form, deviation.type = "ratio") {
        var.form.str <- deparse (substitute (var.form))
        dc %>% compute.var.deviation_(var.form.str, deviation.type)
    }

compute.var.deviation_ <- function (obj, ...) { UseMethod ("compute.var.deviation_") }
compute.var.deviation_.data.cube <-
    function (dc, var.form, deviation.type = "ratio") {
        
        ## Check variable formula
        var.form <- parse.var.form (dc, var.form)

        if (length (var.form$var.names) > 1 || ! is.null (var.form$bot.dim.names[[1]])) {
            stop ("Only models of univariate data are currently implented. Variable formula should contain only one term.")
        }

        ## Reprint form
        message (paste0 ("Computing deviation of '", var.form$str, "'"))

        ## Variable names
        var.name <- var.form$var.names[[1]]
        data.dim.names <- var.form$top.dim.names[[1]]

        data.var.name <- var.name
        base.var.name <- paste0 (var.name, ".")
        model.var.name <- paste0 (var.name, ".model")
        deviation.var.name <- paste0 (var.name, ".deviation")

        ## COMPUTE DEVIATION        
        if (deviation.type == "ratio") {
            deviation.var.mutate <- paste0 (deviation.var.name, " = ", data.var.name, " / ", model.var.name)
        }

        if (deviation.type == "poisson") {
            deviation.var.mutate <- paste0 (deviation.var.name, " = ifelse (", data.var.name, " < ", model.var.name, ", ppois (", data.var.name, ", ", model.var.name, ", lower.tail = TRUE, log.p = TRUE), - ppois (", data.var.name, ", ", model.var.name, ", lower.tail = FALSE, log.p = TRUE))")
        }
        
        if (deviation.type == "KLdiv") {            
            base.var.name <- paste0 (var.name, ".")
            base.var.value <- (dc %>% select.dim %>% select.var (var.name) %>% as.data.frame) [[var.name]]

            deviation.var.mutate <- paste0 (deviation.var.name, " = ifelse (", data.var.name, " > 0, ", data.var.name, " / ", base.var.value, " * log2 (", data.var.name, " / ", model.var.name, "), 0)")
        }
        
        if (deviation.type == "chi2") {
            deviation.var.mutate <- paste0 (deviation.var.name, " = sign (", data.var.name, " - ", model.var.name, ") * (", data.var.name, " - ", model.var.name, ")^2 / ", model.var.name)
        }

        dc <- dc %>% mutate.var_(data.dim.names, deviation.var.mutate)
        
        if (deviation.type == "ratio") {
            attr (vars (dc, deviation.var.name), "NA.value") <- 1
        } else {
            attr (vars (dc, deviation.var.name), "NA.value") <- 0
        }

        return (dc)
    }


compute.var.outlier <- function (obj, ...) { UseMethod ("compute.var.outlier") }
compute.var.outlier.data.cube <-
    function (dc, var.form, outlier.threshold = NA, outlier.rule = NULL) {
        var.form.str <- deparse (substitute (var.form))
        dc %>% compute.var.outlier_(var.form.str, outlier.threshold, outlier.rule)
    }

compute.var.outlier_ <- function (obj, ...) { UseMethod ("compute.var.outlier_") }
compute.var.outlier_.data.cube <-
    function (dc, var.form, outlier.threshold = NA, outlier.rule = NULL) {

        ## Check variable formula
        var.form <- parse.var.form (dc, var.form)

        if (length (var.form$var.names) > 1 || ! is.null (var.form$bot.dim.names[[1]])) {
            stop ("Only models of univariate data are currently implented. Variable formula should contain only one term.")
        }

        ## Reprint form
        message (paste0 ("Computing outlier values of '", var.form$str, "'"))

        ## Variable names
        var.name <- var.form$var.names[[1]]
        data.dim.names <- var.form$top.dim.names[[1]]

        data.var.name <- var.name
        var.deviation.name <- paste0 (var.name, ".deviation")
        var.outlier.name <- paste0 (var.name, ".outlier")
        
        ## APPLY THRESHOLD OR RULE
        if (is.null (outlier.rule)) {
            if (length (outlier.threshold) == 1) {
                if (is.na (outlier.threshold)) {
                    outlier.threshold <- dc %>% vars (var.deviation.name) %>% attr ("NA.value") %>% rep (2)
                } else {
                    outlier.threshold <- outlier.threshold %>% rep (2)
                }
            }

            outlier.var.mutate <- paste0 (var.outlier.name, " = as.integer (findInterval (", var.deviation.name, ", c (", outlier.threshold[1], ", ", outlier.threshold[2], ")) - 1)")

        } else {
            data <- plane (dc, data.dim.names) %>% pull (var.name)
            dev.mean <- mean (data)
            dev.sd <- sd (data)

            outlier.var.mutate <- paste0 (var.outlier.name, " = as.integer (findInterval (", var.deviation.name, ", ", dev.mean, " + ", dev.sd, " * ", outlier.rule, " * c(-1, 1)) - 1)")

        }
        
        dc <- dc %>% mutate.var_(data.dim.names, outlier.var.mutate)

        return (dc)
    }


compute.vars.outlier <- function (obj, ...) { UseMethod ("compute.vars.outlier") }
compute.vars.outlier.data.cube <-
    function (dc, var.form, var.model.form = NULL, deviation.type = "ratio", outlier.threshold = NA, outlier.rule = NULL) {
        var.form.str <- deparse (substitute (var.form))

        var.model.form.str <- deparse (substitute (var.model.form))
        if (var.model.form.str == "NULL") { var.model.form.str <- NULL }
            
        dc %>% compute.vars.outlier_(var.form.str, var.model.form.str, deviation.type, outlier.threshold, outlier.rule)
    }

compute.vars.outlier_ <- function (obj, ...) { UseMethod ("compute.vars.outlier_") }
compute.vars.outlier_.data.cube <-
    function (dc, var.form, var.model.form = NULL, deviation.type = "ratio", outlier.threshold = NA, outlier.rule = NULL) {        
        dc %>%
            compute.var.model_(var.form, var.model.form) %>%
            compute.var.deviation_(var.form, deviation.type) %>%
            compute.var.outlier_(var.form, outlier.threshold, outlier.rule)
    }

        

select.dim <- function (obj, ...) { UseMethod ("select.dim") }
select.dim.data.cube <-
    function (dc, ...) {
        #str.dim.names <- dot.names (enquos (...))
        dim.names <- dim.names (dc)
        dim.names <- dim.names %>% setNames (dim.names)
        str.dim.names <- tibble (!!! dim.names, .rows = 0) %>% select (...) %>% names

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

        ## Adjust dp.arrange.var.names
        if (! is.null (attr (dc, "arrange.var.names"))) {
            for (dp.name in names (attr (dc, "arrange.var.names"))) {
                dp.dim.names <- data.plane.dim.names (dp.name)
                if (! all (dp.dim.names %in% dim.names)) { attr (dc, "arrange.var.names") [[dp.name]] <- NULL }
            }
            if (length (attr (dc, "arrange.var.names")) == 0) { attr (dc, "arrange.var.names") <- NULL }
        }
        

        if (! is.null (names (dim.names))) { dc <- dc %>% rename.dim_(dim.names [names (dim.names) != ""]) }
        dc <- dc %>% reorder.dim_()

        return (dc)
    }



select.var <- function (obj, ...) { UseMethod ("select.var") }
select.var.data.cube <-
    function (dc, ...) {
        # str.var.names <- dot.names (enquos (...))
        var.names <- var.names (dc)
        var.names <- var.names %>% setNames (var.names)
        str.var.names <- tibble (!!!var.names, .rows = 0) %>% select (...) %>% names
        
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
            if (length (dc[[dp.name]]) == data.plane.dim.nb (dp.name)) {
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

        ## Adjust dp.arrange.var.names
        if (! is.null (attr (dc, "arrange.var.names"))) {
            for (dp.name in names (attr (dc, "arrange.var.names"))) {
                for (i in seq_along (attr (dc, "arrange.var.names") [[dp.name]])) {
                    var.name <- attr (dc, "arrange.var.names") [[dp.name]][[i]]
                    f <- function () {}
                    body (f) <- parse_expr (var.name)
                    struct <- codetools::findGlobals (f, merge = FALSE)
                    if (any (struct$variables %in% (old.var.names %>% setdiff (var.names)))) { attr (dc, "arrange.var.names") [[dp.name]][[i]] <- "" }
                }

                attr (dc, "arrange.var.names") [[dp.name]] <- attr (dc, "arrange.var.names") [[dp.name]] [attr (dc, "arrange.var.names") [[dp.name]] != ""]

                ## Remove data.plane if empty
                if (length (attr (dc, "arrange.var.names") [[dp.name]]) == 0) {
                    attr (dc, "arrange.var.names") [[dp.name]] <- NULL
                }
            }
            if (length (attr (dc, "arrange.var.names")) == 0) { attr (dc, "arrange.var.names") <- NULL }
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
                dc[[dp.name]] <- dc[[dp.name]] %>% filter (!! filter_expr) %>% dplyr::mutate (!! dim.name := !! mutate_expr)
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


top_n.elm <- function (obj, ...) { UseMethod ("top_n.elm") }
top_n.elm.data.cube <- function (dc, dim.name, var.name, n) {
    dim.name <- substitute (dim.name)
    var.name <- substitute (var.name)
    if (n < 0) {
        bquote (filter.elm (dc, .(dim.name), min_rank (.(var.name)) <= -.(n))) %>% eval
    } else {
        bquote (filter.elm (dc, .(dim.name), min_rank (dplyr::desc (.(var.name))) <= .(n))) %>% eval
    }
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
    ## TODO: suppress multiple occurrences of the same arrangement

    if (is.null (attr (dc, "arrange.var.names"))) { attr (dc, "arrange.var.names") <- list() }

    dp.name <- plane.name (dc, dim.names)

    ##if (is.null (plane (dc, dim.names))) { plane (dc, dim.names) <- empty.plane (dim.names) }

    if (is.null (attr (dc, "arrange.var.names") [[dp.name]])) { attr (dc, "arrange.var.names") [[dp.name]] <- character(0) }
    attr (dc, "arrange.var.names") [[dp.name]] <- append (unname (var.names), attr (dc, "arrange.var.names") [[dp.name]])

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

    for (dp.name.2 in names (attr (dc, "arrange.var.names"))) {
        
        dim.names.2 <- data.plane.dim.names (dp.name.2)

        if (all (dim.names.2 %in% dim.names)) {
            arrange.var.names <- attr (dc, "arrange.var.names") [[dp.name.2]]

            ## Compute involved variables
            for (var.name in rev (arrange.var.names)) {
                
                f <- function () {}
                body (f) <- parse_expr (var.name)
                struct <- codetools::findGlobals (f, merge = FALSE)
                dc <- dc %>% compute.var_(dim.names.2, struct$variables %>% intersect (var.names (dc)))

                ## Arrange elements
                dc[[dp.name.2]] <-
                    parse (text = paste0 ("dc[[dp.name.2]] %>% arrange (", var.name, ")")) %>% eval

                ## if (length (struct$functions) == 0) {
                ##     dc[[dp.name.2]] <- dc[[dp.name.2]] %>% arrange (!! rlang::sym(struct$variables[[1]]))
                ## } else {
                ##     dc[[dp.name.2]] <- dc[[dp.name.2]] %>% arrange (get (struct$functions[[1]]) (!! rlang::sym(struct$variables[[1]])))
                ## }
                
            }
                        
            dc[[dp.name]] <- dc[[dp.name.2]] %>%
                select (dim.names.2) %>%
                inner_join (dc[[dp.name]], by = dim.names.2)
        }
    }

    dc <- dc %>% reorder.dim ()
    
    return (dc)
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
                        geom_point (aes (size = ifelse (value == 0, NA, value)), pch = 21, color = "black", fill = "grey", na.rm = TRUE) +
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
                        geom_point (aes (size = ifelse (get (var.names) == 0, NA, get (var.names))), pch = 21, color = "black", fill = "grey", na.rm = TRUE) +
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
                    geom_point (aes (size = ifelse (value == 0, NA, value)), pch = 21, color = "black", fill = "grey", na.rm = TRUE)
            }
        }

        ## Add origin line
        if (type %in% c ("line", "bar") && all (sapply (attr (vars (dc, var.names), "NA.value"), is.numeric))) {
            NA.values <- unique (unlist (attr (vars (dc, var.names), "NA.value")))
            if (length (NA.values) == 1) {
                p <- p + geom_hline (yintercept = NA.values, color = "grey")

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



plot.var.outlier <- function (obj, ...) { UseMethod ("plot.var.outlier") }
plot.var.outlier.data.cube <-
    function (dc, var.form, log = "", labels = "only.outliers") {
        var.form.str <- deparse (substitute (var.form))
        dc %>% plot.var.outlier_(var.form.str, log = log, labels = labels)
    }

plot.var.outlier_ <- function (obj, ...) { UseMethod ("plot.var.outlier_") }
plot.var.outlier_.data.cube <-
    function (dc, var.form, log = "", labels = "only.outliers") {

        ## Check variable formula
        var.form <- parse.var.form (dc, var.form)

        if (length (var.form$var.names) > 1 || ! is.null (var.form$bot.dim.names[[1]])) {
            stop ("Only models of univariate data are currently implented. Variable formula should contain only one term.")
        }

        ## Reprint form
        message (paste0 ("Plotting outlier values of '", var.form$str, "'"))

        ## Variable names
        var.name <- var.form$var.names[[1]]
        data.dim.names <- var.form$top.dim.names[[1]]

        data.var.name <- var.name
        data.var.names <- paste0 (var.name, c ("", ".model", ".deviation", ".outlier"))
        var.model.name <- paste0 (var.name, ".model")
        var.deviation.name <- paste0 (var.name, ".deviation")
        var.outlier.name <- paste0 (var.name, ".outlier")

        var.NA.value <- dc %>% vars (var.name) %>% attr ("NA.value")
        var.deviation.NA.value <- dc %>% vars (var.deviation.name) %>% attr ("NA.value")

        if (labels == "all") {
            label.mutate.str <- paste0 ("paste (", paste (data.dim.names, collapse = ", "), ", sep = \" / \")")
        } else if (labels == "only.outliers") {
            label.mutate.str <- paste0 ("ifelse (", var.outlier.name, " != 0, paste (", paste (data.dim.names, collapse = ", "), ", sep = \" / \"), \"\")")
        } else {
            label.mutate.str <- paste0 ("")
        }

        dc %>%
            select.var_(data.var.names) %>%
            select.dim_(data.dim.names) %>%
            as.data.frame %>%
            filter (!! parse_expr (paste0 (data.var.name, " != ", var.NA.value))) %>%
            dplyr::mutate (label := !! parse_expr (label.mutate.str)) %>%
            ggplot (aes (x = get (var.name), y = get (var.deviation.name), label = label)) +
            ## ggplot (aes (x = get (var.model.name), y = get (var.name), label = label)) +
            geom_point (
                aes (
                    shape = factor (get (var.outlier.name), levels = c ("1", "0", "-1")),
                    fill = get (var.deviation.name),
                    size = abs (get (var.deviation.name))
                )
            ) + {
                if (labels %in% c ("all", "only.outliers")) { geom_text (vjust = 2) }
            } +
            scale_shape_manual (name = var.outlier.name, values = c (24, 22, 25), drop = FALSE, guide = guide_legend (order = 1)) + # , labels = c ("positive","normal","negative"),
            scale_fill_gradient2 (name = var.deviation.name, low = "blue", mid = "white", high = "red", midpoint = var.deviation.NA.value, guide = guide_colourbar (order = 2)) +
            scale_size_continuous (name = paste0 ("abs (", var.deviation.name, ")"), guide = guide_legend (order = 3)) +
            geom_hline (yintercept = var.deviation.NA.value) +
            ## geom_abline (intercept = 0, slope = 1) +
            xlab (var.name) + ylab (var.deviation.name) + {
            ## xlab (var.model.name) + ylab (var.name) + {
                if (str_detect (log, "x")) { scale_x_log10 () }
            } + {
                if (str_detect (log, "y")) { scale_y_log10 () }
            }
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
