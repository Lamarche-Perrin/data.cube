rm(list=ls())

library ("rgl")
source ('../data.cube.R')

df <- read.csv ('../data/guardian.small.csv', stringsAsFactors=FALSE)

cell3d <- function (x, y, z, dx, dy, dz, color="green", alpha=1, lwd=5) {
    shade3d (translate3d (scale3d (cube3d (color=color, alpha=alpha), dx/2, dy/2, dz/2), x+dx/2, y+dy/2, z+dz/2))
    wire3d (translate3d (scale3d (cube3d (color="black"), dx/2, dy/2, dz/2), x+dx/2, y+dy/2, z+dz/2), lwd=lwd)
}


dims <- c("user","topic","time")
dev.dims <- c("time","topic")

dc <- as.data.cube (df)
dc <- remove.dims (dc, dims=c("user"))
dc <- select.elems (dc, elems=list(topic="Access_to_energy"))
str(dc)


open3d (userMatrix = rotationMatrix (pi/5,1,0,0) %*% rotationMatrix (pi/4,0,1,0))

cv <- list()
dv <- list()

for (i in 1:3) {
    if (dims[i] %in% dc$dim.names) {
        cv[[i]] <- 1:5 #min(length(dc$elem.names[[dims[i]]]),5)
        dv[[i]] <- 1
    } else {
        cv[[i]] <- c(1)
        dv[[i]] <- 5
    }

    if (dims[i] %in% dev.dims) {
        cv[[i]] <- append(-1,cv[[i]])
    }
}

for (x in cv[[1]]) {
    for (y in cv[[2]]) {
        for (z in cv[[3]]) {
            if (x == -1) { dx <- 1 } else { dx <- dv[[1]] }
            if (y == -1) { dy <- 1 } else { dy <- dv[[2]] }
            if (z == -1) { dz <- 1 } else { dz <- dv[[3]] }
            if (x == -1 || y == -1 || z == -1) { color <- "blue" } else { color <- "green" }
            cell3d (x, y, z, dx, dy, dz, color=color)
        }
    }
}

cell3d (1, 1, 1, 5, 5, 5, color="grey", alpha=0.1, lwd=3)
