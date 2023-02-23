make.rectangle <- function(x,y) {
    square <- t(matrix(c(-1,1,1,1,1,-1,-1,-1,-1,1),ncol=2,byrow=T))
    st_polygon(list(t(square * c(x/2,y/2))))
}

rot <- function(a) {
    matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

bbox.angles <- function(p) {
    ## return angle in degrees of major and minor axes.
    rbb <- st_minimum_rotated_rectangle(p)
    rbm <- as.matrix(rbb[[1]])
    p12 <- rbm[1,] - rbm[2,]
    p23 <- rbm[2,] - rbm[3,]
    d1 <- sum(p12 * p12)
    d2 <- sum(p23 * p23)
    a1 <- atan(p12[2]/p12[1])*180/pi
    a2 <- atan(p23[2]/p23[1])*180/pi

    if (d1 > d2) {
        out <- c(a1,a2)
    } else {
        out <- c(a2,a1)
    }

    out
}

showme <- function(marin, rects, results, np, nr) {
    a <- results[[np]][[nr+1]]$par[1]
    x <- results[[np]][[nr+1]]$par[2]
    y <- results[[np]][[nr+1]]$par[3]
    cat("np", np, "r#", r, " angle", a, "x", x, "y", y, "\n")

    rect <- st_sfc(rects[[nr]])


    rect.t <- rect * rot(a/180*pi) + c(x,y)

    lot <- marin$geometry[np]
    lot <- lot - st_centroid(lot)

    cat("lot", is(lot),"\n")
    cat("rect.t", is(rect.t),"\n")
    p <- ggplot(c(lot, rect.t)) + geom_sf(alpha=.6)

    p
}

objfun <- function(lot, rect, graph=FALSE, debug=FALSE) {
    lot.a <- st_area(lot)
    rect.a <- st_area(rect)
    rect.t <- rect
    iter <- 0
    function(par) {
        a <- par[1]
        x <- par[2]
        y <- par[3]
        iter <<- iter + 1
        rect.t <<- rect * rot(a/180*pi) + c(x,y)
        coverage <- st_area(st_intersection(lot, rect.t))/rect.a
        if (length(coverage) == 0) {
            val <- 1
        } else {
            val <- 1 - coverage
        }
        if (graph) {
            ##png(file=sprintf("x/g%05d.png", iter))
            plt <- ggplot(c(lot[[1]], rect.t)) + geom_sf(alpha=.7)
            show(plt)
            Sys.sleep(.25)
            ##dev.off()
        }
        if (debug) {
            cat(rect.a, "[", iter, "]", par, " = ", val, "\n")
        }
        val
    }
}

if (!exists("marin") || !is(marin, "sf")) {
    marin <- read_sf("~/src/housing/marin/data/Parcel/Parcel.shp")
}


if (FALSE) {
    prop <- marin[176,]
    lot <- prop$geometry - st_centroid(prop$geometry)
    arect <- make.rectangle(150, 80)

    f <- objfun(lot, arect, graph=F)
    res <- optim(c(0,0,0), f)
}

opt.lot <- function(prop, rects) {
    rl <- list(prop)
    lot <- prop$geometry - st_centroid(prop$geometry)

    ## center the rectangle on the largest inscribed circle
    cir <- st_inscribed_circle(lot, nQuadSegs=0)[[1]][1,]
    a <- bbox.angles(lot)
    prev <- c(-a[1], cir)
    value <- 1
    ## cat("initial", prev, "\n")
    for (rect in rects) {
        if (value < 1e-6) {
            ## if the previous lot fit then we know smaller one does
            res$counts <- c(0,NA)
            res$message <- "covered by previous"
            rl <- append(rl, list(res))
            next
        }

        f <- objfun(lot, rect)
        res <- try(optim(prev, f, control=list(abstol=1e-5, reltol=1e-5)))

        if (!is(res, "try-error")) {
            prev <- res$par
            value <- res$value
            ## cat(prop$PropID, " ", res$value, res$counts, res$par,"\n")
        } else {
            value <- 1
        }

        rl <- append(rl, list(res))
    }

    rl
}

rects <- list(make.rectangle(150, 80)
            , make.rectangle(125, 75)
            , make.rectangle(100, 50))


if (TRUE) {
    results <- list()
    eps <- 1e-5
    tic("full")
    tic("first")
    for (i in 1:nrow(marin)) {
        prop <- marin[i,]

        rl <- opt.lot(prop, rects)
        results[[prop$PropID]] <- rl

        if (i %% 5000 == 0) {
            toc()
            tic(paste0("run ",i))
            ## save(results, file=sprintf("~/tmp/marin/run%06d.save", i))
        }
    }
    save(results, file="~/tmp/marin/run.full.save")
    toc()
    toc()
}



##p <- showme(marin, rects, results, 97, 1)
##res.df <- ldply(results, function(x) data.frame(PropID=x[[1]]$PropID, starea=x[[1]]$SHAPESTAre, l1=x[[2]]$value, l2=x[[3]]$value, l3=x[[4]]$value), .id=NULL)

