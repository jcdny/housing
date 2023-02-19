make.rectangle <- function(x,y) {
    square <- t(matrix(c(-1,1,1,1,1,-1,-1,-1,-1,1),ncol=2,byrow=T))
    st_polygon(list(t(square * c(x/2,y/2))))
}

rot <- function(a) {
    matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

objfun <- function(lot, rect, graph=FALSE, debug=FALSE) {
    lot.a <- st_area(lot)
    rect.a <- st_area(rect)
    rect.t <- rect.a
    iter <- 0
    function(v) {
        a <- v[1]
        x <- v[2]
        y <- v[3]
        iter <<- iter + 1
        rect.t <<- rect * rot(a/180*pi) + c(x,y)

        val <- 1 - st_area(st_intersection(lot, rect.t))/rect.a
        if (graph) {
            png(file=sprintf("x/g%05d.png", iter))
            print(ggplot(c(lot,rect.t)) + geom_sf(alpha=.6))
            dev.off()
        }
        if (debug) {
            cat(v," = ", val, "\n")
        }
        val
    }
}

rect <- make.rectangle(125,70)

f <- objfun(xx[[176]], rect, graph=T)
