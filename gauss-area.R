#!/usr/bin/Rscript
# test the gauss area formula

#### functions ####
gauss_area <- function(x,y) {
    # first method
    xnm1 <- x[c(length(x),1:(length(x)-1))]
    xnp1 <- x[c(2:length(x),1)]
    area_1 <- 0.5 * sum(y * (xnm1 - xnp1))
    # second method
    ynm1 <- y[c(length(y),1:(length(y)-1))]
    ynp1 <- y[c(2:length(y),1)]
    area_2 <- 0.5 * sum(x * (ynp1 - ynm1))
    stopifnot(all.equal(area_1,area_2))
    return(area_1)
}
# test it (poorly)
stopifnot(gauss_area(x = c(0,1,1,2,2,1,0,0), y =c(0,0,1,1,2,2,2,1))==3)

remove_equal_first_digits <- function(x) {
    # make it a number below 1
    # i.e. divide by 10 power the number of digits before the comma
    digits_before_comma <- nchar(as.character(max(floor(x))))
    as_comma <- x/10^digits_before_comma
    res <- as_comma
    
    for (equal_first_digits in 16:1){
        rounded <- unique(floor(as_comma*10^equal_first_digits))/10^equal_first_digits
        if(length(rounded) == 1) break
    }
    
    res <- ( as_comma - rounded ) * 10 ^ digits_before_comma
    return(res)
}

#### generate random polygon ####
# number of points
POLYNUM <- 10
# ~radius of points from center
POLYRADIUS <- 10
# Paradeplatz Mannheim as example
POLYCENTER <- c(rechts=3461404.0,hoch=5483498.0)
# angles around the center
polyangles <- sort(runif(n = POLYNUM, min = 0, max = 2*pi))
# radii from center
polyradii <- runif(n = POLYNUM, min = POLYRADIUS * 0.7, max = POLYRADIUS * 1.3)
# create polynom coordinates
POLYGON <- list(
    rechts = POLYCENTER["rechts"] + polyradii * cos(polyangles),
    hoch = POLYCENTER["hoch"] + polyradii * sin(polyangles)
)

#### Plot polygon ####
plot_polygon <- function(x,y,title="") {
    # base plot
    plot(NA, type = "n"
         ,main = title
         ,xlim = range(x)
         ,ylim = range(y)
         ,xlab = "Rechtswert y [m]"
         ,ylab = "Hochwert x [m]"
         ,panel.first = grid(col="darkgray")
    )
    # draw the polygon
    polygon(
        x = x
        ,y = y
        ,col = "#00ff0044"
        ,border = "black"
    )
    # show area
    area <- gauss_area(x = x, y = y)
    center <- c(x=mean(x),y=mean(y))
    text(x = center["x"], y = center["y"],
         labels = paste(round(area,digits=2),"m^2"),
         adj = c(0.5,0.5)
         )
}

par(mfrow = c(1,3),ps=16)
# original polygon
plot_polygon(x = POLYGON$rechts, y = POLYGON$hoch,
             title="Original")
# intelligently shifted polygon
# remove equal first digits
POLYGON_EASIER <- list(
    rechts = remove_equal_first_digits(POLYGON$rechts),
    hoch = remove_equal_first_digits(POLYGON$hoch)
)
plot_polygon(x = POLYGON_EASIER$rechts,y = POLYGON_EASIER$hoch,
             title="Die ersten gleichen Ziffern weggelassen")
# randomly shifted
POLYGON_SHIFTED <- list(
    rechts = POLYGON$rechts - runif(n = 1, 
        min = mean(POLYGON$rechts) - abs(max(POLYGON$rechts)),
        max = mean(POLYGON$rechts) + abs(max(POLYGON$rechts))),
    hoch = POLYGON$hoch - runif(n = 1, 
        min = mean(POLYGON$hoch) - abs(max(POLYGON$hoch)),
        max = mean(POLYGON$hoch) + abs(max(POLYGON$hoch)))
)
plot_polygon(x = POLYGON_SHIFTED$rechts,y = POLYGON_SHIFTED$hoch,
             title = "Irgendwelchwas von \n Rechts-/Hochwert abgezogen")