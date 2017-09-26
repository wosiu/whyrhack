library(parallel)
point_in_polygon <- function(point_lon, point_lat, multipol) {
    tmp <- gsub(multipol, pattern = "[^0-9 \\.,]", replacement = "")
    tmp2 <- strsplit(tmp, split=",")
    tmp3 <- lapply(tmp2, strsplit, split = " ")
    tmp4 <- lapply(tmp3, function(x) {
        tt <- sapply(x, function(y) {
            as.numeric(y)
        })
        tt <- cbind(tt, tt[,1])
        tt
    })
    pol_x <- tmp4[[1]][1, ] 
    pol_y <- tmp4[[1]][2, ]
    point.in.polygon(point_lon, point_lat, pol_x, pol_y)
}

library(rgdal)

find_zone <- function(point_lon, point_lat, zones_dict) {
    in_zone <- apply(zones_dict, 1, function(x) {
        point_in_polygon(point_lon, point_lat, x[2])
    }) 
    if (sum(in_zone) == 0) {
        return(-1)
    } 
    return(which.max(in_zone))
}

bus_all$my_lon <- bus_all$lon / 1000000
bus_all$my_lat <- bus_all$lat / 1000000
zones_dict <- orange %>% 
    select(zoneid, geom4326) %>% 
    unique()

find_zone(bus_all$my_lon[6], bus_all$my_lat[6], zones_dict)
zones = mclapply(bus_all[1:10,], function(r) {find_zone(r$my_lon, r$my_lat, zones_dict)}, mc.cores=6 )
