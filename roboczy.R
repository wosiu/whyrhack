# WCZYTANIE BUSOW
library("readr")
library("dplyr")


setwd("offline")
# jak chcesz wczytac wszystkie dni, to zostaw po prostu path="VAVEL")
bus_files <- list.files(path="VAVEL/2017-09-04",
           recursive=T,
           pattern="part"
           ,full.names=T)
bus_parts <- lapply(bus_files, read_csv2, col_names = FALSE)

# enrich
bus_all <- do.call(rbind, bus_parts)
rm(bus_parts)

colnames(bus_all) <- c( "versionID", #String
                        "line",#String,
                        "brigade", #String
                        "time",#String,
                        "lon",#Double,
                        "lat",#Double,
                        "rawLon",#Double,
                        "rawLat",#Double,
                        "status",#String,
                        "delay",#String,
                        "delayAtStop",#String,
                        "plannedLeaveTime",#String,
                        "nearestStop",#String,
                        "nearestStopDistance",#Double,
                        "nearestStopLon",#Double,
                        "nearestStopLat",#Double,
                        "previousStop",#String,
                        "previousStopLon",#Double,
                        "previousStopLat",#Double,
                        "previousStopDistance",#Double,
                        "previousStopArrivalTime",#String,
                        "previousStopLeaveTime",#String,
                        "nextStop",#String,
                        "nextStopLon",#Double,
                        "nextStopLat",#Double,
                        "nextStopDistance",#Double,
                        "nextStopTimetableVisitTime",#String,
                        "courseIdentifier",#String,
                        "courseDirection",#String,
                        "timetableIdentifier",#String,
                        "timetableStatus",#String,
                        "receivedTime",#String,
                        "processingFinishedTime",#String
                        "onWayToDepot" ,#String
                        "overlapsWithNextBrigade",#String
                        "atStop",#String
                        "overlapsWithNextBrigadeStopLineBrigade",#String
                        "speed")

# clean up the data
bus_all$time16 <- substr(bus_all$time, 1, 15)
bus_all$delay_num = as.numeric(bus_all$delay)
# any(is.na(bus_all$delay_num))

# READ ORANGE DATA
orange <- read.csv("measurements.csv", stringsAsFactors = FALSE)
orange$time16 = substr(orange$datetime, 1, 15)
orange_4 <- orange %>% filter(datetime > "2017-09-04 00:00:00", datetime < "2017-09-05 23:59:59")
orange_grp = orange_4 %>% group_by(zoneid, time16) %>% summarise(mean_intensity=mean(intensity))

# pogrupowanie po strefie (orange), godzinie
# todo add zone
delay_zone <- bus_all %>% group_by(time16, zoneid) %>% summarise(mean_deley=mean(delay_num))
                                   
# inner data
joined = delay_zone %>% inner_join(bus_all, by = c("zoneid" = "zoneid", "time16" = "time16"))


# ==========================

# TODO:

#abs(cor(bus_all$delay_zone, decisionClasses))
ggplot(daneBemowo, aes(ymd_hms(datetime), intensity*10000, fill=factor(zoneid))) +
  geom_col() + facet_grid(zoneid~., scales = "free_y") +
  theme_light()
ggplot2(joined)


#joined$cor = abs(cor(joined$mean_intensity, joined$mean_deley))