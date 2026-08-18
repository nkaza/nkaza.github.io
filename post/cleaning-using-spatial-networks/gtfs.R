
library(tidyverse)
library(tidytransit)
library(gtfsrouter)
library(sf)

setwd('/Users/kaza/Dropbox/website_new/website/content/post/2022-10-26-cleaning-using-spatial-networks/')

gtfs_feeds <- read_csv("https://bit.ly/catalogs-csv") %>%
               filter(provider == "Metro St. Louis") %>%
               filter(data_type == "gtfs") %>%
               pull('urls.direct_download')

# outDirs <- c("bus", 'rail')
# 
# outDir <- tools::file_path_sans_ext(basename(feed_url)) %>% str_split("_", simplify = T) %>% .[,2]
# unzip(download.file(feed_url), exdir = outDir)
# trips <- read_csv(file.path(outDir, "trips.txt"))
# routes <- read_csv(file.path(outDir, "routes.txt"))
# stop_times <- read_csv(file.path(OutDir, "stop_times.txt"), col_types= cols(arrival_time = col_character(), departure_time = col_character()))
# 

# stlouis_gtfs <- map(gtfs_feeds, function(feed_url){
#   zipfilename <- basename(feed_url)
#   download.file(feed_url, destfile = zipfilename)
#   gtfstools::read_gtfs(zipfilename)
# }) %>% 
#   gtfstools::merge_gtfs(prefix=T)

download_read_gtfs <- function(feed_url){
      zipfilename <- basename(feed_url)
     download.file(feed_url, destfile = zipfilename)
      gtfsrouter::extract_gtfs(zipfilename)
}

stlouis_gtfs <- gtfs_feeds %>% download_read_gtfs()
                  

transitstops <- st_as_sf(stlouis_gtfs$stops, coords = c("stop_lon", "stop_lat"), crs=4326)

#stlouis_boundary <- st_as_sfc(st_bbox(transitstops)) %>% st_buffer



blkgrp_centroid <- list()

blkgrp_centroid[[1]] <- tigris::block_groups(state="MO", county = c('510', '189')) %>% 
                        st_centroid() %>% 
                        st_transform(st_crs(transitstops)) %>%
                        select(GEOID) 

blkgrp_centroid[[2]] <- tigris::block_groups(state="IL", county = '163') %>% 
  st_centroid() %>% 
  st_transform(st_crs(transitstops)) %>%
  select(GEOID) 


blkgrps <- do.call(rbind, blkgrp_centroid)


library(tmap)

tmap_mode('view')
tm_shape(transitstops) +
  tm_dots(col = 'red') +
  tm_shape(blkgrps) +
  tm_dots(col = 'green')




library(osrm)

options(osrm.server = "http://localhost:5000/")
options(osrm.profile = 'foot') 


ttmatrix <- osrmTable(src=blkgrps, dst=transitstops, measure = c('duration'))
ttmatrix$durations <- round(ttmatrix$durations)

stop_iso <- list()
stop_iso <- map(1:nrow(transitstops), possibly(function(x){osrmIsochrone(loc=transitstops[x,], breaks = seq(0,45,1))}, otherwise = NULL))

blkgrp_iso <- list()
blkgrp_iso <- map(1:nrow(blkgrps), possibly(function(x){osrmIsochrone(loc=blkgrps[x,], breaks = seq(0,45,1))}, otherwise = NULL))




############ Start

names(stop_iso) <- stlouis_gtfs$stops$stop_id
names(blkgrp_iso) <- blkgrps$GEOID

stlouis_gtfs$transfers <- gtfs_transfer_table(stlouis_gtfs)

stlouis_gtfs_wkday <- gtfs_timetable(stlouis_gtfs, day = "Tuesday")



library(lubridate)
library(tictoc)
sf_use_s2(FALSE)



stop_iso_old <- stop_iso
blkgrp_iso_old <- blkgrp_iso

stop_iso <- map(stop_iso, ~nngeo::st_remove_holes(.))




get_transitsheds <-
  function(blkgrp_id,
           blkgrps = blkgrps,
           gtfsdata = stlouis_gtfs,
           start_times,
           day_of_week = "Tuesday",
           blk_isochorones = blkgrp_iso,
           stop_isochrones = stop_iso,
           blkgrp_stop_ttmatrix = ttmatrix$durations) {
    geo_temp_sf2 <-
      blk_isochorones[[blkgrp_id]] %>% slice_max(isomax, n = 1) %>% nngeo::st_remove_holes() %>% st_geometry() %>% st_make_valid()
    
    
    
    row.names(blkgrp_stop_ttmatrix) <- blkgrps$GEOID
    
    if(!exists("transfers", gtfsdata)){gtfsdata$transfers <- gtfs_transfer_table(gtfsdata)}
    
    gtfsdata <- gtfs_timetable(gtfsdata, day = day_of_week)
    
    
    geo_sf <- list()
    
    for (blk_start_time in start_times) {
      geo_sf[[blk_start_time]] <- geo_temp_sf2
      travel_to_stations <-
        tibble(tt = blkgrp_stop_ttmatrix[blkgrp_id,],
               stop_id = gtfsdata$stops$stop_id) %>%
        filter(tt < 45) %>%
        mutate(start_time1 = blk_start_time + tt * 60,
               start_time2 = blk_start_time + 45 * 60)
      
      if (nrow(travel_to_stations) == 0) {
        next
      }
      else {
        remain_time_dt <-
          map_dfr(1:nrow(travel_to_stations), function(idx) {
            gtfs_traveltimes(
              gtfsdata,
              from = travel_to_stations[idx, ] %>% pull('stop_id'),
              start_time_limits = travel_to_stations[idx, ] %>% select(c("start_time1", "start_time2")) %>% as.numeric(),
              from_is_id = TRUE,
              max_traveltime = travel_to_stations[idx, ] %>% select(c("start_time1", "start_time2")) %>% as.numeric() %>% diff()
            ) %>%
              filter(ntransfers < 3)
          }) %>%
          mutate(
            end_time = as.numeric(start_time + duration),
            remain_time = (blk_start_time + 45 * 60) - end_time
          ) %>%
          filter(remain_time > 0) %>%
          select(stop_id, remain_time) %>%
          bind_cols(tibble("GEOID" = blkgrp_id, "start" = blk_start_time))
        
        if (nrow(remain_time_dt) == 0) {
          next
        } else {
          geo_temp_sf1 <- map_dfr(1:nrow(remain_time_dt), function(idx) {
            stop_isochrones[[remain_time_dt[idx,]$stop_id]] %>%
              filter(isomax <= remain_time_dt[idx,]$remain_time / 60) %>%
              slice_max(isomax, n = 1)
          }) %>%
            st_union() %>%
            st_make_valid()
          
          geo_sf[[blk_start_time]] <- st_union(geo_temp_sf2, geo_temp_sf1)  %>%
            st_sf(GEOID = blkgrp_id, start = blk_start_time) %>%
            st_make_valid()
          
        }
        
      }
      
    }
    
    geo_sf <- bind_rows(geo_sf)
    geo_sf$start <- as.character(lubridate::seconds_to_period(round(geo_sf$start)))
    
    return(geo_sf)
  }




k_tues<- get_transitsheds(blkgrp_id = blkgrps$GEOID[900], blkgrps = blkgrps, start_times = seq(6,8,.083333)*3600, day_of_week = "Tuesday")


options(osrm.server = "http://localhost:5000/")
options(osrm.profile = 'car') 


bikeisochrone <- osrmIsochrone(loc=blkgrps[900,], breaks = 45}

carisochrone <- osrmIsochrone(loc=blkgrps[900,], breaks = 45}

library(tmaptools)

# read OSM raster data
osm_NLD <- read_osm(bb(carisochrone, ext=1.5), type = "stamen-toner")

m1_tues <-
  tm_shape(osm_NLD) +
  tm_rgb() +
  tm_shape(blkgrps[900,]) +
  tm_dots(col = 'red', size = 1 , shape = 17) +
  tm_shape(bikeisochrone)+
  tm_borders(col = "green", lwd = 2)+
  tm_shape(carisochrone) +
  tm_borders(col = 'orange', lwd = 2)+
  tm_shape(k_tues) +
  tm_fill(col = 'blue', alpha = .5) +
  tm_facets(along = "start", free.coords = FALSE) +
  tm_add_legend(type = "symbol", col = 'red', shape =17, labels = "Block Group (Origin)" )+
  tm_add_legend(type = "fill", col = "blue", labels = "Transit", title = "45 min Isochrones") +
  tm_add_legend(type = "line", col = "green", labels = "Bike") +
  tm_add_legend(type = "line", col = "orange", labels = "Car")

  

tmap_animation(m1_tues, filename = "/Users/kaza/Dropbox/website_new/website/content/post/2022-11-18-transit-accessibility-using-gtfs/images/stlouis2.gif")
