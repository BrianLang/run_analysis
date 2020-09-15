# library(xml2)
#
# gpx_data <- xml2::read_xml("~/Downloads/2331687206243155 (1)_2018_07_29_12_56_55.gpx")
#
# data_parse <- tibble(
#         lat = "//d1:trkpt/attribute::lat",
#         lon = "//d1:trkpt/attribute::lon",
#         ele = "//d1:trkpt/d1:ele",
#         times = "//d1:trkpt/d1:time",
#         # cad = "//d1:trkpt/d1:extensions/gpxtpx:TrackPointExtension/gpxtpx:cad",
#         # hr = "//d1:trkpt/d1:extensions/gpxtpx:TrackPointExtension/gpxtpx:hr",
#         cadence = "//d1:trkpt/d1:extensions/gpxdata:cadence",
#         power = "//d1:trkpt/d1:extensions/d1:power"
# ) %>%
#         pivot_longer(cols = everything()) %>%
#         mutate(datas = map(value, ~ gpx_data %>%
#                                    xml_child(2) %>%
#                                    xml2::xml_find_all(., .x) %>%
#                                    xml2::xml_text())
#         ) %>%
#         select(-value) %>%
#         pivot_wider(names_from = name, values_from = datas) %>%
#         mutate_at(.vars = c("ele", "times", "cadence", "power"),
#                       ~ list(.x[[1]][gpx_data %>%
#                                              xml_child(2) %>%
#                                              xml_child(3) %>%
#                                              xml_children() %>%
#                                              xml_has_attr("lat")])) %>%
#         unnest(cols = everything())
#
# track_tibble <- data_parse %>%
#         mutate_at(.vars = c("lat", "lon", "ele",
#                             # "cad", "hr",
#                             "cadence", "power"), as.numeric) %>%
#         mutate(times = lubridate::ymd_hms(times)) %>%
#         dplyr::mutate(time_span = times - dplyr::lag(times)) %>% #cNompute time_span between single gps points
#         tidyr::replace_na(list(time_span = 0)) %>%
#         dplyr::mutate(duration = cumsum(as.double(time_span))) %>% # total time on the route
#         dplyr::mutate(lat1 = lat, lon1 = lon) %>%
#         dplyr::mutate(lat2 = dplyr::lag(lat1), lon2 = dplyr::lag(lon1)) %>% ## have a segment from lat1, lon1, to lat2, lon2
#         dplyr::mutate(dist = purrr::pmap_dbl(., ## Calculate the distance between points
#                                              function(lon1, lat1, lon2, lat2, ...) {
#                                                      geosphere::distm(c(lon1, lat1), c(lon2, lat2), fun = geosphere::distHaversine)
#                                              })) %>%
#         tidyr::replace_na(list(dist = 0)) %>% ## if distance is NA, then replace it with 0
#         dplyr::mutate(dist_cumsum =  cumsum(dist)) %>%
#         #dplyr::mutate(pace = (as.double(time_span) / 60) / (dist / 1000))%>%
#         dplyr::mutate(pace = if_else(dist == 0, 0, (as.double(time_span) / 60) / (dist / 1000)))
#
# downsample_track <- track_tibble %>% ## only take out a point every 2 minutes (or so)
#         filter(duration %in% seq(0, max(duration), by = 120))
#
#
#
# library(ggmap)
# library(rayshader)
#
#
# trackmap_bb <- ggmap::make_bbox(lon = downsample_track$lon, lat = downsample_track$lat, f = .5)
#
# track_map <- ggmap::get_stamenmap(bbox = trackmap_bb,
#                                   maptype = "terrain-background",
#                                   zoom = 7)
#
#
# track_map_attributes <- attributes(track_map)
#
# track_map_trans <- matrix(adjustcolor(track_map,
#                                       alpha.f = 0),
#                           nrow = nrow(track_map))
#
# attributes(track_map_trans) <- track_map_attributes


#
# save.image("script_data_tymride.RData")
load("script_data_tymride.RData")
library(tidyverse)
library(magick)
library(ggmap)
library(rayshader)


glimpse(downsample_track)

plot_design <- list(geom_point(data = downsample_track,
                               aes(x = lon1,
                                   y = lat1,
                                   color = ele)), # change this for color.
                    scale_color_viridis_c(direction = -1, option = "C", # color scaling. this is viridis variant "c"
                                          guide = FALSE),
                    labs(x = "Longitude",
                         y = "Latitude"),
                    theme_minimal(),
                    theme(axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          panel.grid = element_blank(),
                          axis.title = element_blank())
)

together_plot <- ggmap(track_map_trans) +
        plot_design

point_plot <- ggmap(track_map) +
        plot_design


# plot_gg(list(point_plot, together_plot), # this line and the next could be swapped to have a base layer.
plot_gg(together_plot,
        multicore = TRUE, # tells the computer to render using more cores.
        width = 5, # 5 inches
        height = 5,
        scale = 150, # adjusts the height of your textures. base is 150 for some reason
        theta = 0, # this means you'll look at it straight-on
        phi = 60, # this adjusts the tilt (forwards and back)
        windowsize = c(1000,1000),
        reduce_size = c(1, 1), # if you make these smaller than 1 the quality goes down and it renders faster
        zoom = .6, # the smaller this is, the more zoomed-in it is.
        triangulate = TRUE, # when this is TRUE you have lower quality (faster)
        max_error = .0001, # when this is big you have higher quality (faster)
        sunangle = 70)  # change this to change the



Sys.sleep(.2)
render_snapshot("samplemap.png", clear = TRUE) # save the image

plot_obj <- magick::image_read("samplemap.png") # read in the image
plot_obj <- magick::image_modulate(plot_obj, brightness = 115) # brighten it up since it's pretty dark!
magick::image_write(plot_obj, path = "samplemap.png") # save the image
