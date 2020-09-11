library(ggplot2)
library(ggmap)
library(sf)


world_map <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")$geometry

# tym_route <- st_read("https://opendata.arcgis.com/datasets/fab4feab211c4899b602ecfbfbc420a3_3.kml")

tym_route <- sf::st_as_sf(sp::SpatialPointsDataFrame(coords = downsample_track[c("lon","lat")] %>%
                                    as.data.frame(), data = downsample_track,
                                   proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))

# tym_route <- ggmap::get_stamenmap(sp::bbox(route_points),
#                                 zoom = 7)

# Convert to 3857
tym_route_3857 <- st_transform(tym_route, 3857)
world_map <- st_transform(world_map, 3857)

basemap <- ggmap::get_map(location = unname(st_bbox(tym_route)), source = "stamen", crop = FALSE)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
 if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
 # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
 # and set the names to what sf::st_bbox expects:
 map_bbox <- setNames(unlist(attr(map, "bb")),
                      c("ymin", "xmin", "ymax", "xmax"))

 # Coonvert the bbox to an sf polygon, transform it to 3857,
 # and convert back to a bbox (convoluted, but it works)
 bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

 # Overwrite the bbox of the ggmap object with the transformed coordinates
 attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
 attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
 attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
 attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
 map
}

# Use the function:
basemap_transformed <- ggmap_bbox(basemap)

ggmap(basemap_transformed) +
 coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
 geom_sf(data = world_map, inherit.aes = FALSE, fill = NA, colour = "black", size = 1, linetype = 2) +
 geom_sf(data = tym_route_3857, inherit.aes = FALSE, aes(colour = ele)) +
 lims(x = c(6000, 1000000))

