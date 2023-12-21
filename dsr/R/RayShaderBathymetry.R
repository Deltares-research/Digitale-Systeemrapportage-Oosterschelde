
rm(list = ls())

require(tidyverse)
require(scales)
require(raster)
require(sf)
source("runThisFirst.R")
require(rayshader)
require(rayrender)


#=== AHN background ====

# 5 m resolution grids 
ahn5RawDir <- file.path(datadir, "AHN5", "raw")
ahnList <- list.files(ahn5RawDir)
rl <- lapply(ahnList, function(x) raster::raster(file.path(ahn5RawDir, x)))

bgRaster <- do.call(merge, c(rl, tolerance = 1))

#=======================

# codes <- c("STAVNSE_OW_WATHTE_NVT","ROOMPBNN_OW_WATHTE_NVT","MARLGT_OW_WATHTE_NVT","KRAMMSZWT_OW_WATHTE_NVT")

# hoogte <- lapply(codes, function(x) read_delim(file = file.path(datadir, "ddl/standard", paste0(x, ".csv")), delim=" ") %>%
#                    mutate(tijdstip = as_datetime(tijdstip)) %>%
#                    mutate(jaar = year(tijdstip)) %>%
#                    filter(numeriekewaarde<1e7 & grootheid.code == "WATHTE") %>%
#                    filter(jaar > 2013 & jaar < 2019)) %>%
#   bind_rows()
# 
# stations = hoogte %>% distinct(locatie.code, geometriepunt.x, geometriepunt.y) %>%
#   st_as_sf(coords = c("geometriepunt.x", "geometriepunt.y"), crs = 25831) %>% 
#   sf::st_transform(4326)

vakken <- sf::read_sf(file.path(datadir, "kaarten", "wl2018_OSv4.geojson")) %>% 
  sf::st_transform(28992) %>% 
  mutate(DEELGEBIED = case_when(DEELGEBIED == "west" ~ "monding",
                                DEELGEBIED == "midden" ~ "midden",
                                DEELGEBIED == "oost" ~ "kom",
                                DEELGEBIED == "noord" ~ "noord"))

# factpal <- colorFactor(topo.colors(4), vakken$DEELGEBIED)


# bathy <- raster::raster(file.path(datadir, "RWS_bathymetrie/Vakloding/selection/OSbathymetrie2013_correct.tif"))

bathy2016 <- raster(file.path(datadir, "RWS_ecotopen", "standard", "Basisbestanden Ecotopenkaart OS 2016", "Diepte", "ecodiep_os16", "hdr.adf"))
proj4rd <- CRS('+init=EPSG:28992')
proj4wgs <- CRS('+init=EPSG:4326')
raster::crs(bathy2016) <- proj4rd

# proj4rd <- CRS('+init=EPSG:28992')
# proj4wgs <- CRS('+init=EPSG:4326')
# raster::crs(bathy) <- proj4rd
# bathyWGS <- projectRaster(bathy, crs = proj4wgs)

e = extent(36180, 53000, 400000, 414460 )
deelraster <- crop(raster::mask(bathy2016, vakken[vakken$DEELGEBIED== "monding",]), e)/100

omgeving = "AHN" # vlak of AHN

if(omgeving == "vlak"){
  deelraster[is.na(deelraster[])] <- -1 
  all.raster <- deelraster
}

# AHN downloaden, zie https://geoforum.nl/t/r-package-gemaakt-om-ahn-hoogte-punten-gebieden-of-point-clouds-op-te-halen-via-r/4201
if(omgeving == "AHN"){
  # AHN eromheen zetten
  deelraster[is.na(deelraster[])] <- -10 
  raster::crs(bgRaster)
  deelAHN <- crop(bgRaster, e) + 1.5
  new.deelAHN <- projectRaster(deelAHN, deelraster) # define the projection and extent
  all.raster <- raster::mosaic(deelraster, new.deelAHN, fun=max)
  # deelraster[is.na(deelraster[])] <- deelAHN
}

# plot(all.raster)

bthy = raster_to_matrix(projectRaster(all.raster, crs = CRS("+init=epsg:4326")))

rm(all.raster, bgRaster, bathy2016, deelAHN, deelraster)

# bthy %>%
#   sphere_shade(zscale = 1, texture = "imhof1") %>%
#   add_shadow(ambient_shade(bthy, maxsearch = 100, multicore = TRUE, zscale=1),0) %>%
#   add_shadow(lamb_shade(bthy), 0.5) %>%
#   # add_shadow(ambient_shade(bthy, zscale = 5)) %>%
#   plot_3d(bthy, zscale = 1, fov = 10, theta = -100, zoom = .3, phi = 15, 
#           windowsize = c(1920, 1080), water = TRUE, waterdepth = -1, 
#           wateralpha = 0.5, watercolor = "#233aa1",
#           waterlinecolor = "white", waterlinealpha = 0.5)
# render_label(bthy, x = 730, y = 550, z = 10, zscale = 1, relativez = F,
#              text = "Zeelandbrug", textsize = 2, linewidth = 5)
# render_label(bthy, x = 250, y = 400, z = 100, zscale = 1, relativez = F,
#              text = "Neeltje Jans", textsize = 2, linewidth = 5)
# render_label(bthy, x = 400, y = 250, z = 50, zscale = 1, relativez = F,
#              text = "Roggenplaat", textsize = 2, linewidth = 5)
# Sys.sleep(0.5)
# # render_snapshot(clear = TRUE)
# rayrender::render_scene()
# render_depth(focus = 0.4, focallength = 20, clear = TRUE)

bthy %>%
  sphere_shade(texture = "imhof1") %>%
  plot_3d(bthy, zscale = 1, fov = 10, theta = -100, zoom = .3, phi = 15, 
          windowsize = c(960, 540), water = TRUE, waterdepth = -1, 
          wateralpha = 0.5, watercolor = "#233aa1",
          waterlinecolor = "white", waterlinealpha = 0.5)
render_label(bthy, x = 730, y = 550, z = 10, zscale = 1, relativez = F,
             text = "Zeelandbrug", textsize = 2, linewidth = 5)
render_label(bthy, x = 250, y = 400, z = 100, zscale = 1, relativez = F,
             text = "Neeltje Jans", textsize = 2, linewidth = 5)
render_label(bthy, x = 400, y = 250, z = 50, zscale = 1, relativez = F,
             text = "Roggenplaat", textsize = 2, linewidth = 5)
# render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50,
#                 scale_length = c(0.33,1))
# render_compass(position = "E")
Sys.sleep(0.5)
# render_highquality(samples=200, scale_text_size = 24,clear=TRUE)
render_highquality(
  filename = file.path(datadir, "RWS_bathymetrie", "products", "Bath2016MondingRayshaderHR.png"),
  lightdirection = c(-35, 55),
  lightaltitude  = 40,
  clamp_value = 10,
  samples = 400,
  camera_lookat= NULL,
  ground_material = diffuse(color="grey50",checkercolor = "grey20", checkerperiod = 100)
)


# optical flow om tussen rasters te interpoleren
# https://rdrr.io/cran/SpatialVx/man/optflow.html



