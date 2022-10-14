pkgs = c('data.table', 'tidyverse', 'sf', 'RPostgreSQL','spatstat',
         'leaflet', 'ggmap')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

code.dir = paste0(getwd(), '/code/') # code directory
data.dir = paste0(getwd(), '/data/')
res.dir = paste0(data.dir, 'results/')
gfx.dir = paste0(getwd(), '/graphics/')
grant.dir = gsub('flaring/vnf', 'grants/Flaring/R01 - Oct 2019/', getwd())

source(paste0(code.dir, 'helper_functions.R'))
theme_set(theme_bw())

# by-shale density --------------------------------------------------------
# import shapefile
tx = paste0(data.dir, "shapes/texas.shp") %>%
  st_read(quiet = TRUE) %>% st_union() %>% st_sf()

system.time({
  dense.tx.window = paste0(data.dir, "shapes/texas.shp") %>%
    st_read(quiet = TRUE) %>% st_union() %>% 
    st_transform(crs = 26913) %>% st_coordinates() %>% 
    data.table() %>% 
    mutate(x = rev(X / 1e4), y = rev(Y / 1e4)) %>% 
    select(x, y) %>% data.table()
  # read clustered points
  # project to local projection
  # create ppp object and then apply kernel density
  flares.tx.ppp = paste0(data.dir, "vnf/us-vnf-basins-wells-clust-2012-2020.rds") %>%
    readRDS() %>% 
    filter(cluster == "Yes", stusps == "TX", year(date) %in% 2016:2017) %>%
    st_as_sf(coords = c("vnf_lon", "vnf_lat"), crs = 4326, remove = FALSE) %>% 
    st_transform(26913) %>% st_coordinates() %>% data.table() %>% 
    mutate(x = X / 1e4, y = Y / 1e4) %>% select(x, y) %>% 
    as.ppp(X = ., W = owin(poly = dense.tx.window))
  flares.tx.dense = flares.tx.ppp %>% 
    density(kernel = "gaussian",
            sigma = bw.scott(flares.tx.ppp)[2],
            diggle = TRUE,
            dimyx = c(117, 126))
  flares.tx.dense.dat = data.table(
    expand.grid(x = flares.tx.dense$xcol * 1e4,
                y = flares.tx.dense$yrow * 1e4),
    flares.tx.dense$v %>% t %>% melt %>%
      select(density = value)) %>% na.omit() %>%
    st_as_sf(coords = c("x", "y"), crs = 26913) %>%
    st_transform(4326) %>%
    cbind(., st_coordinates(.)) %>% st_drop_geometry() %>%
    select(lon = X, lat = Y, density) %>% 
    data.table()
})

trans.theme = theme(
  rect = element_rect(fill = "transparent"),
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank())

flares.tx.dense.dat %>% 
  filter(density >= 2) %>% 
  mutate(density_cut = cut(density, c(2, seq(50, 200, by = 50), 350))) %>% 
  ggplot() +
  geom_sf(data = tx, color = "goldenrod", fill = NA) +
  geom_point(aes(x = lon, y = lat, color = density_cut),
             shape = 15, size = 1.5) +
  scale_color_viridis_d() +
  labs(color = "Flares / 100 km2") +
  trans.theme +
  ggsave(paste0(gfx.dir, "tx-dense-overlay-2016-2017.png"),
         bg = "transparent", height = 5, width = 6, dpi = 600)