# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/Main/20170206_SBAK/Analysis/hydrology/rainfall_mth')


#import data
RF_data = read.csv("LTMthly.csv", header = T, sep = ",")

#set format
str(RF_data)
#RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")


# CHECK OUTLIERS
## monthly rainfall CHART

gg_rainfall <- RF_data %>% 
  ggplot(aes(x = Month, y = mth_Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name="Month", 
                     breaks = seq(1, 12, by = 1),
                     minor_breaks = NULL) +
  #scale_x_datetime(name= "Datetime", date_labels = "%b %d",
  #                 date_breaks = "1 day", #date_minor_breaks = "1 day",
  #                 minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     #breaks = seq(0, 5000, by = 1000), limits = c(0,5000),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Monthly Rainfall")

gg_rainfall

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()

#print last plot to file
ggsave(paste0("Klang_RF_mth_chart.jpg"), dpi = 300,
       width = 8, height = 5, units = "in")


#########################
# MAPPING

library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
library(rgdal) # read shapefile
library(mapproj)
library(tmap) # animation

library(gridExtra)

library(viridis)
library(RColorBrewer)
library(scales)

# And a lot of different packages to test their interpolation functions
library(gstat)  # inverse distance weighted, Kriging
library(fields) # Thin Plate Spline
library(automap)# Automatic approach to Kriging


# import coordinate data

RF_stn = read.csv("RF_Klang_annual_lt.csv", header = T, sep = ",")

str(RF_stn)


# map country and coordinate data

## shapefile
klang_shp <- readOGR("J:/Backup_main/Main/20170206_SBAK/GIS/shp/jsb_basin_Klang.shp",
                  stringsAsFactors = F)

sel_shp <- readOGR("J:/Backup_main/Main/20170206_SBAK/GIS/shp_DBKL/Boundary_WPKL.shp",
                  stringsAsFactors = F)


crs(sel_shp)

crs(klang_shp)

### change projection to WGS84 (original Kertau)
#klang_shp2 <-spTransform(klang_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
sel_shp2 <-spTransform(sel_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

crs(sel_shp2)


# layout map

map <- ggplot() + 
  geom_polygon(data = klang_shp, aes(x = long, y = lat, group = group), 
               fill = "grey", alpha = 0.3, colour = "white") +
  geom_point(data = RF_stn, aes(x = Long, y = Lat),
              color = 'red', size = 1, alpha = 0.5) +
  coord_map(xlim=c(101.1, 102), ylim=c(2.8, 3.4)) +
  theme_void()

map


# INTERPOLATE (one day first)

# subset 
RF_data_m12 <- RF_data %>% 
  filter(Month == "12")

# join data
RF_m12 <- RF_data_m12 %>% 
  merge(RF_stn, by = "Stn_no")

str(RF_m12)

# convert df to spatial
sf_m12 <- st_as_sf(RF_m12, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_m12)


# create raster template
ras_interp_template <- raster(klang_shp, res = 0.001)

# make sure same projection
crs(ras_interp_template)
crs(sf_m12)

#plot(ras_interp_template)


## Nearest Neighbour
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = mth_Depth ~ 1,    # The column  we are interested in
  data = as(sf_m12, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
m12_NN <- interpolate(ras_interp_template, fit_NN)
plot(m12_NN)
m12_NN_mask <- mask(m12_NN, mask = klang_shp)
plot(m12_NN_mask)


# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = mth_Depth ~ 1,
  locations = sf_m12,
  nmax = 10, nmin = 3,
  set = list(idp = 0.5) # inverse distance power
)
m12_IDW <- interpolate(ras_interp_template, fit_IDW)
plot(m12_IDW)
m12_IDW_mask <- mask(m12_IDW, mask = klang_shp)
plot(m12_IDW_mask)

class(m12_IDW_mask)


## overlay map

map_rf_m12 <- ggplot() +
  geom_raster(data = as.data.frame(m12_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  #scale_fill_gradientn(name="Rainfall (mm)", 
  #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
  #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
  #                     limits = c(0, 4000)) +
  scale_fill_distiller(name="Rainfall (mm)", 
                       palette = "Spectral", direction = 1,
                       #na.value = "purple",
                       oob = scales::squish, # squish out of bound values to nearest extreme
                       breaks = seq(0, 350, by = 50),
                       limits = c(0, 350)) + # set fixed legend) +
  geom_polygon(data = klang_shp, aes(x = long, y = lat, group = group),
               colour = "white", fill = NA) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey50", fill = NA) +
  theme_void() + 
  theme(legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11)) +
  labs(title="Long-Term Monthly Rainfall Distribution",
       subtitle = "December") +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "right", title.hjust = 0.9, 
                               barwidth = unit(1, "lines"), barheight = unit(10, "lines")))

map_rf_m12

#print last plot to file
ggsave(paste0("Klang_rainfall_m12.jpg"), dpi = 300,
       width = 6, height = 5, units = "in")



# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(RF_y2019[, c('Long', 'Lat')]), # accepts points but expects them as matrix
  Y = RF_y2019$sum_Depth,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)
y2019_TPS <- interpolate(ras_interp_template, fit_TPS)
plot(y2019_TPS)
y2019_TPS_mask <- mask(y2019_TPS, mask = klang_shp)
plot(y2019_TPS_mask)



# Automatized Kriging  

## reproject to GDM 2000 PM
sf_y2019_gdm <- st_transform(sf_y2019, crs = 3375)
crs(sf_y2019_gdm)

fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = sum_Depth ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(sf_y2019_gdm, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 
y2019_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = 4326) #no changes to CRS??
plot(y2019_KRIG)


#############################
# INTERPOLATE FOR ALL DAYS

##  Use IDW

# daily rainfall data join stn data

RF_data_stn <- RF_data %>% 
  merge(RF_stn, by = "Stn_no")

str(RF_data_stn)

# convert df to spatial
sf_rf_all <- st_as_sf(RF_data_stn, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_rf_all)

# extract date for iteration
un_date <- sort(unique(RF_data_stn$Month)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Month"


# PRODUCE INTERPOLATION RASTER AND MAP



# SEPARATE INTERPOLATION AND MAPPING

# produce interpolation raster only (without map)

interp_list = list() #for combination

for (i in 1:(nrow(df_date))) {
  #i=1
  sel_mth <- df_date[i,]
  
  #filter according to date
  RF_data_mth <- RF_data_stn %>% 
    filter(Month == sel_mth)
  
  # convert to sf
  sf_rf_mth <- st_as_sf(RF_data_mth, coords = c('Long', 'Lat'), crs = 4326)
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = mth_Depth ~ 1,
    locations = sf_rf_mth,
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  mth_IDW <- interpolate(ras_interp_template, fit_IDW)
  mth_IDW_mask <- mask(mth_IDW, mask = klang_shp)
  #plot(daily_IDW_mask)
  

  
  #counter <- counter + 1
  interp_list[[i]] <- mth_IDW_mask
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# produce maps from interpolation raster

maplist <- list()

#j = 1

for (j in 1:length(interp_list)) {
  
  sel_mth <- df_date[j,]

  # plot map
  mth_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[j]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred),
                show.legend = FALSE) +
    #scale_fill_gradientn(name = "Rainfall (mm)", 
    #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
    #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
    #                     limits = c(0, 4000)) +
    scale_fill_distiller(name="Rainfall (mm)", 
                         palette = "Spectral", direction = 1,
                         #na.value = "purple",
                         oob = scales::squish, # squish out of bound values to nearest extreme
                         breaks = seq(0, 350, by = 50),
                         limits = c(0, 350)) + # set fixed legend) +
    geom_polygon(data = klang_shp, aes(x = long, y = lat, group = group),
                 colour = "white", fill = NA) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "grey50", fill = NA) +
    theme_void() + 
    theme(plot.title=element_text(size = 14)) +
    labs(title = month.name[sel_mth]) +
    coord_fixed()
  
  
  #counter <- counter + 1
  maplist[[j]] <- mth_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# facet mapping

library(gridExtra)


# extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(map_rf_m12)


### arrange layout

facet_map <- grid.arrange(grobs = maplist, ncol = 4)

#### title font format
title = grid::textGrob('Klang Basin Long-Term Monthly Rainfall\n', 
                       gp = grid::gpar(fontsize = 16))

facet_legend_map <- grid.arrange(facet_map, mylegend, 
                                 top = title,
                                 ncol = 2, widths = c(9, 1))


# multiple maps with single particular map
#facet_all_map <- grid.arrange(facet_map, map_rf_m12, ncol = 2)


### print last plot to file
ggsave(paste0("Klang_rainfall_ltmth2.jpg"), facet_legend_map, dpi = 400,
       width = 17, height = 10, units = "in")


#########################

# CLASSIFIED MAP


## set palette 
#col_pal <- brewer.pal(n = 8, name = "Spectral")
col_pal <- brewer.pal(n = 9, name = "YlGnBu")
col_brk <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450)

up_limit <- max(col_brk)
low_limit <- min(col_brk)


## map

### single map

map_cl_m12 <- ggplot() +
  geom_raster(data = as.data.frame(m12_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  #geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group),
  #             colour = "white", fill = NA) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey60", size = 0.1, fill = NA) +
  #geom_point(data = RF_y2020, aes(x = Long, y = Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  labs(title = paste0("Klang Basin Long-Term Average Monthly Rainfall"),
       subtitle = "December") +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(20, "lines"), barheight = unit(0.5, "lines")))

map_cl_m12

#print last plot to file
ggsave(paste0("Klang_RF_m12_lgc2.jpg"), dpi = 300,
       width = 6, height = 4, units = "in")


### produce multiple maps from interpolation raster

maplist_cl <- list()

m = 1

for (m in 1:length(interp_list)) {
  
  sel_mth <- df_date[m,]
  sel_pt <- RF_data_stn %>% 
    filter(Month == sel_mth)
  
  # plot map
  mth_IDW_map_cl <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[m]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred)) +
    scale_fill_stepsn(name = "Rainfall (mm)",
                      #n.breaks = 3, 
                      colours = col_pal,
                      breaks = col_brk,
                      values = rescale(col_brk),
                      limits = c(low_limit, up_limit)) +
    #geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group),
    #             colour = "white", fill = NA) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "grey20", size = 0.1, fill = NA) +
    #geom_point(data = sel_pt, aes(x = Long, y = Lat),
    #           color = 'red', size = 0.5, alpha = 0.5) +
    theme_void() + 
    theme(legend.position = "none",
          plot.title=element_text(size = 11)) +
    labs(title = month.name[sel_mth]) +
    coord_fixed()
  
  
  #counter <- counter + 1
  maplist_cl[[m]] <- mth_IDW_map_cl
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}



### facet map

mylegend_cl <- g_legend(map_cl_m12)


# arrange layout

facet_map_cl <- grid.arrange(grobs = maplist_cl[1:12], ncol = 4)

#### title font format
title2 = grid::textGrob(paste0('Klang Basin Long-Term Average Monthly Rainfall\n'), 
                        gp = grid::gpar(fontsize = 14))

facet_legend_map_cl <- grid.arrange(facet_map_cl, mylegend_cl, 
                                    top = title2, 
                                    nrow = 2, heights = c(9, 1)
                                    #ncol = 2, widths = c(9, 1)
)




#print last plot to file
ggsave(paste0("Klang_RF_ltmth_lgc2.jpg"), facet_legend_map_cl, dpi = 400,
       width = 17, height = 10, units = "in")




#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)




