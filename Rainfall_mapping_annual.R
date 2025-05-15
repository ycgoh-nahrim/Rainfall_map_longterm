# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('J:/Backup_main/Main/20170206_SBAK/Analysis/hydrology/rainfall_annual')


#import data
RF_data = read.csv("RF_Klang_year_stn.csv", header = T, sep = ",")

#set format
str(RF_data)
#RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")


# CHECK OUTLIERS
## annual rainfall CHART

gg_rainfall <- RF_data %>% 
  ggplot(aes(x = Year, y = sum_Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name="Year", 
                     breaks = seq(1970, 2020, by = 5),
                     minor_breaks = NULL) +
  #scale_x_datetime(name= "Datetime", date_labels = "%b %d",
  #                 date_breaks = "1 day", #date_minor_breaks = "1 day",
  #                 minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     breaks = seq(0, 5000, by = 1000), limits = c(0,5000),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = "Roboto", color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Annual Rainfall")

gg_rainfall

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()

#print last plot to file
ggsave(paste0("Klang_rainfall_yr_chart.jpg"), dpi = 300,
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

#sel_shp <- subset(klang_shp, STATE %in% c("Selangor", "Kuala Lumpur"))

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
RF_data_sely <- RF_data %>% 
  filter(Year == "2019")

# join data
RF_sely <- RF_data_sely %>% 
  merge(RF_stn, by = "Stn_no")

str(RF_sely)

# convert df to spatial
sf_sely <- st_as_sf(RF_sely, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_sely)


# create raster template
ras_interp_template <- raster(klang_shp, res = 0.001)

# make sure same projection
crs(ras_interp_template)
crs(sf_sely)

#plot(ras_interp_template)


## Nearest Neighbour
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = sum_Depth ~ 1,    # The column  we are interested in
  data = as(sf_sely, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
sely_NN <- interpolate(ras_interp_template, fit_NN)
plot(sely_NN)
sely_NN_mask <- mask(sely_NN, mask = klang_shp)
plot(sely_NN_mask)


# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = sum_Depth ~ 1,
  locations = sf_sely,
  nmax = 10, nmin = 3,
  set = list(idp = 0.5) # inverse distance power
)
sely_IDW <- interpolate(ras_interp_template, fit_IDW)
plot(sely_IDW)
sely_IDW_mask <- mask(sely_IDW, mask = klang_shp)
plot(sely_IDW_mask)

class(sely_IDW_mask)

## overlay map

map_rf_sely <- ggplot() +
  geom_raster(data = as.data.frame(sely_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  #scale_fill_gradientn(name="Rainfall (mm)", 
  #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
  #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
  #                     limits = c(1000, 4000)) +
  scale_fill_distiller(name="Rainfall (mm)", 
                       palette = "Spectral", direction = 1,
                       #na.value = "purple",
                       oob = scales::squish, # squish out of bound values to nearest extreme
                       #breaks = seq(0, 5000, by = 500),
                       limits = c(1000, 4000)) + # set fixed legend) +
  geom_polygon(data = klang_shp, aes(x = long, y = lat, group = group),
               colour = "white", fill = NA) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey60", size = 0.1, fill = NA) +
  theme_void() + 
  theme(legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11)) +
  labs(title="Annual Rainfall Distribution",
       subtitle = "2019") +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "right", title.hjust = 0.9, 
                               barwidth = unit(1, "lines"), barheight = unit(10, "lines")))

map_rf_sely

#print last plot to file
ggsave(paste0("Klang_rainfall_y2019-3.jpg"), dpi = 300,
       width = 6, height = 5, units = "in")




# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(RF_sely[, c('Long', 'Lat')]), # accepts points but expects them as matrix
  Y = RF_sely$sum_Depth,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)
sely_TPS <- interpolate(ras_interp_template, fit_TPS)
plot(sely_TPS)
sely_TPS_mask <- mask(sely_TPS, mask = klang_shp)
plot(sely_TPS_mask)



# Automatized Kriging  

## reproject to GDM 2000 PM
sf_sely_gdm <- st_transform(sf_sely, crs = 3375)
crs(sf_sely_gdm)

fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = sum_Depth ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(sf_sely_gdm, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 
sely_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = 4326) #no changes to CRS??
plot(sely_KRIG)


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
un_date <- sort(unique(RF_data_stn$Year)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Year"


# PRODUCE INTERPOLATION RASTER AND MAP



# SEPARATE INTERPOLATION AND MAPPING

# produce interpolation raster only (without map)

interp_list = list() #for combination

for (i in 1:(nrow(df_date))) {
  #i=1
  sel_yr <- df_date[i,]
  
  #filter according to date
  RF_data_annual <- RF_data_stn %>% 
    filter(Year == sel_yr)
  
  # convert to sf
  sf_rf_annual <- st_as_sf(RF_data_annual, coords = c('Long', 'Lat'), crs = 4326)
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = sum_Depth ~ 1,
    locations = sf_rf_annual,
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  annual_IDW <- interpolate(ras_interp_template, fit_IDW)
  annual_IDW_mask <- mask(annual_IDW, mask = klang_shp)
  #plot(daily_IDW_mask)
  

  
  #counter <- counter + 1
  interp_list[[i]] <- annual_IDW_mask
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# produce maps from interpolation raster

maplist <- list()

j = 1

for (j in 10:length(interp_list)) {
  
  sel_yr <- df_date[j,]

  # plot map
  annual_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[j]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred),
                show.legend = FALSE) +
    #scale_fill_gradientn(name = "Rainfall (mm)", 
    #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
    #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
    #                     limits = c(0, 4000)) +
    scale_fill_distiller(name = "Rainfall (mm)", 
                         palette = "Spectral", direction = 1,
                         #na.value = "purple",
                         oob = scales::squish, # squish out of bound values to nearest extreme
                         #breaks = seq(0, 5000, by = 500),
                         limits = c(1000, 4000)) + # set fixed legend) +
    geom_polygon(data = klang_shp, aes(x = long, y = lat, group = group),
                 colour = "white", fill = NA) +
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
                 colour = "grey80", fill = NA) +
    theme_void() + 
    theme(plot.title=element_text(size = 11)) +
    labs(title = sel_yr) +
    coord_fixed() 
  
  
  #counter <- counter + 1
  maplist[[j]] <- annual_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


# facet mapping

library(gridExtra)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(map_rf_sely)


# arrange layout

facet_map <- grid.arrange(grobs = maplist[10:49], ncol = 10)

#### title font format
title = grid::textGrob('Klang Basin Annual Rainfall\n', 
                       gp = grid::gpar(fontsize = 16))

facet_legend_map <- grid.arrange(facet_map, mylegend, 
                                 top = title,
                                 ncol = 2, widths=c(9, 1))


# multiple maps with single particular map
#facet_all_map <- grid.arrange(facet_map, map_rf_sely, ncol = 2)


#print last plot to file
ggsave(paste0("Klang_rainfall_annual4.jpg"), facet_legend_map, dpi = 400,
       width = 17, height = 10, units = "in")


#########################

# CLASSIFIED MAP


## set palette 
#col_pal <- brewer.pal(n = 11, name = "Spectral")
col_pal <- brewer.pal(n = 9, name = "YlGnBu")
#col_brk <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)
col_brk <- c(1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 4000)
#col_brk <- c(1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 4000) #n=11
#col_brk <- seq(2100, 2900, by = 100) #n=8
up_limit <- max(col_brk)
low_limit <- min(col_brk)



## map

#########
### single map - single year

map_cl_sely <- ggplot() +
  geom_raster(data = as.data.frame(sely_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey60", size = 0.1, fill = NA) +
  #geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
  #             colour = "grey60", size = 0.1, fill = NA) +
  #geom_point(data = RF_y2020, aes(x = Long, y = Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  labs(title = paste0("Klang Basin Annual Rainfall Distribution"),
       subtitle = "2019") +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(20, "lines"), barheight = unit(0.5, "lines")))

map_cl_sely

#print last plot to file
ggsave(paste0("Klang_RF_2019_lgc1.jpg"), dpi = 300,
       width = 6, height = 4, units = "in")


#########
### single map - average all years


# subset 
RF_data_avgy <- RF_data %>% 
  group_by(Stn_no, Stn_name) %>% 
  summarise(Avg_depth = mean(sum_Depth))


# join data
RF_avgy <- RF_data_avgy %>% 
  merge(RF_stn, by = "Stn_no")

str(RF_avgy)

# convert df to spatial
sf_avgy <- st_as_sf(RF_avgy, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_avgy)


# make sure same projection
crs(ras_interp_template)
crs(sf_avgy)


## Nearest Neighbour
fit_avgy_NN <- gstat::gstat( # using package {gstat} 
  formula = Avg_depth ~ 1,    # The column  we are interested in
  data = as(sf_avgy, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
avgy_NN <- interpolate(ras_interp_template, fit_avgy_NN)
plot(avgy_NN)
avgy_NN_mask <- mask(avgy_NN, mask = klang_shp)
plot(avgy_NN_mask)



## Inverse Distance Weighting
fit_avgy_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = Avg_depth ~ 1,
  locations = sf_avgy,
  nmax = 10, nmin = 3,
  set = list(idp = 0.5) # inverse distance power
)
avgy_IDW <- interpolate(ras_interp_template, fit_avgy_IDW)
plot(avgy_IDW)
avgy_IDW_mask <- mask(avgy_IDW, mask = klang_shp)
plot(avgy_IDW_mask)

class(avgy_IDW_mask)


map_cl_avg <- ggplot() +
  geom_raster(data = as.data.frame(avgy_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
               colour = "grey60", size = 0.1, fill = NA) +
  #geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group),
  #             colour = "grey60", size = 0.1, fill = NA) +
  #geom_point(data = RF_y2020, aes(x = Long, y = Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  labs(title = paste0("Average Annual Rainfall (1971 - 2019)")) +
  coord_fixed() +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(20, "lines"), barheight = unit(0.5, "lines")))

map_cl_avg

#print last plot to file
ggsave(paste0("Klang_RF_avg_lgc3.jpg"), dpi = 300,
       width = 6, height = 4, units = "in")


#########

### produce multiple maps from interpolation raster

maplist_cl <- list()

m = 1

for (m in 10:length(interp_list)) {
  
  sel_yr <- df_date[m,]
  sel_pt <- RF_data_stn %>% 
    filter(Year == sel_yr)
  
  # plot map
  annual_IDW_map_cl <- ggplot() +
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
                 colour = "grey60", size = 0.1, fill = NA) +
    #geom_point(data = sel_pt, aes(x = Long, y = Lat),
    #           color = 'red', size = 0.5, alpha = 0.5) +
    theme_void() + 
    theme(legend.position = "none",
          plot.title=element_text(size = 11)) +
    labs(title = sel_yr) +
    coord_fixed()
  
  
  #counter <- counter + 1
  maplist_cl[[m]] <- annual_IDW_map_cl
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}



# FACET MAP

mylegend_cl <- g_legend(map_cl_avg)


# arrange layout

facet_map_cl <- grid.arrange(grobs = maplist_cl[10:49], ncol = 10)

#### title font format
title2 = grid::textGrob(paste0('Klang Basin Annual Rainfall\n'), 
                        gp = grid::gpar(fontsize = 14))


# layout - all annual

facet_legend_map_cl <- grid.arrange(facet_map_cl, mylegend_cl, 
                                    top = title2, 
                                    nrow = 2, heights = c(9, 1)
                                    #ncol = 2, widths = c(9, 1)
)

#### print last plot to file
ggsave(paste0("Klang_RF_annual_lgc3.jpg"), facet_legend_map_cl, dpi = 400,
       width = 17, height = 10, units = "in")



# combined layout - annual & avg
facet_legend_map_cl2 <- grid.arrange(facet_map_cl, map_cl_avg, #mylegend_cl, 
                                    top = title2, 
                                    #nrow = 2, heights = c(9, 1)
                                    ncol = 2, widths = c(7, 3)
)

#### print last plot to file
ggsave(paste0("Klang_RF_annual_avg_lgc3.jpg"), facet_legend_map_cl2, dpi = 400,
       width = 18, height = 10, units = "in")



#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)




