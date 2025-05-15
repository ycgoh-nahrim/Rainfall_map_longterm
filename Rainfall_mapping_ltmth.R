# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(data.table)


# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('D:/gyc/2025/20250312_Nota_Jemaah_Menteri/Analysis/Rainfall/Monthly')


#import data
RF_data <- fread(file="D:/gyc/2025/20211221_Banjir_2021/Analysis/Aquarius1h_202404_KLSel/Aq_RF_1h_KLSel_yr_full.csv",
                 header = TRUE, sep=",", stringsAsFactors = F)

#set format
str(RF_data)
RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")



# data information

reg_name_full <- "Selangor - Kuala Lumpur - Putrajaya"
reg_name_short <- "KLSel"


###########################
# DATA AGGREGATION
#add a date & year column to data table
setDT(RF_data)[, Date:= date(Datetime) ]
setDT(RF_data)[, Month:= month(Datetime) ]
setDT(RF_data)[, Year:= year(Datetime) ]

str(RF_data)


## DAILY

RF_day <- RF_data[,.(Depth_day = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))), 
                  by = c("Stn_no", "Date")]

min(RF_day$cnt)

setDT(RF_day)[, Month:= month(Date) ]
setDT(RF_day)[, Year:= year(Date) ]


## MONTHLY

### from hourly data
RF_mth <- RF_data[,.(Depth_mth = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))), 
                  by = c("Stn_no", "Year", "Month")]

min(RF_mth$cnt) # 24*28=672


### from daily data
RF_mth <- RF_day[,.(Depth_mth = sum(Depth_day, na.rm = T), cnt = sum(!is.na(Depth_day))), 
                 by = c("Stn_no", "Year", "Month")]

min(RF_mth$cnt) # 28

RF_mth_full <- RF_mth[cnt > 27,] 

min(RF_mth_full$cnt)



## YEARLY

### from hourly data
RF_yr <- RF_data[,.(Depth_yr = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))), 
                 by = c("Stn_no", "Year")]

min(RF_yr$cnt) #24*365=8760, 8760*0.95=8322


### from daily data
RF_yr <- RF_day[,.(Depth_yr = sum(Depth_day, na.rm = T), cnt = sum(!is.na(Depth_day))), 
                by = c("Stn_no", "Year")]


min(RF_yr$cnt) # 365*0.95=346

RF_yr_full <- RF_yr[cnt > 346,] 


#############
# AVERAGED

## MONTHLY
RF_mth_avg <- RF_mth_full[,.(Depth_mth_avg = mean(Depth_mth, na.rm = T)), 
                          by = c("Stn_no", "Month")]

yr_max <- max(RF_mth_full$Year)
yr_min <- min(RF_mth_full$Year)


#############
## station list
stn_list <- as.data.frame(unique(RF_mth_avg$Stn_no))




# CHECK OUTLIERS


gg_rainfall <- RF_mth_avg %>% 
  ggplot(aes(x = Month, y = Depth_mth_avg)) +
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
  labs(title = paste0("Monthly Rainfall (", reg_name_full, ")"))

gg_rainfall

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


#print last plot to file
ggsave(paste0(reg_name_short, "_rainfall_mth_scatter.jpg"), dpi = 300,
       width = 8, height = 5, units = "in")


#########################
# MAPPING

library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#library(rgdal) # read shapefile
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

RF_stn = read.csv("D:/gyc/2025/20250312_Nota_Jemaah_Menteri/GIS/shp/TIDEDA_RF_stn_KLSel2.csv", header = T, sep = ",")

str(RF_stn)


# map country and coordinate data

## shapefile
#klang_shp <- readOGR("D:/gyc/Main/20170206_SBAK/GIS/shp/jsb_basin_Klang.shp",
#                  stringsAsFactors = F)


#sel_shp <- readOGR("D:/gyc/2025/20211221_Banjir_2021/GIS/shp/BoundaryKLSel.shp",
#                   stringsAsFactors = F)


pm_shp <- st_read("D:/GIS_data/Boundary/state/state_pmsia_short.shp")


sel_shp <- subset(pm_shp, STATE %in% c("Selangor", "Kuala Lumpur"))


## basin

all_basin_shp <- st_read("D:/GIS_data/DATA_SG_BASIN/Jica_subbasin/jsb_semm_merge4.shp")


basin_shp <- subset(all_basin_shp, BASINNAME %in% c("Selangor", "Klang", "Langat", 
                                                    "Tengi", "Buloh", "Sepang", "Bernam"))


# check projection

crs(sel_shp)

crs(basin_shp)


### change projection to WGS84 (original Kertau)
#klang_shp2 <-spTransform(klang_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

pm_shp2 <-st_transform(pm_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
sel_shp2 <-st_transform(sel_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
basin_shp2 <-st_transform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


crs(pm_shp2)

crs(sel_shp2)

crs(basin_shp2)


# FIND INTERSECTING BASIN

## make sure same crs
st_crs(sel_shp2) == st_crs(basin_shp2)


# invalid geometry
## check for invalid geometry
sf::st_is_valid(basin_shp2)
## option 1: repair invalid geometry
basin_shp3 <- st_make_valid(basin_shp2)
## option 2: buffer to repair geometry
basin_shp3 <- st_buffer(basin_shp2, 0)
## recheck geometry
sf::st_is_valid(basin_shp3)



## apply intersection
basin_sel_shp <- st_intersection(st_as_sf(pm_shp2), st_as_sf(basin_shp3))


## calculate area
basin_sel_shp$area <- st_area(basin_sel_shp)


## filter polygon fragments
### by area
#basin_sel_shp2 <- basin_sel_shp %>% 
#  filter(area > units::set_units(150000000, m^2))
### by state
basin_sel_shp2 <- basin_sel_shp %>% 
  filter(STATE %in% c("Selangor", "Kuala Lumpur"))

## convert to SpatialPolygonsDataFrame 
basin_sel_shp2 <- as_Spatial(basin_sel_shp2)

## dissolve polygons back to basins (merge to basins)
basin_sel_shp2 <- aggregate(basin_sel_shp2, by = "BASINNAME")

class(basin_sel_shp2)

## convert SpatialPolygonsDataFrame to sf to plot with geom_sf
basin_sel_shp3 <- st_as_sf(basin_sel_shp2)

class(basin_sel_shp3)



# LAYOUT MAP

map <- ggplot() + 
  geom_sf(data = pm_shp2, fill = "grey", alpha = 0.3, colour = "white") +
  geom_sf(data = sel_shp2, fill = "orange", alpha = 0.3, colour = "white") +
  geom_sf(data = basin_sel_shp3, fill = NA, alpha = 0.3, colour = "steelblue3") +
  geom_point(data = RF_stn, aes(x = Long, y = Lat),
             color = 'black', size = 1, alpha = 0.5) +
  #geom_point(data = banjir, aes(x = Long, y = Lat),
  #           color = 'red', size = 1, alpha = 0.5) +
  coord_sf(xlim=c(100.5, 102), ylim=c(2.5, 3.9)) +
  theme_void()


map


# INTERPOLATE (one month first)

# choose month
mth_name <- "November"
mth_no <- "11"


# subset 
RF_data_m12 <- RF_mth_avg %>% 
  filter(Month == mth_no)



# join data
RF_m12 <- RF_data_m12 %>% 
  merge(RF_stn, by = "Stn_no")

str(RF_m12)

# convert df to spatial
sf_m12 <- st_as_sf(RF_m12, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_m12)


# create raster template
ras_interp_template <- raster(sel_shp2, res = 0.001)

# make sure same projection
crs(ras_interp_template)
crs(sf_m12)

#plot(ras_interp_template)


## Nearest Neighbour
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = Depth_mth_avg ~ 1,    # The column  we are interested in
  data = as(sf_m12, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
m12_NN <- interpolate(ras_interp_template, fit_NN)
plot(m12_NN)
m12_NN_mask <- mask(m12_NN, mask = sel_shp2)
plot(m12_NN_mask)


# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = Depth_mth_avg ~ 1,   #param to be interpolated
  locations = sf_m12,            #input data
  nmax = 10, nmin = 3,
  set = list(idp = 2) # inverse distance power, adjust as needed (default: 0.5, rainfall usually 2)
)
m12_IDW <- interpolate(ras_interp_template, fit_IDW)
plot(m12_IDW)
m12_IDW_mask <- mask(m12_IDW, mask = sel_shp2)
plot(m12_IDW_mask)

class(m12_IDW_mask)


## overlay map

map_rf_m12 <- ggplot() +
  geom_raster(data = as.data.frame(m12_IDW_mask, xy = TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  #scale_fill_gradientn(name="Rainfall (mm)", 
  #                     colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998", "purple4"),
  #                     values = rescale(c(0, 1000, 2000, 3000, 4000)),
  #                     limits = c(0, 4000)) +
  scale_fill_distiller(name="Rainfall (mm)", 
                       palette = "Spectral", direction = 1,
                       #na.value = "purple",
                       oob = scales::squish, # squish out of bound values to nearest extreme
                       breaks = seq(0, 400, by = 50),
                       limits = c(0, 400)) + # set fixed legend) +
  geom_sf(data = basin_sel_shp3, fill = NA, alpha = 0.3, colour = "tan", linewidth = 1) +
  geom_sf(data = sel_shp2, fill = NA, alpha = 1, colour = "grey60", size = 0.1) +
  theme_void() + 
  theme(legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11)) +
  labs(title="Long-Term Monthly Rainfall Distribution",
       subtitle = mth_name) +
  coord_sf(xlim=c(100.5, 102), ylim=c(2.5, 3.9)) +
  guides(fill = guide_colorbar(label.position = "right", title.hjust = 0.9, 
                               barwidth = unit(1, "lines"), barheight = unit(10, "lines")))

map_rf_m12


# check max min values
m12_IDW_mask@data@max
m12_IDW_mask@data@min


#print last plot to file
ggsave(paste0(reg_name_short, "_RF_mth", mth_no, ".jpg"), dpi = 300,
       width = 6, height = 5, units = "in")



# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(RF_y2019[, c('Long', 'Lat')]), # accepts points but expects them as matrix
  Y = RF_y2019$sum_Depth,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)
y2019_TPS <- interpolate(ras_interp_template, fit_TPS)
plot(y2019_TPS)
y2019_TPS_mask <- mask(y2019_TPS, mask = sel_shp2)
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
# INTERPOLATE FOR ALL MONTHS

##  Use IDW

# monthly rainfall data join stn data

RF_data_stn <- RF_mth_avg %>% 
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

i = 1

interp_list = list() #for combination
#interp_list_nn = list() #for combination

for (i in 1:(nrow(df_date))) {
  #i=1
  sel_mth <- df_date[i,]
  
  #filter according to date
  RF_data_mth <- RF_data_stn %>% 
    filter(Month == sel_mth)
  
  # convert to sf
  sf_rf_mth <- st_as_sf(RF_data_mth, coords = c('Long', 'Lat'), crs = 4326)

  
  ## Nearest Neighbour
  #fit_NN <- gstat::gstat( # using package {gstat} 
  #  formula = Depth_mth_avg ~ 1,    # The column  we are interested in
  #  data = as(sf_rf_mth, "Spatial"), # using {sf} and converting to {sp}, which is expected
  #  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
  #)
  #mth_NN <- interpolate(ras_interp_template, fit_NN)
  #mth_NN_mask <- mask(mth_NN, mask = sel_shp2)
  
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = Depth_mth_avg ~ 1,
    locations = sf_rf_mth,
    nmax = 10, nmin = 3,
    set = list(idp = 2) # inverse distance power
  )
  mth_IDW <- interpolate(ras_interp_template, fit_IDW)
  mth_IDW_mask <- mask(mth_IDW, mask = sel_shp2)
  #plot(daily_IDW_mask)
  

  
  #counter <- counter + 1
  interp_list[[i]] <- mth_IDW_mask
  #interp_list_nn[[i]] <- mth_NN_mask
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}





#########################

# CLASSIFIED MAP


# check max and min values
max(RF_mth_avg$Depth_mth_avg)
min(RF_mth_avg$Depth_mth_avg)


## set palette 
#col_pal <- brewer.pal(n = 11, name = "Spectral")
col_pal <- brewer.pal(n = 6, name = "YlGnBu")
#col_brk <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, 550)
col_brk <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 550)
col_brk <- c(0, 100, 200, 300, 400, 500, 550)

up_limit <- max(col_brk)
low_limit <- min(col_brk)



## map

#########
### single map - single month

map_cl_sely <- ggplot() +
  geom_raster(data = as.data.frame(m12_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  #geom_point(data = RF_y2020, aes(x = Long, y = Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  geom_sf(data = basin_sel_shp3, fill = NA, alpha = 0.3, colour = "tan", linewidth = 0.1) +
  geom_sf(data = sel_shp2, fill = NA, alpha = 1, colour = "red3", linewidth = 0.3) +  theme_void() + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  labs(title = paste0(reg_name_full, " Long-Term Average Monthly Rainfall"),
       subtitle = mth_name) +
  coord_sf(xlim=c(100.5, 102), ylim=c(2.5, 3.9)) +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(20, "lines"), barheight = unit(0.5, "lines")))

map_cl_sely

#print last plot to file
ggsave(paste0(reg_name_short, "_RF_mth", mth_no, "_lgc.jpg"), dpi = 300,
       width = 6, height = 4, units = "in")




#########

# produce maps from interpolation raster

maplist <- list()

m = 1

for (m in 1:length(interp_list)) {
  
  
  sel_mth <- df_date[m,]
  sel_pt <- RF_data_stn %>% 
    filter(Month == sel_mth)
  
  # plot map
  mth_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[m]], xy = TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred)) +
    scale_fill_stepsn(name = "Rainfall (mm)",
                      #n.breaks = 3, 
                      colours = col_pal,
                      breaks = col_brk,
                      values = rescale(col_brk),
                      limits = c(low_limit, up_limit)) +
    geom_sf(data = basin_sel_shp3, fill = NA, alpha = 0.3, colour = "tan", linewidth = 0.2) +
    geom_sf(data = sel_shp2, fill = NA, alpha = 1, colour = "red3", linewidth = 0.3) +
    #geom_point(data = sel_pt, aes(x = Long, y = Lat),
    #           color = 'red', size = 0.5, alpha = 0.5) +
    theme_void() + 
    theme(legend.position = "none",
          plot.title=element_text(size = 11)) +
    labs(title = month.name[sel_mth]) +
    coord_sf(xlim = c(100.5, 102), ylim = c(2.5, 3.9))
  
  
  #counter <- counter + 1
  maplist[[m]] <- mth_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}



#################
# FACET MAP

library(gridExtra)


# extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(map_cl_sely)


### arrange layout

facet_map <- grid.arrange(grobs = maplist, ncol = 4)

#### title font format
title = grid::textGrob(paste0(reg_name_full, " Long-Term Average Monthly Rainfall (", yr_min, "-", yr_max,")\n"), 
                       gp = grid::gpar(fontsize = 16))

facet_legend_map <- grid.arrange(facet_map, mylegend, 
                                 top = title,
                                 nrow = 2, heights = c(9, 1))


# multiple maps with single particular map
#facet_all_map <- grid.arrange(facet_map, map_rf_m12, ncol = 2)


### print last plot to file
 
## widescreen
ggsave(paste0(reg_name_short, "_RF_ltmth_lgc.jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0(reg_name_short, "_RF_ltmth_lgc_a3.jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 14.19, units = "in")
	   

#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)




