rm(list = ls())
# ROMS example:
# Explore ROMS outputs:

setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
require(raster)
library(gapminder)
library(ggplot2)
library(wesanderson)
library(magick)
require(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
require(data.table)
library(mapdata)
library(marmap)
library(tidyverse)
require(mapproj)
require(reshape2)
library(gganimate)
require(lubridate)
require(scales)
require(rnaturalearth)
require(rnaturalearthdata)
library(gridExtra)
require(sf)
source('aux_functions.R')
#load('BathyData.RData')
bathy1 = read.csv('main_files/bathy1.csv')
bathy2 = read.csv('main_files/bathy2.csv')

# Read bathymetry information ---------------------------------------------

ak = map_data('worldHires','USA:Alaska')
world = ne_countries(scale = "medium", returnclass = "sf")
ini_number = 8E+6 # check this number in initial data file csv. 


# Read output files -------------------------------------------------------
# Here I will read every results file and get the appropiate format to make every figure.
# This approach is better since files will be super large for the forcast period and 1 h time step

main_folder = 'Hindcast'
cores = list.files(path = main_folder)
plot_data_1 = list()
plot_data_2 = list()
plot_data_3 = list()
plot_data_4 = list()
plot_data_5 = list()
plot_data_6 = list()
plot_data_7 = list()
plot_data_8 = list()
plot_data_9 = list()
plot_data_10 = list()
plot_data_11 = list()
plot_data_12 = list()
plot_data_13 = list()
indList = 1

for(i in seq_along(cores)) {

  core_name = cores[i]
  files_core = list.files(path = file.path(main_folder, core_name))
  fcore = grep(pattern = 'Results_files', x = list.files(path = file.path(main_folder, core_name)))
  
  for(j in seq_along(fcore)) {
    tmpData = read_data_in(eggInclude = FALSE, 
                           path = file.path(main_folder, core_name, files_core[fcore[j]]))
    
    # Prepare data for plot 1: Hatching success
    num_data = aggregate(x = list(number = tmpData$number), list(year = tmpData$year, stage = tmpData$typeName, id = tmpData$id), 
                         FUN = function(x) mean(x))
    num_dataYSL = num_data[num_data$stage == 'YSL', ]
    num_dataYSL$hatsuc = num_dataYSL$number/ini_number
    plot_data_1[[indList]] = num_dataYSL
    
    # Prepare data for plot 2: Survival 30 dph
    tmpData$ageYSLround = round(tmpData$ageYSL)
    survData = tmpData[tmpData$ageYSLround == 30, ]
    surv_data = aggregate(x = list(psurv = survData$psurvival), list(year = survData$year, id = survData$id),
                          FUN = mean, na.rm = TRUE)
    plot_data_2[[indList]] = surv_data
    
    # Prepare data for plot 3: diet rank
    rank_data = aggregate(x = list(rank = tmpData$avgRank), list(year = tmpData$year, stage = tmpData$typeName, id = tmpData$id), 
                          FUN = mean, na.rm=TRUE)
    rank_data = rank_data[!(rank_data$stage %in% c('YSL')), ] # exclude YSL and benthicjuv stage
    plot_data_3[[indList]] = rank_data

    
    # Prepare data for plot 4: diet size
    size_data = aggregate(x = list(sized = tmpData$avgSize), list(year = tmpData$year, stage = tmpData$typeName, id = tmpData$id), 
                          FUN = mean, na.rm=TRUE)
    size_data = size_data[!(size_data$stage %in% c('YSL')), ] # exclude YSL and benthicjuv stage
    plot_data_4[[indList]] = size_data
    
    # Prepare data for plot 5: DW
    epijuvData = tmpData[tmpData$typeName == 'Epijuv', ]
    wgt_data = aggregate(x = list(dw = epijuvData$DW), list(year = epijuvData$year, id = epijuvData$id), 
                         FUN = max, na.rm=TRUE)
    plot_data_5[[indList]] = wgt_data
    
    # Prepare data for plot 6: SL
    sl_data = aggregate(x = list(sl = epijuvData$SL), list(year = epijuvData$year, id = epijuvData$id), 
                        FUN = max, na.rm=TRUE)
    plot_data_6[[indList]] = sl_data
    
    # Prepare data for plot 7: final points map
    epijuvData$last_day = paste0(day(epijuvData$time), '-', month(epijuvData$time))
    epijuvData2 = epijuvData[epijuvData$last_day == '2-10', ] # final date
    plot_data_7[[indList]] = epijuvData2[,c('horizPos1', 'horizPos2', 'year')]
    
    # Prepare data for plot 8: environmental variables
    stageData = tmpData[tmpData$typeName %in% c('YSL', 'FDL', 'FDLpf', 'Epijuv'), ] 
    env_data = aggregate(x = list(temperature = stageData$temp, pCO2 = stageData$pCO2val), 
                         list(year = stageData$year, id = stageData$id), 
                         FUN = mean, na.rm=TRUE)
    int_data = gather(env_data, key = "variable", value = "value", temperature, pCO2)
    plot_data_8[[indList]] = int_data
    
    # Prepare data for plot 9: prey abundance
    prey_data = aggregate(x = list(copepods = stageData$copepod, microzoo = stageData$microzoo, 
                                   neocalanus = stageData$neocalanus, neocalanusShelf = stageData$neocalanusShelf, 
                                   euphausiids = stageData$euphausiid, euphausiidsShelf = stageData$euphausiidShelf), 
                          list(year = stageData$year, id = stageData$id), 
                          FUN = mean, na.rm=TRUE)
    int_data = gather(prey_data, key = "variable", value = "value",
                      copepods, microzoo, neocalanus, neocalanusShelf, euphausiids, euphausiidsShelf)
    plot_data_9[[indList]] = int_data
    
    
    # These plots are based on release day:
    # Prepare data for plot 10: hatching success by release day
    tmpData$relDay = lubridate::day(tmpData$startTime)
    num_data = aggregate(x = list(number = tmpData$number), list(relDay = tmpData$relDay, year = tmpData$year,
                                                                 stage = tmpData$typeName, id = tmpData$id), 
                         FUN = mean, na.rm = TRUE)
    num_dataYSL = num_data[num_data$stage == 'YSL', ]
    num_dataYSL$hatsuc = num_dataYSL$number/ini_number
    plot_data_10[[indList]] = num_dataYSL
    
    # Prepare data for plot 11: survival 30dph by release day
    survData$relDay = lubridate::day(survData$startTime)
    surv_data = aggregate(x = list(psurv = survData$psurvival), 
                          list(relDay = survData$relDay, id = survData$id, year = survData$year),
                          FUN = mean, na.rm = TRUE)
    plot_data_11[[indList]] = surv_data
    
      # Prepare data for plot 12: SL by release day
    epijuvData = tmpData[tmpData$typeName == 'Epijuv', ]
    epijuvData$relDay = lubridate::day(epijuvData$startTime)
    sl_data = aggregate(x = list(sl = epijuvData$SL), list(relDay = epijuvData$relDay, 
                                                            id = epijuvData$id), 
                         FUN = max, na.rm=TRUE)
    plot_data_12[[indList]] = sl_data
    
    # Prepare data for plot 13: DW by release day
    wgt_data = aggregate(x = list(dw = epijuvData$DW), list(relDay = epijuvData$relDay, 
                                                            id = epijuvData$id), 
                         FUN = max, na.rm=TRUE)
    plot_data_13[[indList]] = wgt_data
        
    # Get to next indicator:
    indList = indList + 1
    
  }
  
}


# -------------------------------------------------------------------------
# Analyze survival data here: (some changes needed)
indList = 1
mort_data = list()

for(i in seq_along(cores)) {
  
  core_name = cores[i]
  files_core = list.files(path = file.path(main_folder, core_name))
  fcore = grep(pattern = 'Results_files', 
               x = list.files(path = file.path(main_folder, core_name)))
  
  for(j in seq_along(fcore)) {
    
    tmpData = read_data_in(eggInclude = FALSE, 
                           path = file.path(main_folder, core_name, files_core[fcore[j]]))
    tmpData$ageYSLround = round(tmpData$ageYSL)
    myData = tmpData[(tmpData$ageYSLround <= 30) & (tmpData$progYSA >= 1 | is.na(tmpData$progYSA)), ]
    myData = myData[which(myData$mortstarv < 10), ]

    myData1 = aggregate(list(mortfish = myData$mortfish, mortinv = myData$mortinv,
                             mortstarv = myData$mortstarv), 
                        list(year = myData$year, id = myData$id),
                        mean)
    myData2 = myData1[,-2]
    myData3 = myData2 %>% 
                gather('type', 'value', -year)
    
    mort_data[[indList]] = myData3
    indList = indList + 1
      
  }
    
}
  


# Analyze BY YEAR ---------------------------------------------------------
# -------------------------------------------------------------------------

# Plot 1: Hatching success --------------------------------------------------------

plot_data = bind_rows(plot_data_1, .id = "column_label")
plot_data$year = as.factor(plot_data$year)

# Plot:
pl1 = ggplot(plot_data, aes(x = year, y = hatsuc)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Hatching success') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

# ggsave(filename = 'figures/hind_hatchsuccess.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)


# Plot 2: Survival 30 days --------------------------------------------------------

plot_data = bind_rows(plot_data_2, .id = "column_label")
plot_data$year = as.factor(plot_data$year)

# Plot:
pl2 = ggplot(plot_data, aes(x = year, y = psurv)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Survival probability (30 dph)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# ggsave(filename = 'figures/hind_survprob.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)


# Plot 2.2: survival rate by category -------------------------------------
plot_data = bind_rows(mort_data, .id = "column_label")
plot_data$year = as.factor(plot_data$year)
plot_data$type = factor(plot_data$type, levels = c("mortfish", "mortinv", "mortstarv"))
plot_data$type = factor(plot_data$type, labels = c('Fish predation', 'Invertebrate predation', 'Starvation'))

ggplot(plot_data, aes(x = year, y = value)) +
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Mortality (1e+6)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~type, ncol = 3, scales = 'free_y')

ggsave(filename = 'figures/hind_mort.png', device = 'png', width = 190, height = 70, units = 'mm', dpi = 500)

# Plot 3: Changes rank in diet ---------------------------------------------------------

plot_data = bind_rows(plot_data_3, .id = "column_label")
plot_data$year = as.factor(plot_data$year)
plot_data$stage = as.factor(plot_data$stage)

# Plot:
pd1 = ggplot(plot_data, aes(x = year, y = rank)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Prey preference') +
  scale_y_reverse(limits = c(6, 1), breaks = 1:6, labels = c('EupO', 'EupS', 'NCaS', 'NCaO', 'Cop', 'MZP')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(~stage)

# ggsave(filename = 'figures/hind_dietrank.png', device = 'png', width = 180, height = 75, units = 'mm', dpi = 500)


# Plot 4: Changes size in diet ---------------------------------------------------------

plot_data = bind_rows(plot_data_4, .id = "column_label")
plot_data$year = as.factor(plot_data$year)
plot_data$stage = as.factor(plot_data$stage)

# Plot:
pd2 = ggplot(plot_data, aes(x = year, y = sized)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Prey size (mm)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~stage, scales = 'free_y')

# ggsave(filename = 'figures/hind_dietsize.png', device = 'png', width = 180, height = 75, units = 'mm', dpi = 500)

# Prey figure:
png(filename = 'figures/hind_preyfig.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(pd1, pd2)
dev.off()

# Plot 5: Changes in DW ----------------------------------------------------------

plot_data = bind_rows(plot_data_5, .id = "column_label")
plot_data$year = as.factor(plot_data$year)

# Plot:
pl4 = ggplot(plot_data, aes(x = year, y = dw)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Dry weight (mg)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# ggsave(filename = 'figures/hind_dryw.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)

# Plot 6: Changes in SL ----------------------------------------------------------

plot_data = bind_rows(plot_data_6, .id = "column_label")
plot_data$year = as.factor(plot_data$year)

# Plot:
pl3 = ggplot(plot_data, aes(x = year, y = sl)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  ylab('Standard length (mm)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# ggsave(filename = 'figures/hind_stdlen.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)

# Prey figure:
png(filename = 'figures/hind_indfig.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(pl1, pl2, pl3, pl4)
dev.off()


# Plot 7: Map final points -------------------------------------------------

plot_data = bind_rows(plot_data_7, .id = "column_label")
plot_data$year = as.factor(plot_data$year)

plot_final_locations(plot_data)
ggsave(filename = 'figures/hind_settlepoint.png', device = 'png', width = 180, height = 150, units = 'mm', dpi = 500)


# Plot 8: Environmental variables -------------------------------------------------

plot_data = bind_rows(plot_data_8, .id = "column_label")
plot_data$year = as.factor(plot_data$year)
plot_data$variable = factor(plot_data$variable, levels = c("temperature", 'pCO2'))
plot_data$variable2 = factor(plot_data$variable, labels = c("Temperature~(C)", 'pCO[2]~(mu*atm)'))

# Plot:
ggplot(plot_data, aes(x = year, y = value)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 1) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = 'figures/hind_envvar.png', device = 'png', width = 95, height = 150, units = 'mm', dpi = 500)


# Plot 9: prey data -------------------------------------------------------

plot_data = bind_rows(plot_data_9, .id = "column_label")
plot_data$year = as.factor(plot_data$year)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "euphausiidsShelf", "neocalanusShelf", 
                                                           "neocalanus", "copepods", "microzoo"))
plot_data$variable2 = factor(plot_data$variable, labels = c("EupO~(mg~C/m^3)",
                                                            "EupS~(mg~C/m^3)", "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)", "MZP~(mg~C/m^3)"))

# Plot:
ggplot(plot_data, aes(x = year, y = value)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = 'figures/hind_preyvar.png', device = 'png', width = 180, height = 100, units = 'mm', dpi = 500)


# Analyze BY RELEASED DAY ---------------------------------------------------------
# -------------------------------------------------------------------------

# Plot 10: Hatching success --------------------------------------------------------

plot_data = bind_rows(plot_data_10, .id = "column_label")
plot_data = data_summary(plot_data, varname = "hatsuc", groupnames = c("relDay"))
plot_data$relDay = as.factor(plot_data$relDay)

# Plot:
pr1 = ggplot(plot_data, aes(x = relDay, y = hatsuc)) + 
  geom_pointrange(aes(ymin = hatsuc-sd, ymax=hatsuc+sd)) +
  scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
  theme_bw() +
  xlab('') +
  ylab('Hatching success')


# ggsave(filename = 'figures/hind_relday_hatchsuccess.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)


# Plot 11: Survival 30 days --------------------------------------------------------

plot_data = bind_rows(plot_data_11, .id = "column_label")
plot_data = data_summary(plot_data, varname = "psurv", groupnames = c("relDay"))
plot_data$relDay = as.factor(plot_data$relDay)

# Plot:
pr2 = ggplot(plot_data, aes(x = relDay, y = psurv)) + 
  geom_pointrange(aes(ymin = psurv-sd, ymax=psurv+sd)) +
  scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
  theme_bw() +
  xlab('') +
  ylab('Survival probability (30 dph)')

# ggsave(filename = 'figures/hind_relday_survprob.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)

# Plot 12: SL --------------------------------------------------------

plot_data = bind_rows(plot_data_12, .id = "column_label")
plot_data = data_summary(plot_data, varname = "sl", groupnames = c("relDay"))
plot_data$relDay = as.factor(plot_data$relDay)

# Plot:
pr3 = ggplot(plot_data, aes(x = relDay, y = sl)) + 
  geom_pointrange(aes(ymin = sl-sd, ymax=sl+sd)) +
  scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
  theme_bw() +
  xlab('') +
  ylab('Standard length (mm)')

# ggsave(filename = 'figures/hind_relday_survprob.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)

# Plot 13: DW --------------------------------------------------------

plot_data = bind_rows(plot_data_13, .id = "column_label")
plot_data = data_summary(plot_data, varname = "dw", groupnames = c("relDay"))
plot_data$relDay = as.factor(plot_data$relDay)

# Plot:
pr4 = ggplot(plot_data, aes(x = relDay, y = dw)) + 
  geom_pointrange(aes(ymin = dw-sd, ymax=dw+sd)) +
  scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
  theme_bw() +
  xlab('') +
  ylab('Dry weight (mg)')

# ggsave(filename = 'figures/hind_relday_survprob.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)


# Release day figure:
png(filename = 'figures/hind_reldayfig.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(pr1, pr2, pr3, pr4)
dev.off()


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Information to map
shift_value_1 <- 0
shift_value_2 <- 360

map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
  dplyr::filter(region != "Antarctica")

country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x=long, y = lat, group = group),
                                fill = "gainsboro",
                                color = "gainsboro",
                                size = 0.15)

# -------------------------------------------------------------------------
# Plot trajectories:

# List to save plots:
plotList = list()
indList = 1

for(i in seq_along(cores)) {
  
  core_name = cores[i]
  files_core = list.files(path = file.path(main_folder, core_name))
  fcore = grep(pattern = 'Results_files', x = list.files(path = file.path(main_folder, core_name)))
  
  for(j in seq_along(fcore)) {
   
    tmpData = read_data_in(eggInclude = FALSE, 
                           path = file.path(main_folder, core_name, files_core[fcore[j]]))
    tmpData$horizPos1 = ifelse(test = tmpData$horizPos1 > 0, yes = tmpData$horizPos1 - 360, 
                               no = tmpData$horizPos1)
    tmpData$horizPos1 = tmpData$horizPos1 + 360
    tmpData$relDay = lubridate::day(tmpData$startTime)
    
    # Find initial and final points
    init_points = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]

    plotList[[indList]] = plot_trajectory(tmpData)
    
    indList = indList+1
     
  } 
    
}

# Plot trajectories: 

plotList2 = plotList[c(1,5,8,2,6,9,3,7,10,4)] # reorder list by year

png(filename = 'figures/hind_trajectories.png', width = 190, height = 200, 
    units = 'mm', res = 500)
do.call("grid.arrange", c(plotList2, ncol = 3))
dev.off()



# Plot spatial indicators -------------------------------------------------

spatdat = list()
distdat = list()
indList = 1

for(i in seq_along(cores)) {
  
  core_name = cores[i]
  files_core = list.files(path = file.path(main_folder, core_name))
  fcore = grep(pattern = 'Results_files', x = list.files(path = file.path(main_folder, core_name)))
  
  for(j in seq_along(fcore)) {
    
    tmpData = read_data_in(eggInclude = FALSE, 
                           path = file.path(main_folder, core_name, files_core[fcore[j]]))
    tmpData$horizPos1 = ifelse(test = tmpData$horizPos1 > 0, yes = tmpData$horizPos1 - 360, 
                               no = tmpData$horizPos1)
    tmpData$horizPos1 = tmpData$horizPos1 + 360
    tmpData$relDay = lubridate::day(tmpData$startTime)
    
    # Find initial and final points
    init_points = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
    end_points = tmpData[tmpData[ , .I[which.max(time)], by = id]$V1]
    
    # Calculate distance per id:
    mergePoints = rbind(init_points[,c('horizPos1', 'horizPos2', 'id', 'relDay')],
                        end_points[,c('horizPos1', 'horizPos2', 'id', 'relDay')])
    idRelDay = aggregate(list(relday = mergePoints$relDay), list(id = mergePoints$id), unique)
    distMat = mergePoints %>%
      group_by(id)%>%
      group_map(~raster::pointDistance(.x[,c('horizPos1', 'horizPos2')], lonlat=TRUE))
    distVals = unlist(distMat)
    distVals = distVals[distVals > 0 & !is.na(distVals)]
    distValsNm = (distVals/111000)*60 # units: nm
    distdat[[indList]] = data.frame(dist = distValsNm, year = unique(tmpData$year)[1],
                                    relDay = idRelDay$relday)
    
    # Calculate CG and Inertia:
    
    spatInfoIni = init_points %>%
      group_by(year, relDay)%>%
      group_map(~cgi(x = .x$horizPos1, y = .x$horizPos2))
    spatInfoEnd = end_points %>%
      group_by(year, relDay)%>%
      group_map(~cgi(x = .x$horizPos1, y = .x$horizPos2))
    spatdat[[indList]] = data.frame(CG_x_ini = sapply(spatInfoIni, "[[", 1), 
                                    CG_x_end = sapply(spatInfoEnd, "[[", 1),
                                    CG_y_ini = sapply(spatInfoIni, "[[", 2),
                                    CG_y_end = sapply(spatInfoEnd, "[[", 2),
                                    I_ini = sapply(spatInfoIni, "[[", 3), 
                                    I_end = sapply(spatInfoEnd, "[[", 3),
                                    year = unique(tmpData$year)[1],
                                    relDay = unique(idRelDay$relday))
    
    indList = indList+1
    
  } 
  
}


# Plot : Distance traveled per year:

dist_data = bind_rows(distdat, .id = "column_label")
dist_data$dist = dist_data$dist*1.852 # from nm to km
plot_data = dist_data
plot_data$year = as.factor(plot_data$year)

# Plot:
ps2 = ggplot(plot_data, aes(x = year, y = dist)) + 
  geom_boxplot(fill='gray90', color="black", width=0.5, lwd = 0.25, outlier.size = 0.3) +
  theme_bw() +
  xlab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Distance (km)') +
  annotate("text",  x=Inf, y = Inf, label = "B", vjust=1, hjust=1, size = 5)

# ggsave(filename = 'figures/hind_disttrav.png', device = 'png', width = 180, height = 90, units = 'mm', dpi = 500)


# Plot center gravity (final) per year

cgdata = bind_rows(spatdat, .id = "column_label")
init_dat = data.frame(cgx = unique(cgdata$CG_x_ini)[1], cgy = unique(cgdata$CG_y_ini))
plot_data = aggregate(list(cgx = cgdata$CG_x_end, cgy = cgdata$CG_y_end), list(year = cgdata$year), mean)
plot_data$year = as.factor(plot_data$year)

ps1 = plot_direction(plot_data, init_dat)
ps1 = ps1 + annotate("text",  x=Inf, y = Inf, label = "A", vjust=1, hjust=1, size = 5)
# ggsave(filename = 'figures/hind_direction.png', device = 'png', width = 100, height = 70, units = 'mm', dpi = 500)


# Plot inertia per year:

cgdata = bind_rows(spatdat, .id = "column_label")
plot_data = aggregate(list(inertiaIni = cgdata$I_ini, inertiaEnd = cgdata$I_end), list(year = cgdata$year), mean)
plot_data$diffInertia = ((plot_data$inertiaEnd - plot_data$inertiaIni)/plot_data$inertiaIni)*100
plot_data$year = as.factor(plot_data$year)

# Plot:
ps3 = ggplot(plot_data, aes(x = year, y = diffInertia)) + 
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Difference in inertia (%)') +
  annotate("text",  x=Inf, y = Inf, label = "C", vjust=1, hjust=1, size = 5)

# ggsave(filename = 'figures/hind_inertia.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)

png(filename = 'figures/hind_spatialind.png', width = 190, height = 70, 
    units = 'mm', res = 500)
grid.arrange(ps1, ps2, ps3, nrow = 1)
dev.off()


