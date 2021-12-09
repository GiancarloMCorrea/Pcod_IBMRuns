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

main_folder = 'Forecast'
scenarios = list.files(main_folder)
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
mort_data = list()
spatdat = list()
distdat = list()
plot_anom_1 = list()
plot_anom_2 = list()
plot_anom_3 = list()
plot_anom_4 = list()
indList = 1
scenarioLevels = c("GFDL_rcp85", "GFDL_rcp45")
mainPal = 'Set1'

for(k in seq_along(scenarios)) {

  cores = list.files(path = file.path(main_folder, scenarios[k]))
  
  for(i in seq_along(cores)) {
  
    core_name = cores[i]
    files_core = list.files(path = file.path(main_folder, scenarios[k], core_name))
    fcore = grep(pattern = 'Results_files', x = list.files(path = file.path(main_folder, scenarios[k], core_name)))
    
    for(j in seq_along(fcore)) {
      
      tmpData = read_data_in(eggInclude = FALSE, 
                             path = file.path(main_folder, scenarios[k], core_name, files_core[fcore[j]]))

      # Find initial and final points
      init_points = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
      end_points = tmpData[tmpData[ , .I[which.max(time)], by = id]$V1]
      
      # Prepare data for plot 1: Hatching success
      num_data = aggregate(x = list(number = tmpData$number), list(year = tmpData$year, 
                                                                   stage = tmpData$typeName, id = tmpData$id), 
                           FUN = function(x) mean(x))
      num_dataYSL = num_data[num_data$stage == 'YSL', ]
      num_dataYSL$hatsuc = num_dataYSL$number/ini_number
      # Prepare data to save:
      sel_var = 'hatsuc'
      toPlotData = num_dataYSL
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      colnames(prevAnom)[3] = 'value'
      prevAnom$scenario = scenarios[k]
      plot_anom_1[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_1[[indList]] = fdata
      
      # Prepare data for plot 2: Survival 30 dph
      tmpData$ageYSLround = round(tmpData$ageYSL)
      survData = tmpData[tmpData$ageYSLround == 30, ]
      surv_data = aggregate(x = list(psurv = survData$psurvival), list(year = survData$year, id = survData$id),
                            FUN = mean, na.rm = TRUE)
      # Prepare data to save:
      sel_var = 'psurv'
      toPlotData = surv_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      prevAnom$scenario = scenarios[k]
      colnames(prevAnom)[3] = 'value'
      plot_anom_2[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_2[[indList]] = fdata

      # Prepare data for plot 8: environmental variables
      stageData = tmpData[tmpData$typeName %in% c('YSL', 'FDL', 'FDLpf', 'Epijuv'), ] 
      env_data = aggregate(x = list(temperature = stageData$temp, pCO2 = stageData$pCO2val), 
                           list(year = stageData$year, id = stageData$id), 
                           FUN = mean, na.rm=TRUE)
      int_data = gather(env_data, key = "variable", value = "value", temperature, pCO2)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = int_data
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, variable = p50$variable, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_8[[indList]] = fdata
      
      # Prepare data for plot 9: prey abundance
      prey_data = aggregate(x = list(copepods = stageData$copepod, microzoo = stageData$microzoo, 
                                     neocalanus = stageData$neocalanus, neocalanusShelf = stageData$neocalanusShelf, 
                                     euphausiids = stageData$euphausiid, euphausiidsShelf = stageData$euphausiidShelf), 
                            list(year = stageData$year, id = stageData$id), 
                            FUN = mean, na.rm=TRUE)
      int_data = gather(prey_data, key = "variable", value = "value",
                        copepods, microzoo, neocalanus, neocalanusShelf, euphausiids, euphausiidsShelf)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = int_data
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, variable = p50$variable, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_9[[indList]] = fdata
      
      # Mortality:
      myData = tmpData[(tmpData$ageYSLround <= 30) & (tmpData$progYSA >= 1 | is.na(tmpData$progYSA)), ]
      myData = myData[which(myData$mortstarv < 10), ]
      
      myData1 = aggregate(list(mortfish = myData$mortfish, mortinv = myData$mortinv,
                               mortstarv = myData$mortstarv), 
                          list(year = myData$year, id = myData$id), FUN = mean)
      myData2 = myData1[,-2]
      myData3 = myData2 %>% 
        gather('type', 'value', -year)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = myData3
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, type = p50$type, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      mort_data[[indList]] = fdata
      
      # Prepare data for plot 3: diet rank
      rank_data = aggregate(x = list(rank = tmpData$avgRank), list(year = tmpData$year, 
                                                                   stage = tmpData$typeName, id = tmpData$id), 
                            FUN = mean, na.rm=TRUE)
      rank_data = rank_data[!(rank_data$stage %in% c('YSL')), ] # exclude YSL and benthicjuv stage
      # Prepare data to save:
      sel_var = 'rank'
      toPlotData = rank_data
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, stage = p50$stage, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_3[[indList]] = fdata
  
      
      # Prepare data for plot 4: diet size
      size_data = aggregate(x = list(sized = tmpData$avgSize), list(year = tmpData$year, stage = 
                                                                      tmpData$typeName, id = tmpData$id), 
                            FUN = mean, na.rm=TRUE)
      size_data = size_data[!(size_data$stage %in% c('YSL')), ] # exclude YSL and benthicjuv stage
      # Prepare data to save:
      sel_var = 'sized'
      toPlotData = size_data
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, stage = p50$stage, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_4[[indList]] = fdata

      # -------------------------------------------
      # Delete dead individuals from here:
      delRows = which(tmpData$DW < 0.75*tmpData$dwmax) # remove ind dead.
      tmpData = tmpData[-delRows, ]
      
      # Prepare data for plot 5: DW
      epijuvData = tmpData[tmpData$typeName == 'Epijuv', ]
      wgt_data = aggregate(x = list(dw = epijuvData$DW), list(year = epijuvData$year, id = epijuvData$id), 
                           FUN = max, na.rm=TRUE)
      # Prepare data to save:
      sel_var = 'dw'
      toPlotData = wgt_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      colnames(prevAnom)[3] = 'value'
      prevAnom$scenario = scenarios[k]
      plot_anom_4[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_5[[indList]] = fdata

      # Prepare data for plot 6: SL
      sl_data = aggregate(x = list(sl = epijuvData$SL), list(year = epijuvData$year, id = epijuvData$id), 
                          FUN = max, na.rm=TRUE)
      # Prepare data to save:
      sel_var = 'sl'
      toPlotData = sl_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      colnames(prevAnom)[3] = 'value'
      prevAnom$scenario = scenarios[k]
      plot_anom_3[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_6[[indList]] = fdata
      
      # Prepare data for plot 7: final points map
      # epijuvData$last_day = paste0(day(epijuvData$time), '-', month(epijuvData$time))
      # epijuvData2 = epijuvData[epijuvData$last_day == '2-10', ] # final date
      # plot_data_7[[indList]] = epijuvData2[,c('horizPos1', 'horizPos2', 'year')]
      

      # These plots are based on release day:
      # Prepare data for plot 10: hatching success by release day
      # tmpData$relDay = lubridate::day(tmpData$startTime)
      # num_data = aggregate(x = list(number = tmpData$number), list(relDay = tmpData$relDay, year = tmpData$year,
      #                                                              stage = tmpData$typeName, id = tmpData$id), 
      #                      FUN = mean, na.rm = TRUE)
      # num_dataYSL = num_data[num_data$stage == 'YSL', ]
      # num_dataYSL$hatsuc = num_dataYSL$number/ini_number
      # plot_data_10[[indList]] = num_dataYSL
      # 
      # # Prepare data for plot 11: survival 30dph by release day
      # survData$relDay = lubridate::day(survData$startTime)
      # surv_data = aggregate(x = list(psurv = survData$psurvival), 
      #                       list(relDay = survData$relDay, id = survData$id, year = survData$year),
      #                       FUN = mean, na.rm = TRUE)
      # plot_data_11[[indList]] = surv_data
      # 
      #   # Prepare data for plot 12: SL by release day
      # epijuvData = tmpData[tmpData$typeName == 'Epijuv', ]
      # epijuvData$relDay = lubridate::day(epijuvData$startTime)
      # sl_data = aggregate(x = list(sl = epijuvData$SL), list(relDay = epijuvData$relDay, 
      #                                                         id = epijuvData$id), 
      #                      FUN = max, na.rm=TRUE)
      # plot_data_12[[indList]] = sl_data
      # 
      # # Prepare data for plot 13: DW by release day
      # wgt_data = aggregate(x = list(dw = epijuvData$DW), list(relDay = epijuvData$relDay, 
      #                                                         id = epijuvData$id), 
      #                      FUN = max, na.rm=TRUE)
      # plot_data_13[[indList]] = wgt_data
          
      # Calculate distance per id:
      mergePoints = rbind(init_points[,c('horizPos1', 'horizPos2', 'id')],
                          end_points[,c('horizPos1', 'horizPos2', 'id')])
      distMat = mergePoints %>%
        group_by(id)%>%
        group_map(~raster::pointDistance(.x[,c('horizPos1', 'horizPos2')], lonlat=TRUE))
      distVals = unlist(distMat)
      distVals = distVals[distVals > 0 & !is.na(distVals)]
      distValsNm = (distVals/111000)*60 # units: nm
      preDist = data.frame(dist = distValsNm, year = unique(tmpData$year)[1])
      # Prepare data to save:
      sel_var = 'dist'
      toPlotData = preDist
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = 0.05, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = 0.95, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      distdat[[indList]] = fdata
      
      # Calculate CG and Inertia:
      init_points$horizPos1 = ifelse(test = init_points$horizPos1 > 0, yes = init_points$horizPos1 - 360, 
                                 no = init_points$horizPos1)
      init_points$horizPos1 = init_points$horizPos1 + 360
      init_points$relDay = lubridate::day(init_points$startTime)
      
      end_points$horizPos1 = ifelse(test = end_points$horizPos1 > 0, yes = end_points$horizPos1 - 360, 
                                     no = end_points$horizPos1)
      end_points$horizPos1 = end_points$horizPos1 + 360
      end_points$relDay = lubridate::day(end_points$startTime)
      
      spatInfoIni = init_points %>%
        group_by(year)%>%
        group_map(~cgi(x = .x$horizPos1, y = .x$horizPos2))
      spatInfoEnd = end_points %>%
        group_by(year)%>%
        group_map(~cgi(x = .x$horizPos1, y = .x$horizPos2))
      spatdat[[indList]] = data.frame(CG_x_ini = sapply(spatInfoIni, "[[", 1), 
                                      CG_x_end = sapply(spatInfoEnd, "[[", 1),
                                      CG_y_ini = sapply(spatInfoIni, "[[", 2),
                                      CG_y_end = sapply(spatInfoEnd, "[[", 2),
                                      I_ini = sapply(spatInfoIni, "[[", 3), 
                                      I_end = sapply(spatInfoEnd, "[[", 3),
                                      year = unique(tmpData$year)[1],
                                      scenario = scenarios[k])
      
      # Get to next indicator:
      print(indList)
      indList = indList + 1
      
    }
    
  }

}

# Analyze BY YEAR ---------------------------------------------------------
# -------------------------------------------------------------------------

# Plot 1: Hatching success --------------------------------------------------------

plot_data = bind_rows(plot_data_1, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

# Plot:
pl1 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Hatching success') +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none')


# ggsave(filename = 'figures/hind_hatchsuccess.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)


# Plot 2: Survival 30 days --------------------------------------------------------

plot_data = bind_rows(plot_data_2, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

# Plot:
pl2 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Survival probability (30 dph)') +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none')

# ggsave(filename = 'figures/hind_survprob.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)


# Plot 2.2: survival rate by category -------------------------------------
plot_data = bind_rows(mort_data, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$type = factor(plot_data$type, levels = c("mortfish", "mortinv", "mortstarv"))
plot_data$type = factor(plot_data$type, labels = c('Fish predation', 'Invertebrate predation', 'Starvation'))

ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab(expression(paste('Mortality 30 dph (', s^{-1}, ', ', 10^{-6}, ')'))) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none') +
  facet_wrap(~type, ncol = 3, scales = 'free_y')

ggsave(filename = 'figures/fore_mort.png', device = 'png', width = 190, height = 70, units = 'mm', dpi = 500)

# Plot 3: Changes rank in diet ---------------------------------------------------------

plot_data = bind_rows(plot_data_3, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$stage = as.factor(plot_data$stage)

# Plot:
pd1 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Prey preference') +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_reverse(limits = c(6.5, 0.5), breaks = 1:6, labels = c('EupO', 'EupS', 'NCaS', 'NCaO', 'Cop', 'MZP')) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none') +
  facet_grid(~stage)


# ggsave(filename = 'figures/hind_dietrank.png', device = 'png', width = 180, height = 75, units = 'mm', dpi = 500)


# Plot 4: Changes size in diet ---------------------------------------------------------

plot_data = bind_rows(plot_data_4, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$stage = as.factor(plot_data$stage)

# Plot:
pd2 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Prey size (mm)') +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none') +
  facet_wrap(~stage, scales = 'free_y')

# ggsave(filename = 'figures/hind_dietsize.png', device = 'png', width = 180, height = 75, units = 'mm', dpi = 500)

# Prey figure:
png(filename = 'figures/fore_preyfig.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(pd1, pd2)
dev.off()

# Plot 5: Changes in DW ----------------------------------------------------------

plot_data = bind_rows(plot_data_5, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

# Plot:
pl4 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Dry weight (mg)') +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none')


# ggsave(filename = 'figures/hind_dryw.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)

# Plot 6: Changes in SL ----------------------------------------------------------

plot_data = bind_rows(plot_data_6, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

# Plot:
pl3 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Standard length (mm)') +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none')

# ggsave(filename = 'figures/hind_stdlen.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)

# Prey figure:
png(filename = 'figures/fore_indfig.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(pl1, pl2, pl3, pl4)
dev.off()



# Plot map anomalies ----------------------------------------------------------
init_points2 = init_points[,c('id', 'horizPos1', 'horizPos2')]
init_points2 = init_points2[order(init_points2$id), ]

# Hatch success:
plot_data = bind_rows(plot_anom_1, .id = "column_label")
plot_data = aggregate(list(value = plot_data$value), list(id = plot_data$id), FUN = mean)
plot_data$lon = init_points2$horizPos1[match(plot_data$id, init_points2$id)]
plot_data$lat = init_points2$horizPos2[match(plot_data$id, init_points2$id)]
plot_data$value = scales::rescale(x = plot_data$value, to = c(-1, 1))

an1 = plot_anomalies(plot_data = plot_data) + 
  annotate("text",  x=Inf, y = Inf, label = "A", vjust=1, hjust=1, size = 5)
  
# Survival:
plot_data = bind_rows(plot_anom_2, .id = "column_label")
plot_data = aggregate(list(value = plot_data$value), list(id = plot_data$id), FUN = mean)
plot_data$lon = init_points2$horizPos1[match(plot_data$id, init_points2$id)]
plot_data$lat = init_points2$horizPos2[match(plot_data$id, init_points2$id)]
plot_data$value = scales::rescale(x = plot_data$value, to = c(-1, 1))

an2 = plot_anomalies(plot_data = plot_data) + 
  annotate("text",  x=Inf, y = Inf, label = "B", vjust=1, hjust=1, size = 5)

# SL:
plot_data = bind_rows(plot_anom_3, .id = "column_label")
plot_data = aggregate(list(value = plot_data$value), list(id = plot_data$id), FUN = mean)
plot_data$lon = init_points2$horizPos1[match(plot_data$id, init_points2$id)]
plot_data$lat = init_points2$horizPos2[match(plot_data$id, init_points2$id)]
plot_data$value = scales::rescale(x = plot_data$value, to = c(-1, 1))

an3 = plot_anomalies(plot_data = plot_data) + 
  annotate("text",  x=Inf, y = Inf, label = "C", vjust=1, hjust=1, size = 5)

# DW:
plot_data = bind_rows(plot_anom_4, .id = "column_label")
plot_data = aggregate(list(value = plot_data$value), list(id = plot_data$id), FUN = mean)
plot_data$lon = init_points2$horizPos1[match(plot_data$id, init_points2$id)]
plot_data$lat = init_points2$horizPos2[match(plot_data$id, init_points2$id)]
plot_data$value = scales::rescale(x = plot_data$value, to = c(-1, 1))

an4 =  plot_anomalies(plot_data = plot_data) + 
  annotate("text",  x=Inf, y = Inf, label = "D", vjust=1, hjust=1, size = 5)

# Save plot:
png(filename = 'figures/fore_anomap.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(an1, an2, an3, an4)
dev.off()



# Plot 7: Map final points -------------------------------------------------

# plot_data = bind_rows(plot_data_7, .id = "column_label")
# plot_data$year = as.factor(plot_data$year)
# 
# plot_final_locations(plot_data)
# ggsave(filename = 'figures/hind_settlepoint.png', device = 'png', width = 180, height = 150, units = 'mm', dpi = 500)


# Plot 8: Environmental variables -------------------------------------------------

plot_data = bind_rows(plot_data_8, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$variable = factor(plot_data$variable, levels = c("temperature", 'pCO2'))
plot_data$variable2 = factor(plot_data$variable, labels = c("Temperature~(C)", 'pCO[2]~(mu*atm)'))

# Plot:
ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
           labeller = label_parsed, ncol = 1) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = 'figures/fore_envvar.png', device = 'png', width = 95, height = 150, units = 'mm', dpi = 500)


# Plot 9: prey data -------------------------------------------------------

plot_data = bind_rows(plot_data_9, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "euphausiidsShelf", "neocalanusShelf", 
                                                           "neocalanus", "copepods", "microzoo"))
plot_data$variable2 = factor(plot_data$variable, labels = c("EupO~(mg~C/m^3)",
                                                            "EupS~(mg~C/m^3)", "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)", "MZP~(mg~C/m^3)"))

# Plot:
ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = 'figures/fore_preyvar.png', device = 'png', width = 180, height = 100, units = 'mm', dpi = 500)


# Analyze BY RELEASED DAY ---------------------------------------------------------
# -------------------------------------------------------------------------

# Plot 10: Hatching success --------------------------------------------------------
# 
# plot_data = bind_rows(plot_data_10, .id = "column_label")
# plot_data = data_summary(plot_data, varname = "hatsuc", groupnames = c("relDay"))
# plot_data$relDay = as.factor(plot_data$relDay)
# 
# # Plot:
# pr1 = ggplot(plot_data, aes(x = relDay, y = hatsuc)) + 
#   geom_pointrange(aes(ymin = hatsuc-sd, ymax=hatsuc+sd)) +
#   scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
#   theme_bw() +
#   xlab('') +
#   ylab('Hatching success')
# 
# 
# # ggsave(filename = 'figures/hind_relday_hatchsuccess.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)
# 
# 
# # Plot 11: Survival 30 days --------------------------------------------------------
# 
# plot_data = bind_rows(plot_data_11, .id = "column_label")
# plot_data = data_summary(plot_data, varname = "psurv", groupnames = c("relDay"))
# plot_data$relDay = as.factor(plot_data$relDay)
# 
# # Plot:
# pr2 = ggplot(plot_data, aes(x = relDay, y = psurv)) + 
#   geom_pointrange(aes(ymin = psurv-sd, ymax=psurv+sd)) +
#   scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
#   theme_bw() +
#   xlab('') +
#   ylab('Survival probability (30 dph)')
# 
# # ggsave(filename = 'figures/hind_relday_survprob.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)
# 
# # Plot 12: SL --------------------------------------------------------
# 
# plot_data = bind_rows(plot_data_12, .id = "column_label")
# plot_data = data_summary(plot_data, varname = "sl", groupnames = c("relDay"))
# plot_data$relDay = as.factor(plot_data$relDay)
# 
# # Plot:
# pr3 = ggplot(plot_data, aes(x = relDay, y = sl)) + 
#   geom_pointrange(aes(ymin = sl-sd, ymax=sl+sd)) +
#   scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
#   theme_bw() +
#   xlab('') +
#   ylab('Standard length (mm)')
# 
# # ggsave(filename = 'figures/hind_relday_survprob.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)
# 
# # Plot 13: DW --------------------------------------------------------
# 
# plot_data = bind_rows(plot_data_13, .id = "column_label")
# plot_data = data_summary(plot_data, varname = "dw", groupnames = c("relDay"))
# plot_data$relDay = as.factor(plot_data$relDay)
# 
# # Plot:
# pr4 = ggplot(plot_data, aes(x = relDay, y = dw)) + 
#   geom_pointrange(aes(ymin = dw-sd, ymax=dw+sd)) +
#   scale_x_discrete(labels = c('01/03', '15/03', '31/03')) +
#   theme_bw() +
#   xlab('') +
#   ylab('Dry weight (mg)')
# 
# # ggsave(filename = 'figures/hind_relday_survprob.png', device = 'png', width = 180, height = 120, units = 'mm', dpi = 500)
# 
# 
# # Release day figure:
# png(filename = 'figures/hind_reldayfig.png', width = 190, height = 130, 
#     units = 'mm', res = 500)
# grid.arrange(pr1, pr2, pr3, pr4)
# dev.off()
# 

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
# plotList = list()
# indList = 1
# 
# for(i in seq_along(cores)) {
#   
#   core_name = cores[i]
#   files_core = list.files(path = file.path(main_folder, core_name))
#   fcore = grep(pattern = 'Results_files', x = list.files(path = file.path(main_folder, core_name)))
#   
#   for(j in seq_along(fcore)) {
#    
#     tmpData = read_data_in(eggInclude = FALSE, 
#                            path = file.path(main_folder, core_name, files_core[fcore[j]]))
#     tmpData$horizPos1 = ifelse(test = tmpData$horizPos1 > 0, yes = tmpData$horizPos1 - 360, 
#                                no = tmpData$horizPos1)
#     tmpData$horizPos1 = tmpData$horizPos1 + 360
#     tmpData$relDay = lubridate::day(tmpData$startTime)
#     
#     # Find initial and final points
#     init_points = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
# 
#     plotList[[indList]] = plot_trajectory(tmpData)
#     
#     indList = indList+1
#      
#   } 
#     
# }
# 
# # Plot trajectories: 
# 
# plotList2 = plotList[c(1,5,8,2,6,9,3,7,10,4)] # reorder list by year
# 
# png(filename = 'figures/hind_trajectories.png', width = 190, height = 200, 
#     units = 'mm', res = 500)
# do.call("grid.arrange", c(plotList2, ncol = 3))
# dev.off()



# Plot spatial indicators -------------------------------------------------
# Plot : Distance traveled per year:

dist_data = bind_rows(distdat, .id = "column_label")
dist_data$q50 = dist_data$q50*1.852 # from nm to km
dist_data$q5 = dist_data$q5*1.852 # from nm to km
dist_data$q95 = dist_data$q95*1.852 # from nm to km
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data = dist_data

# Plot:
ps2 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Distance (km)') +
  scale_x_continuous(expand=c(0, 0)) +
  coord_cartesian(ylim = c(0, 1000)) +  
  scale_color_brewer(palette = mainPal) +
  scale_fill_brewer(palette = mainPal) +
  annotate("text",  x=Inf, y = Inf, label = "B", vjust=1, hjust=1, size = 5) +
  theme(legend.position = 'none')



# ggsave(filename = 'figures/hind_disttrav.png', device = 'png', width = 180, height = 90, units = 'mm', dpi = 500)


# Plot center gravity (final) per year

cgdata = bind_rows(spatdat, .id = "column_label")
init_dat = data.frame(cgx = unique(cgdata$CG_x_ini)[1], cgy = unique(cgdata$CG_y_ini)[1])
plot_data = aggregate(list(cgx = cgdata$CG_x_end, cgy = cgdata$CG_y_end), list(year = cgdata$year, scenario = cgdata$scenario), mean)
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

ps1 = plot_cg(plot_data, init_dat, mainPal)
ps1 = ps1 + annotate("text",  x=Inf, y = Inf, label = "A", vjust=1, hjust=1, size = 5)
# ggsave(filename = 'figures/hind_direction.png', device = 'png', width = 100, height = 70, units = 'mm', dpi = 500)


# Plot inertia per year:

cgdata = bind_rows(spatdat, .id = "column_label")
plot_data = cgdata
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$diffInertia = ((plot_data$I_end - plot_data$I_ini)/plot_data$I_ini)*100

# Plot:
ps3 = ggplot(plot_data, aes(x = year, y = diffInertia, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab('') +
  ylab('Difference in inertia (%)') +
  scale_color_brewer(palette = mainPal) +
  annotate("text",  x=Inf, y = Inf, label = "C", vjust=1, hjust=1, size = 5) +
  theme(legend.position = 'none')

# ggsave(filename = 'figures/hind_inertia.png', device = 'png', width = 100, height = 75, units = 'mm', dpi = 500)

png(filename = 'figures/fore_spatialind.png', width = 190, height = 60, 
    units = 'mm', res = 500)
grid.arrange(ps1, ps2, ps3, nrow = 1)
dev.off()


