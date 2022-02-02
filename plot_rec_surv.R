rm(list = ls())
# ROMS example:
# Explore ROMS outputs:

setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')

library(ggplot2)
library(dplyr)

main_data = read.csv('Compare_Recs_Surv.csv')
new_data = spread(data = main_data, key = type, value = value)

png(filename = 'figures/recr_surv_comp.png', width = 190, height = 120, units = 'mm', res = 500)

par(mar = c(3,4,1,4.5), xaxs = 'i')
plot(new_data$year, new_data$recruitment/1e+6, col = 'black', type = 'l', axes = FALSE,
     ylab = 'Recruitment (age-0, billion of fish)', xlab = '', xlim = c(2006, 2020))
axis(side = 1)
axis(2)
par(new = TRUE)
plot(new_data$year, new_data$GFDL_rcp85, col = 'red', type = 'l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", ylim = c(0.05, 0.17), xlim = c(2006, 2020))
lines(new_data$year, new_data$GFDL_rcp45, col = 'blue')
axis(side = 4)
mtext("Survival probability (30 dph)", side = 4, line = 3)

dev.off()

# par(mar = c(3,4,1,4.5), xaxs = 'i')
# plot(new_data$year, new_data$age_comps, col = 'black', type = 'l', axes = FALSE,
#      ylab = 'Recruitment (billion of fish)', xlab = '', xlim = c(2006, 2020))
# axis(side = 1)
# axis(2)
# par(new = TRUE)
# plot(new_data$year, new_data$GFDL_rcp85, col = 'red', type = 'l', xaxt = "n", yaxt = "n",
#      ylab = "", xlab = "", ylim = c(0.05, 0.17), xlim = c(2006, 2020))
# lines(new_data$year, new_data$GFDL_rcp45, col = 'blue')
# axis(side = 4)
# mtext("Survival probability (30 dph)", side = 4, line = 3)
