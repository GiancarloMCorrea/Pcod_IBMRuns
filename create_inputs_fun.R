# Function to create spawning locations (initial)

create_init_locations = function(ini_dates, Path) {

    library(maps)
    library(mapdata)
    require(chron)
    require(lubridate)
    require(ggplot2)
    
    # create input data frame  ------------------------------------------------------------------
    # spawning_times = strptime(x = c('2003-03-15 00:00:00', '2004-03-15 00:00:00',
    #                                 '2005-03-15 00:00:00', '2006-03-15 00:00:00',
    #                                 '2007-03-15 00:00:00', '2008-03-15 00:00:00',
    #                                 '2009-03-15 00:00:00', '2010-03-15 00:00:00',
    #                                 '2011-03-15 00:00:00', '2012-03-15 00:00:00'),
    #                           format = "%Y-%m-%d %H:%M:%S")
    spawning_times = strptime(x = paste0(ini_dates, ' 00:00:00'),
                              format = "%Y-%m-%d %H:%M:%S")
    spawning_times = format(spawning_times,"%Y-%m-%d %H:%M:%S")
    spawlocations = read.csv('main_files/SpawLocations2.csv')
    lon_vec = spawlocations$lon
    lat_vec = spawlocations$lat


    ini_dat = data.frame(life_stage = 'Eggs', ID = -1, parentID = -1, originalID = -1, 
                         start_time = rep(spawning_times, each = length(lon_vec)), time = -1,
                         horiz_pos_type = 2, vert_pos_type = 3,
                         horiz_pos_1 = rep(x = lon_vec, times = length(spawning_times)),
                         horiz_pos_2 = rep(x = lat_vec, times = length(spawning_times)), vert_pos = 0, 
                         bathy_dep = -1, grid_cell_id = '', track = '', active_status = FALSE, 
                         alive_status = TRUE, age = 0, age_in_stage = 0, n_ind = 8000000, 
                         attached = TRUE, 
                         stage_prog = 0, embryoSL = 0, embryoDW = 0.01, growthrateSL = 0, 
                         growthrateDW = 0, egg_density = 0, temp = -1, salinity = -1, insitu_den = -1)
    ini_dat = ini_dat[order(ini_dat$start_time), ]

    write.csv(ini_dat, file.path(Path, 'initAtts.AllSpawningAndPrespawningLocations_04.csv'), row.names = FALSE)

}