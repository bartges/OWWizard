require(rgdal)
require(tidyverse)

# *.gdb file names
fgdb_gulf <- "data/NREL_HourlyWind_Gulf_2017_01_22.gdb"
fgdb_atlantic <- "data/NREL_HourlyWind_Atlantic_2017_01_22.gdb"
fgdb_pacific <- "data/NREL_HourlyWind_Pacific_2017_01_22.gdb"

loadFgdb <- function(filename, layer, showTags = FALSE) {
# read in *.gdb file
# aggregate by 0.1 degree resolution to improve performance
# transform from "wide" to "long" to faciliate filtering by region and time
# return df
    
    # print tags to screen for exploratory to expedite exploratory analysis
    if (showTags) {
        subset(ogrDrivers(), grepl("GDB", name))
        fc_list <- ogrListLayers(filename)
        print(fc_list)
    }
    
    # read in *.gdb data as an S4 object
    fc <- rgdal::readOGR(dsn = filename,
                         layer = layer)
    
    # aggregate data to 0.1 lat/long resolution to reduce load
    # e.g. results in a ~80% reduction in data points for the "gulf" data
    message(paste0("Munging ", filename, " - ", layer, "..."))
    df <- fc %>%
        as.data.frame() %>% # convert to df to enable dplyr grammar
        dplyr::mutate(latApprox = round(Latitude, 1),
                      longApprox = round(Longitude, 1)) %>%
        dplyr::group_by(latitude = latApprox,
                        longitude = longApprox) %>%
        dplyr::summarize(WS = mean(WS),
                         WK = mean(WK),
                         WC = mean(WC),
                         H00_WS = mean(H00_WS),
                         H00_WK = mean(H00_WK),
                         H00_WC = mean(H00_WC),
                         H01_WS = mean(H01_WS),
                         H01_WK = mean(H01_WK),
                         H01_WC = mean(H01_WC),
                         H02_WS = mean(H02_WS),
                         H02_WK = mean(H02_WK),
                         H02_WC = mean(H02_WC),
                         H03_WS = mean(H03_WS),
                         H03_WK = mean(H03_WK),
                         H03_WC = mean(H03_WC),
                         H04_WS = mean(H04_WS),
                         H04_WK = mean(H04_WK),
                         H04_WC = mean(H04_WC),
                         H05_WS = mean(H06_WS),
                         H05_WK = mean(H05_WK),
                         H05_WC = mean(H05_WC),
                         H06_WS = mean(H06_WS),
                         H06_WK = mean(H06_WK),
                         H06_WC = mean(H06_WC),
                         H07_WS = mean(H07_WS),
                         H07_WK = mean(H07_WK),
                         H07_WC = mean(H07_WC),
                         H08_WS = mean(H08_WS),
                         H08_WK = mean(H08_WK),
                         H08_WC = mean(H08_WC),
                         H09_WS = mean(H09_WS),
                         H09_WK = mean(H09_WK),
                         H09_WC = mean(H09_WC),
                         H10_WS = mean(H10_WS),
                         H10_WK = mean(H10_WK),
                         H10_WC = mean(H10_WC),
                         H11_WS = mean(H11_WS),
                         H11_WK = mean(H11_WK),
                         H11_WC = mean(H11_WC),
                         H12_WS = mean(H12_WS),
                         H12_WK = mean(H12_WK),
                         H12_WC = mean(H12_WC),
                         H13_WS = mean(H13_WS),
                         H13_WK = mean(H13_WK),
                         H13_WC = mean(H13_WC),
                         H14_WS = mean(H14_WS),
                         H14_WK = mean(H14_WK),
                         H14_WC = mean(H14_WC),
                         H15_WS = mean(H15_WS),
                         H15_WK = mean(H15_WK),
                         H15_WC = mean(H15_WC),
                         H16_WS = mean(H16_WS),
                         H16_WK = mean(H16_WK),
                         H16_WC = mean(H16_WC),
                         H17_WS = mean(H17_WS),
                         H17_WK = mean(H17_WK),
                         H17_WC = mean(H17_WC),
                         H18_WS = mean(H18_WS),
                         H18_WK = mean(H18_WK),
                         H18_WC = mean(H18_WC),
                         H19_WS = mean(H19_WS),
                         H19_WK = mean(H19_WK),
                         H19_WC = mean(H19_WC),
                         H20_WS = mean(H20_WS),
                         H20_WK = mean(H20_WK),
                         H20_WC = mean(H20_WC),
                         H21_WS = mean(H21_WS),
                         H21_WK = mean(H21_WK),
                         H21_WC = mean(H21_WC),
                         H22_WS = mean(H22_WS),
                         H22_WK = mean(H22_WK),
                         H22_WC = mean(H22_WC),
                         H23_WS = mean(H23_WS),
                         H23_WK = mean(H23_WK),
                         H23_WC = mean(H23_WC)
        ) %>%
        # convert to long format to enable filtering by time and region
        dplyr::mutate(region = layer) %>%
        tidyr::gather(key = "layer", value = "value", -c(region, latitude, longitude)) %>%
        dplyr::mutate(period = stringr::str_extract(layer, "[0-9]+"),
                      layer = stringr::str_extract(layer, "[W][A-Z]")) %>%
        dplyr::select(region, latitude, longitude, period, layer, value) %>%
        dplyr::mutate(period = replace(period, is.na(period), "annual"))
    
    return(df)
}

loadAllGdbDumpRds <- function(timePeriod){
    # load all regions for a given time period, e.g. "annual" or "may"
    # save as native R binary format for efficiency (*.Rds)
    message("Reading Gulf data...")
    gulf <- loadFgdb(fgdb_gulf, paste0(timePeriod, "_gulf"))
    
    message("Reading Atlantic data...")
    atlantic <- loadFgdb(fgdb_atlantic, paste0(timePeriod, "_atlantic"))
    
    message("Reading Pacific data...")
    pacific <- loadFgdb(fgdb_pacific, paste0(timePeriod, "_pacific"))
    
    # merge all to single df
    df <- rbind(gulf, atlantic) %>%
        rbind(pacific)

    message("Dumping data to *.Rds...")
    if (!file.exists("processedData")) {
        dir.create("processedData")
    }
    saveRDS(df, paste0("processedData/hourlyData_", timePeriod, ".Rds"))    
}