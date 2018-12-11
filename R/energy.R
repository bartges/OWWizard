# statistic.R

generateWeibullHours <- function(A, k){
    # takes weibull shape and scale parameters as input
    # returns df of cumulative hours at 0.5 m/s wind speed bins for 1 year
    df <- data.frame(WS_ms = seq(0, 30, 0.5)) %>%
        dplyr::mutate(Hours = 8760 * (
            pweibull(WS_ms, shape = k, 
                     scale = A, 
                     lower.tail = FALSE) - 
                pweibull(WS_ms + 0.5, 
                         shape = k, 
                         scale = A, 
                         lower.tail = FALSE)
        ))
}

loadPCs <- function(filename){
    # reads *.csv of power curves (should be replaced by database, e.g. sql)
    # returns df of power curves
    df <- readr::read_csv(filename) %>%
        tidyr::gather(key = "RotorDiameter_m", value = "Power_kW", -(WS_ms)) %>%
        dplyr::mutate(RotorDiameter_m = as.numeric(stringr::str_extract(
            RotorDiameter_m, "[0-9]+")
        ))
    
    return(df)
}

calcEnergy <- function(pc, weib){
    # takes pc and weibull dfs
    # returns merged df with energy values at each wind speed bin in MWh
    df <- pc %>%
        dplyr::left_join(weib, by = "WS_ms") %>%
        dplyr::mutate(Energy_MWh = (Power_kW * Hours) / 1E3)
    
    return(df)
}