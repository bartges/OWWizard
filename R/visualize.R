# visualize.R

plotWeibull <- function(A, k){
    df <- generateWeibullHours(A, k)
    
    p <- df %>%
        ggplot2::ggplot(aes(x = WS_ms, y = Hours)) +
        geom_line(color = "blue", size = 2) +
        xlab("Wind Speed [m/s]") +
        ylab("Duration [h]")
    p        
}

plotPowerCurve <- function(pc){
    p <- pc %>%
        ggplot2::ggplot(aes(x = WS_ms, y = Power_kW)) +
        geom_line(color = "steelblue", size = 2) +
        xlab("Wind Speed [m/s]") +
        ylab("Power [kW]")
    
    p
}