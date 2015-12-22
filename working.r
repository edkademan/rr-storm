read_storm_data <- function() {
  r <- readRDS("storm-data.rds")
  r[,c("EVTYPE", "BGN_DATE", "END_DATE", "INJURIES", "FATALITIES",
       "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]     
}

injuries_plot <- function(d = read_storm_data()) {
  d <- filter(d, INJURIES > 0)
  ggplot(d) +
    geom_histogram(aes(log10(INJURIES)), bins = 30)
}

if_plot <- function(d = read_storm_data) {
  ggplot(d) +
    geom_point(aes(INJURIES, FATALITIES))
}