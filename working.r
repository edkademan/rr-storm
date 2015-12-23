library(dplyr)
library(ggplot2)
library(reshape2)

low <- function(x) casefold(as.character(x))

read_storm_data <- function() {
  validexp <- function(f) low(f) %in% c("k", "m", "b")
  scale_val <- function(v, s) {
    i <- v > 0
    v[i] <- v[i]*c(k = 1e3, m = 1e6, b = 1e9)[low(s[i])]
    v}
  readRDS("storm-data.rds") %>%
    filter(INJURIES > 0 | FATALITIES > 0 |
           (PROPDMG > 0 & validexp(PROPDMGEXP)) |
           (CROPDMG > 0 & validexp(CROPDMGEXP))) %>%
    mutate(PROPDMG = scale_val(PROPDMG, PROPDMGEXP),
           CROPDMG = scale_val(CROPDMG, CROPDMGEXP),
           Event   = simplify_events(EVTYPE),
           Decade  = noaa_date_to_decade(BGN_DATE))}

p <- function(d)
  d[,c("Event", "EVTYPE", "FATALITIES", "INJURIES",
       "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

## This consolidates the event descriptions and fixes spelling
## mistakes. It reduces the number of unique events from 483 to
## 61.
simplify_events <- function(events) {
  r <- as.character(events)
  g <- function(pat) grep(pat, r, ignore.case = TRUE)
  r[g("wind")] <- "wind"
  r[g("thunderstorm|tstm|lightning")] <- "thunderstorm"
  r[g("rain|shower|burst")] <- "rain"
  r[g("snow|wintry|winter|blizzard")] <- "snow"
  r[g("cold|freez|ice|sleet|glaze|low temper|icy|frost")] <- "cold"
  r[g("hurricane")] <- "hurricane"
  r[g("torn|funnel")] <- "tornado"
  r[g("hail")] <- "hail"
  r[g("flood")] <- "flood"
  r[g("tropical storm")] <- "tropical storm"
  r[g("heat|warm weather")] <- "heat"
  r[g("current|coast|seas|marine|surf|beach|wave|surge|swell")] <- "coastal/seas"
  r[g("fire")] <- "fire"
  r[g("slid|slump|avala")] <- "landslide"
  r[g("fog")] <- "fog"
  factor(low(r))}

injuries_plot <- function(d = read_storm_data()) {
  d <- filter(d, INJURIES > 0)
  ggplot(d) +
    geom_histogram(aes(log10(INJURIES)), bins = 30)
}

if_plot <- function(d = read_storm_data) {
  ggplot(d) +
    geom_point(aes(INJURIES, FATALITIES))
}

costly <- function(d = read_storm_data()) {
  r <- d %>%
    group_by(Event) %>%
    summarise(Total_Prop = sum(PROPDMG, na.rm = TRUE),
              Total_Crop = sum(CROPDMG, na.rm = TRUE)) %>%
    mutate(Total_Dollar = Total_Prop + Total_Crop) %>%
    arrange(desc(Total_Dollar))
  r$Cumu_Prop <- cumsum(r$Total_Dollar)/sum(r$Total_Dollar)
  r}

costly2 <- function(d = read_storm_data())
  d %>%
    group_by(Decade, Event) %>%
    summarise(Total_Prop = sum(PROPDMG, na.rm = TRUE),
              Total_Crop = sum(CROPDMG, na.rm = TRUE))

costly_plot <- function(d = read_storm_data(), prop = .999) {
  d <- within(filter(costly(d), Cumu_Prop <= prop),
              Event <- factor(Event, levels = rev(Event),
                               ordered = TRUE)) %>%
    filter(Cumu_Prop <= prop) %>%
    melt(measure.vars = c("Total_Prop", "Total_Crop"))
  ggplot(d) +
    geom_bar(aes(Event, value*1e-9, fill = variable),
             stat = "identity", position = "dodge") +
    ylab("billions of dollars") +
    coord_flip()}

costly2_plot <- function(d = read_storm_data(), n = 10) {
  ordered_event <- costly(d)$Event
  d <- costly2(d) %>%
    mutate(Event = factor(low(Event), levels = rev(ordered_event),
                          ordered = TRUE)) %>%
    filter(Event %in% ordered_event[seq(from = 1, to = n)]) %>%
    melt(measure.vars = c("Total_Prop", "Total_Crop"))
  ggplot(d) +
    geom_bar(aes(Event, value*1e-9, fill = variable),
             stat = "identity", position = "dodge") +
    ylab("billions of dollars") +
    coord_flip() +
    facet_wrap(~ Decade)}

noaa_date_to_decade <- function(x) {
  ## extract the 4-digit year
  x <- sub("\\d{1,2}/\\d{1,2}/(\\d{4}).*", "\\1", as.character(x))
  cut(as.numeric(x),
      breaks = seq(from = 1949, to = 2019, by = 10),
      labels = c("50s", "60s", "70s", "80s",
                 "90s", "00s", "10s"),
      ordered_result = TRUE)}

deadly <- function(d = read_storm_data()) {
  d %>%
    filter(INJURIES > 0 | FATALITIES > 0) %>%
    group_by(Event) %>%
    summarise(Injuries   = sum(INJURIES),
              Fatalities = sum(FATALITIES)) %>%
    arrange(desc(Injuries + Fatalities))}

deadly_plot <- function(d = read_storm_data(), n = 20) {
  d <- within(
    deadly(d),
    Event <- factor(Event, levels = rev(Event), ordered = TRUE))
  d <- melt(d[seq(from = 1, to = n),], id.vars = "Event")
  ggplot(d) +
    geom_bar(aes(Event, value), stat = "identity") +
    facet_wrap(~ variable) +
    coord_flip()}
