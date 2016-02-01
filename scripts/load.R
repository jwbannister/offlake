library(dplyr)
library(lubridate)
library(ggplot2)

site_list=c("AP", "DS", "FR", "KE", "LP", "LT", "MI", "NB", "OL", "SC")
df1 <- data.frame(site=c(), date=c(), wd=c(), teom=c(), wdf=c(), olteom=c()) 
for (i in site_list){
  temp <- read.csv(paste0("./data-raw/", i, "_all.csv"))
  temp$site <- i
  df1 <- rbind(df1, temp)
}
colnames(df1) <- tolower(colnames(df1))
df1$date <- as.POSIXct(df1$date, "%Y-%m-%d %H:%M")
df1$date <- df1$date - 30*60
df1$year <- year(df1$date)
df1$day <- yday(df1$date)
df1$hour <- hour(df1$date)

station_count <- df1 %>% group_by(year) %>% 
  summarize(count = length(unique(site)))
p1 <- ggplot(station_count, aes(x=year, y=count)) + 
  geom_bar(stat="identity")

hours_count <- df1 %>% group_by(year, day, site) %>%
  summarize(total.hours = length(hour))
max(hours_count$total.hours)
min(hours_count$total.hours)

daily_teom <- df1 %>% group_by(year, day, site) %>%
  summarize(pm10 = sum(olteom)/24)
exceed_df <- filter(daily_teom, pm10 > 150)

write.csv(daily_teom, file="./data/daily_pm10.csv")
write.csv(exceed_df, file="./data/exceed_pm10.csv")
