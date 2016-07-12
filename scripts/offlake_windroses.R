load_all()
load_all("~/analysis/Rowens")
load_all("~/analysis/windroseR")
library(lubridate)
library(dplyr)
library(ggplot2)
library(rgdal)

start_date <- mdy("01-01-2016")
end_date <- start_date %m+% months(6) %m-% days(1)

mfile_df <- pull_mfile_data(start_date, end_date)
offshore_sites <- c("Lone Pine", "Keeler", 
                    "North Beach", "Lizard Tail", "Mill Site", "Flat Rock",
                    "Shell Cut", "Dirty Socks", "Olancha", "Stanley")
mfile_df <- filter(mfile_df, deployment %in% offshore_sites)
mfile_sum <- mfile_df %>% distinct(deployment, x, y)
p1 <- ggplot() +
  geom_path(data=shoreline_poly, mapping=aes(x=x, y=y)) +
  geom_point(data=mfile_sum, mapping=aes(x=x, y=y)) +
  geom_text(data=mfile_sum, mapping=aes(x=x, y=y, label=deployment)) +
  coord_equal()
  
mfile_df$date <- as.Date(mfile_df$datetime)
mfile_df <- mfile_df[complete.cases(mfile_df), ]
violations <- mfile_df %>% group_by(date, deployment) %>%
  summarize(daily.pm10=sum(pm10.avg, na.rm=T)/24) %>% 
  filter(daily.pm10>150) 

roses <- vector(mode="list", length=length(unique(violations$date)))
names(roses) <- as.character(unique(violations$date))
for (i in as.character(unique(violations$date))){
  plots <- vector(mode="list", length=length(offshore_sites))
  names(plots) <- offshore_sites
  for (j in offshore_sites){
    if (nrow(filter(mfile_df, date==i, deployment==j))==0) next 
    plots[[j]] <- mfile_df %>% filter(date==i, deployment==j) %>%
      plot_rose_image_only(., value='ws', dir='wd', 
                           valueseq=c(5, 10, 15, 20))
  }
  roses[[i]] <- plots
}
