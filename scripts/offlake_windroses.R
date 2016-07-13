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
offshore_sites <- c("Lone Pine", "Keeler", "North Beach", "Lizard Tail", 
                    "Mill Site", "Flat Rock","Shell Cut", "Dirty Socks", 
                    "Olancha", "Stanley")
mfile_df <- filter(mfile_df, deployment %in% offshore_sites)
mfile_sum <- mfile_df %>% distinct(deployment, x, y)
p1 <- ggplot() +
  geom_path(data=shoreline_poly, mapping=aes(x=x, y=y)) +
  geom_point(data=mfile_sum, mapping=aes(x=x, y=y)) +
  geom_text(data=mfile_sum, mapping=aes(x=x, y=y, label=deployment)) +
  coord_equal()
  
mfile_df$date <- as.Date(mfile_df$datetime)
mfile_df <- mfile_df[complete.cases(mfile_df), ]
daily_summary <- mfile_df %>% group_by(date, deployment) %>%
  summarize(daily.pm10=round(sum(pm10.avg, na.rm=T)/24, 0))
daily_summary$flag <- sapply(daily_summary$daily.pm10, 
                             function(x) ifelse(x>150, "red", "white"))
violations <- mfile_df %>% group_by(date, deployment) %>%
  summarize(daily.pm10=round(sum(pm10.avg, na.rm=T)/24, 0)) %>% 
  filter(daily.pm10>150) 

buffer <- 4000
p3 <- ggplot(shoreline_poly, aes(x=x, y=y)) +
  coord_equal() +
  geom_path() +
  geom_point(data=mfile_sum, mapping=aes(x=x, y=y))
info <- ggplot_build(p3)
xrange <- info[[2]]$ranges[[1]]$x.range
yrange <- info[[2]]$ranges[[1]]$y.range
valueseq <- c(5, 10, 15, 20)
legend.plot <- mfile_df %>% filter(date==violations$date[1], 
                                   deployment==violations$deployment[1]) %>%
plot_rose(., value='ws', dir='wd', valueseq=valueseq,
          legend.title="PM10")
legnd <- g_legend(legend.plot)
p4 <- p3 + 
  xlim(xrange[1] - 1000, xrange[2] + 1000) +
  ylim(yrange[1] - 1000, yrange[2] + 1000) +
  annotation_custom(legnd,
                    xmin=xrange[2] - 4000, 
                    xmax=xrange[2], 
                    ymin=yrange[2] - 8000, 
                    ymax=yrange[2]) +
theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
      plot.title=element_text(size=12))

for (i in unique(as.character(violations$date))){
  p5 <- p4
  roses <- list(grobs=c(), centers=c())
  for (j in offshore_sites){
    if (nrow(filter(mfile_df, date==i, deployment==j))==0) next 
    p2 <- mfile_df %>% filter(date==i, deployment==j) %>%
      plot_rose_image_only(., value='ws', dir='wd', 
                           valueseq=valueseq)
    png(filename=paste0(tempdir(), "/p2.png"), bg="transparent")
    print(p2)
    dev.off()
    img <- png::readPNG(paste0(tempdir(), "/p2.png"))
    ras <- grid::rasterGrob(img, interpolate=TRUE)
    roses$grobs[[j]] <- ras
    roses$centers[[j]] <- c(filter(mfile_sum, deployment==j)$x, 
                            filter(mfile_sum, deployment==j)$y)
  }
  for (m in names(roses$grobs)){
    label_data <- daily_summary %>% 
      inner_join(select(mfile_sum, deployment, x, y), by="deployment") %>%
      filter(deployment==m, date==i)
    p5 <- p5 +
      annotation_custom(roses$grobs[[m]],
                        xmin=roses$centers[[m]][1] - buffer, 
                        xmax=roses$centers[[m]][1] + buffer, 
                        ymin=roses$centers[[m]][2] - buffer, 
                        ymax=roses$centers[[m]][2] + buffer) +
geom_label(data=label_data, mapping=aes(x=x, y=y, label=daily.pm10, 
                                        color=flag),
           nudge_x=1000, nudge_y=1000)
  }
  dcas <- dcm_polys %>% 
    inner_join(select(dcm_labels, dcm, objectid), by="objectid") %>%
    filter(dcm %in% c("T1A-4", "T32-2a", "T37-2b", "T1A-2a"))
  lbls <- filter(dcm_labels, dcm %in% c("T1A-4", "T32-2a", "T37-2b", "T1A-2a"))
  lbls[lbls$dcm=="T37-2b", ]$dcm <- "T37-2"
  p5 <- p5 + ggtitle(format(as.Date(i), "%m-%d-%Y")) +
    geom_polygon(data=dcas, 
                 mapping=aes(x=x, y=y, group=objectid), size=.2, 
                             fill="grey62", color="black") +
    geom_text(data=filter(lbls, dcm=="T37-2"), 
              mapping=aes(x=x, y=y, label=dcm), nudge_y=-2000, nudge_x=500) +
    geom_text(data=filter(lbls, dcm=="T1A-2a"), 
              mapping=aes(x=x, y=y, label=dcm), nudge_y=-1500, nudge_x=700) +
    geom_text(data=filter(lbls, dcm=="T1A-4"), 
              mapping=aes(x=x, y=y, label=dcm), nudge_y=-2300) 
  png(filename=paste0("~/dropbox/offshore_exceedences/",
                      format(as.Date(i), "%m-%d-%Y"), 
                      "_roses.png"), width=6, height=8, units="in", res=300)
  print(p5)
  dev.off()
} 
