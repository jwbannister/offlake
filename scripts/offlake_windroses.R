load_all()
load_all("~/code/owensData")
load_all("~/analysis/Rowens")
load_all("~/analysis/windroseR")
library(lubridate)
library(dplyr)
library(ggplot2)
library(rgdal)

mfile_sites <- c("LonePine", "Keeler", "NorthBch", "LizardTl", 
                    "MillSite", "ShellCut", "DirtySox", 
                    "Olancha", "Stanley")
mfile_df <- query_owenslake(paste0("SELECT site, datetime, dir, 
                         aspd, teom, qaqc_level_id  
                         FROM archive.mfile_data 
                         WHERE datetime BETWEEN '2016-10-16 01:00:00' 
                         AND '2016-10-17 00:00:00';"))
mfile_df <- filter(mfile_df, site %in% mfile_sites)

instrument_deployments <- c("Lone Pine", "Keeler", "North Beach", "Lizard Tail", 
                    "Mill Site", "Shell Cut", "Dirty Socks", 
                    "Olancha", "Stanley")
deploy_df <- query_owenslake(paste0("SELECT deployment, easting_utm, 
                                   northing_utm
                         FROM instruments.deployments;"))
deploy_df <- filter(deploy_df, deployment %in% instrument_deployments)                         
deploy_df$deployment <- factor(deploy_df$deployment, levels=instrument_deployments, 
                               labels=mfile_sites)
mfile_df <- left_join(mfile_df, deploy_df, by=c("site"="deployment"))
names(mfile_df)[1] <- "deployment"
names(mfile_df)[7:8] <- c("x", "y")

mfile_sum <- mfile_df %>% distinct(deployment, x, y)
p1 <- ggplot() +
  geom_path(data=shoreline_poly, mapping=aes(x=x, y=y)) +
  geom_point(data=mfile_sum, mapping=aes(x=x, y=y)) +
  geom_text(data=mfile_sum, mapping=aes(x=x, y=y, label=deployment)) +
  coord_equal()
  
mfile_df$date <- '2016-10-16'
daily_summary <- mfile_df %>% group_by(date, deployment) %>%
  summarize(daily.pm10=round(sum(teom, na.rm=T)/24, 0))
daily_summary$flag <- sapply(daily_summary$daily.pm10, 
                             function(x) ifelse(x>150, "red", "white"))
violations <- mfile_df %>% group_by(date, deployment) %>%
  summarize(daily.pm10=round(sum(teom, na.rm=T)/24, 0)) %>% 
  filter(daily.pm10>150) 

buffer <- 4000
p3 <- ggplot(shoreline_poly, aes(x=x, y=y)) +
  coord_equal() +
  geom_path() +
  geom_point(data=mfile_sum, mapping=aes(x=x, y=y))
info <- ggplot_build(p3)
xrange <- info[[2]]$ranges[[1]]$x.range
yrange <- info[[2]]$ranges[[1]]$y.range
valueseq <- c(10, 50, 100, 150)
legend.plot <- mfile_df %>% filter(date==violations$date[1], 
                                   deployment==violations$deployment[1]) %>%
plot_rose(., value='teom', dir='dir', valueseq=valueseq,
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
  for (j in mfile_sites){
    if (nrow(filter(mfile_df, date==i, deployment==j))==0) next 
    p2 <- mfile_df %>% filter(date==i, deployment==j) %>%
      plot_rose_image_only(., value='teom', dir='dir', 
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
           nudge_x=1000, nudge_y=1000) +
ggtitle("Hourly PM10 Roses for Oct 16, 2016\n(Site Label = 24-hour Average PM10)")
  }
  png(filename=paste0("~/Desktop/", 
                      format(as.Date(i), "%m-%d-%Y"), 
                      "_roses.png"), width=6, height=8, units="in", res=300)
  print(p5)
  dev.off()
} 

mfile_print <- mfile_df %>% arrange(deployment, datetime)
write.csv(mfile_print, file="~/Desktop/csv_10162016.csv")
