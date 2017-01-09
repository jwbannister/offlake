load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
load_all("~/code/Roses")
library(lubridate)
library(dplyr)
library(ggplot2)
library(rgdal)

dat <- mdy('11-16-2016')
mfile_sites <- c("LonePine", "Keeler", "NorthBch", "LizardTl", 
                    "MillSite", "ShellCut", "DirtySox", 
                    "Olancha", "Stanley")
query1 <- paste0("SELECT i.deployment, m.site, m.datetime, m.dir, m.aspd, ",
                 "m.teom, m.qaqc_level_id, ", 
                 "st_y(st_transform(i.geom, 26911)) AS y, ",
                 "st_x(st_transform(i.geom, 26911)) AS x ",
                 "FROM archive.mfile_data m JOIN instruments.deployments i ",
                 "ON m.deployment_id=i.deployment_id ",
                 "WHERE (m.datetime - ('1 second')::interval)::date='",
                 dat, "'::date AND m.site IN ('", 
                 paste0(mfile_sites, collapse="', '"), "')")
mfile_df <- query_owens(query1)
site_labels <- mfile_df %>% distinct(deployment, x, y)
  
daily_summary <- mfile_df %>% group_by(deployment) %>%
  summarize(daily.pm10=round(sum(teom, na.rm=T)/24, 0))
daily_summary$exceed <- sapply(daily_summary$daily.pm10, 
                             function(x) ifelse(x>150, T, F))

buffer <- 4000
p3 <- ggplot() +
  coord_equal() +
  geom_path(data=shoreline$polygons, mapping=aes(x=x, y=y, group=objectid)) +
  geom_point(data=site_labels, mapping=aes(x=x, y=y))
info <- ggplot_build(p3)
xrange <- info[[2]]$ranges[[1]]$x.range
yrange <- info[[2]]$ranges[[1]]$y.range
valueseq <- c(10, 50, 100, 150)
legend.plot <- mfile_df %>% filter(deployment==mfile_df$deployment[1]) %>%
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
