library(plyr)
library(dplyr)
library(ggplot2)

df1 <- read.csv(file="./data/exceed_pm10.csv")
df1$site <- factor(df1$site, levels=c("AP", "DS", "FR", "KE", "LP", "LT",
                                      "MI", "NB", "OL", "SC"), 
                   labels=c("Ash Point", "Dirty Socks", "Flat Rock", 
                            "Keeler", "Lone Pine", "Lizard Tail", 
                            "Mill Site", "North Beach", "Olancha", 
                            "Shell Cut"))

temp <- df1 %>% group_by(year) %>% 
  summarize(count = length(pm10))
p1 <- df1 %>% 
  ggplot(aes(year)) +
  geom_bar(aes(fill=site), color="black", width=.8) +
  ggtitle("Days per Year by Location Exceeding the PM10 Standard from Off-Lake Wind Directions") +
  ylab("Number of Days") + xlab("Year") +
  scale_fill_brewer(name="Monitor Location", palette="Set3") +
  scale_y_discrete(breaks=c(1:max(temp$count)), limits=c(1:max(temp$count))) +
  scale_x_continuous(breaks=c(min(temp$year):max(temp$year))) + 
  coord_cartesian(xlim=c(min(temp$year), max(temp$year))) +
  #  geom_smooth(data=temp, aes(x=year, y=count), method="lm", se=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary_df <- df1 %>% group_by(year, day) %>% 
  summarize(count = length(pm10), temp=1)
for (i in 1:nrow(summary_df)){
  summary_df$year_index[i] <- match(summary_df$day[i],
                                    filter(summary_df, year==summary_df$year[i])$day)
}
summary_df$year_index <- factor(summary_df$year_index)
maxx2 <- as.numeric(levels(summary_df$year_index)[length(levels(summary_df$year_index))])
# calculate midpoints of bars (simplified using comment by @DWin)
summary_df <- ddply(summary_df, .(year), 
                    transform, pos = cumsum(temp) - (0.5 * temp)
                    )
temp2 <- df1  %>% group_by(year) %>% 
  summarize(count = length(pm10))
p2 <- ggplot(summary_df, aes(x=year, y=temp)) +
  geom_bar(aes(fill=factor(count)), color="black", stat="identity") +
  geom_text(aes(label = count, y = pos), size = 3) +
  theme(legend.position='none') +
  scale_x_continuous(breaks=c(min(temp$year):max(temp$year))) +
  coord_cartesian(xlim=c(min(temp$year), max(temp$year))) +
  ggtitle(expression(atop("Days per Year on Which One or More Monitors Exceeded the PM10 Standard from Off-Lake Wind Directions", 
                          atop(italic("Bar Label = # of Stations Registering Exceedance That Day"),"")))) +
ylab("Number of Days") + xlab("Year") +
scale_fill_brewer(name="Monitor Location", palette="Reds") +
scale_y_discrete(breaks=c(1:maxx2), limits=c(1:maxx2)) +
#geom_smooth(data=temp2, aes(x=year, y=count), method="lm", se=FALSE) +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("plot1.png", plot=p1, path="./output/", width=9, height=6,
       units="in", dpi=300)
ggsave("plot2.png", plot=p2, path="./output/", width=9.5, height=6,
       units="in", dpi=300)

# evertthing below here is an alternate of plot 2, showing site labels in lieu 
# of count for number of sites showing exceedance
df1_alt <- df1
df1_alt$site <- factor(df1_alt$site, 
                   levels=c("Ash Point", "Dirty Socks", "Flat Rock", 
                            "Keeler", "Lone Pine", "Lizard Tail", 
                            "Mill Site", "North Beach", "Olancha", 
                            "Shell Cut"),
                           labels=c("AP", "DS", "FR", "KE", "LP", "LT", 
                                    "MI", "NB", "OL", "SC"))
summary_alt <- df1_alt %>% group_by(year, day) %>% 
  summarize(count = length(pm10), temp=1,
            sites = paste(unique(site), collapse=","))
for (i in 1:nrow(summary_alt)){
  summary_alt$year_index[i] <- match(summary_alt$day[i],
                                    filter(summary_alt, year==summary_alt$year[i])$day)
}
summary_alt$year_index <- factor(summary_alt$year_index)
maxx2 <- as.numeric(levels(summary_alt$year_index)[length(levels(summary_alt$year_index))])
# calculate midpoints of bars (simplified using comment by @DWin)
summary_alt <- ddply(summary_alt, .(year), 
                    transform, pos = cumsum(temp) - (0.5 * temp)
                    )
for (i in 1:nrow(summary_alt)){
  summary_alt$temp2[i] <- ifelse(summary_alt$count[i] > 2,
                                 summary_alt$count[i],
                                summary_alt$sites[i]) 
}

p2_alt <- ggplot(summary_alt, aes(x=year, y=temp)) +
  geom_bar(aes(fill=factor(count)), color="black", stat="identity") +
  geom_text(aes(label = temp2, y = pos), size = 2) +
  theme(legend.position='none') +
  scale_x_continuous(breaks=c(min(temp$year):max(temp$year))) +
  coord_cartesian(xlim=c(min(temp$year), max(temp$year))) +
  ggtitle(expression(atop("Days per Year on Which One or More Monitors Exceeded the PM10 Standard from Off-Lake Wind Directions", 
                          atop(italic("Bar Label = Names or # (if >2) of Stations Registering Exceedance That Day"),"")))) +
ylab("Number of Days") + xlab("Year") +
scale_fill_brewer(name="Monitor Location", palette="Reds") +
scale_y_discrete(breaks=c(1:maxx2), limits=c(1:maxx2)) +
#geom_smooth(data=temp2, aes(x=year, y=count), method="lm", se=FALSE) +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("plot2_alt.png", plot=p2_alt, path="./output/", width=9.5, height=6,
       units="in", dpi=300)
