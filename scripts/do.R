library(plyr)
library(dplyr)
library(ggplot2)

df1 <- read.csv(file="./data/exceeds.csv")
df1$site <- factor(df1$site, labels=c("Ash Point", "Dirty Socks", "Flat Rock", 
                                      "Keeler", "Lone Pine", "Lizard Tail", 
                                      "Olancha", "Shell Cut"))

temp <- df1 %>% group_by(year) %>% filter(pm10>150) %>%
  summarize(count = length(pm10))
p1 <- df1 %>% filter(pm10>150) %>%
  ggplot(aes(year)) +
  geom_bar(aes(fill=site), color="black", width=.8) +
  ggtitle("Days per Year by Location Exceeding the PM10 Standard from Off-Lake Wind Directions") +
  ylab("Number of Days") + xlab("Year") +
  scale_fill_brewer(name="Monitor Location", palette="Set2") +
  scale_y_discrete(breaks=c(1:max(temp$count)), limits=c(1:max(temp$count))) +
  scale_x_continuous(breaks=c(min(temp$year):max(temp$year))) + 
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
  ggtitle(expression(atop("Days per Year on Which One or More Monitors Exceeded the PM10 Standard from Off-Lake Wind Directions", 
                          atop(italic("Bar Label = # of Stations Registering Exceedance That Day"),"")))) +
  ylab("Number of Days") + xlab("Year") +
  scale_fill_brewer(name="Monitor Location", palette="Reds") +
scale_y_discrete(breaks=c(1:maxx2), limits=c(1:maxx2)) +
#geom_smooth(data=temp2, aes(x=year, y=count), method="lm", se=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

png("./output/plot1.png", width=9, height=6, units="in", res=300)
dev.off()
png("./output/plot2.png", width=9.5, height=6, units="in", res=300)
dev.off()
