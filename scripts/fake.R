library(plyr)
library(dplyr)
library(ggplot2)
set.seed(1)
# make fake test dataset
test_df <- data.frame(year=rep(NA, 100),
                      day=rep(NA, 100),
                      site.id=rep(NA, 100),
                      pm10=rep(NA, 100))
test_df$year <- sample(c(1999:2012), 100, replace=TRUE)
test_df$day <- sample(c(1:365), 100, replace=TRUE)
test_df$site.id <- sample(c("A", "B", "C", "D", "E"), 100, replace=TRUE)
test_df$pm10 <- sample(c(1:500), 100, replace=TRUE)
test_df$day <- factor(test_df$day)
temp <- test_df %>% group_by(year) %>% filter(pm10>150) %>%
  summarize(count = length(pm10))
p1 <- test_df %>% filter(pm10>150) %>%
  ggplot(aes(year)) +
  geom_bar(aes(fill=site.id)) +
  ggtitle("Exceedances (PM10 > 150) by Year") +
  scale_y_discrete(breaks=c(1:max(temp$count)), limits=c(1:max(temp$count))) +
  scale_x_continuous(breaks=c(min(temp$year):max(temp$year))) + 
  geom_smooth(data=temp, aes(x=year, y=count), method="lm", se=FALSE)

summary_df <- test_df %>% group_by(year, day) %>% 
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
temp2 <- test_df  %>% group_by(year) %>% 
  summarize(count = length(pm10))
p2 <- ggplot(summary_df, aes(x=year, y=temp)) +
  geom_bar(aes(fill=year_index), stat="identity") +
  geom_text(aes(label = count, y = pos), size = 3) +
  theme(legend.position='none') +
  scale_x_continuous(breaks=c(min(temp$year):max(temp$year))) + 
  ggtitle(expression(atop("Number of Exceedance Days per Year", 
                          atop(italic("Bar Label = # of Stations Registering Exceedance That Day"),"")))) +
  scale_y_discrete(breaks=c(1:maxx2), limits=c(1:maxx2)) +
  geom_smooth(data=temp2, aes(x=year, y=count), method="lm", se=FALSE)

