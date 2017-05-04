library(ggplot2)
library(gganimate)

#animate real total GDP, measured in 100 million yuan;
#calculate real GDP( GDP deflator use 2000 as the base year)
rgdptota<- total$gdptota/total$gdpdef
pa<- ggplot(data, aes(rgdptota, ncampus, size = poptota, frame = year)) +
  geom_point()+geom_smooth(aes(group = year), method = "lm", show.legend = FALSE) +
  facet_wrap(~region, scales = "free")
gganimate(pa)


rgdptotc<- total$gdptotc/total$gdpdef
pc<- ggplot(total, aes(rgdptotc, ncampus, size = poptotc, frame = year)) +
  geom_point()+geom_smooth(aes(group = year), method = "lm", show.legend = FALSE) +
  facet_wrap(~region, scales = "free")
gganimate(pc)