library(ggplot2)
source("gg_rink.R")

x=seq(45,60,1)
y=seq(10,25,1)

allsh22=data.frame(x,y)
ggplot(data=allsh22,aes(x=x-30,y=y-15))+
  gg_rink(specs = "iihf")+
  geom_point(alpha = 0.7, size = 4)



