library(readxl)
library(dplyr)
source("gg_rink.R")

df = read_xlsx("data/shot_datasæt.xlsx")

table(df$Spilsituation)
dfskud = as.data.frame(table(df$Skudtype))
dfoutc = as.data.frame(table(df$Skud_outcome))
dfoutm = as.data.frame(table(df$outcome))
dfzone = as.data.frame(table(df$Placering_af_skytte))
dfzoneC = df %>% filter(Placering_af_skytte=="17")
dfzoneB = df %>% filter(Placering_af_skytte=="15")
dfzoneD = df %>% filter(Placering_af_skytte=="14")
dfang = as.data.frame(table(df$angle.to.goal))

df$outcome = ifelse(df$Skud_outcome!="Mål",0,1)


hist(df$distance_to_goal_line)

ggplot(dfskud, aes(x=Var1,y=Freq))+geom_bar(stat="identity")
ggplot(df, aes(x=outcome, fill=as.factor(outcome)))+geom_bar()
ggplot(dfzone, aes(x=Var1,y=Freq))+geom_bar(stat="identity")
ggplot(dfoutc, aes(x=Var1,y=Freq))+geom_bar(stat="identity")
ggplot(df, aes(x=as.factor(Skudtype), fill=as.factor(outcome)))+geom_bar(position = "dodge")
ggplot(df, aes(x=as.factor(Placering_af_skytte), fill=as.factor(outcome)))+geom_bar(position = "dodge")

allsh = data.frame(df$outcome,df$Lokation_på_skytte_x_koordinat,df$Lokation_på_skytte_y_koordinat)
allshc = allsh %>% filter(allsh$df.outcome==1)
allshnc = allsh %>% filter(allsh$df.outcome==0)
colnames(allsh)=c("mål","x","y")

hist(allsh$x)
hist(allsh$y)
hist(df$distance_to_goal_line)
hist(df$angle.to.goal, breaks=40)

ggplot(data=allsh,aes(x=x-30,y=y-15, fill=mål))+
  gg_rink(specs = "iihf")+
  geom_point(alpha = 0.7, size = 4) +
  scale_color_gradient(low = "blue", high = "red") +  # You can choose your colors
  labs(title = "Frequency of Goals by Zone", x = "X Coordinate", y = "Y Coordinate", color = "Goal Count") +
  theme_minimal()
  

ggplot(data=allsh,aes(x=x-30,y=y-15, fill=mål))+
  gg_rink(specs = "iihf")+
  geom_point(alpha = 0.7, size = 4)
  

onesh = allsh[1]

# beregn skudsandsynlighed for hver zone
# dvs FSh%=goals / antal unblocked shots

dfg <- df %>% filter(outcome==1)

dfzone2 <- df %>% group_by(Placering_af_skytte) %>% summarise(goalprz = sum(outcome)/n())


ggplot(dfzone2)+gg_rink()


giveangle <- function(x,y) {
  # midpoint
  a=30-4
  c=30-4
  b=1.835/2
  d=-1.835/2
  v <- (a + c) / 2
  w <- (b + d) / 2
  vector1 <- c(v - x, w - y)
  vector2 <- c(c - a, d - b)
  
  # Calculate the dot product and norms
  dot_product <- sum(vector1 * vector2)
  norm1 <- sqrt(sum(vector1^2))
  norm2 <- sqrt(sum(vector2^2))
  
  # Calculate the angle in radians
  angle_radians <- acos(dot_product / (norm1 * norm2))
  
  # Convert to degrees if needed
  angle_degrees <- angle_radians * (180 / pi)
  return(angle_degrees)
  
}
