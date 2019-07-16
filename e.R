library(ggplot2)
library(plotly)
library(gapminder)
library(ggridges)

timing <- `complete`
timer <- `complete_timer`


ggplot(timing, aes(V3, V1)) + 
  geom_col(aes(fill = V2))



ggplot(timing[which(timing$V2==10),], aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Frequenz") + 
  ylab("Baud")

ggplot(data = timing[which(timing$V2>0),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, legend=FALSE) + 
  scale_color_gradient() + 
  xlab("Frequenz [Mhz]") + 
  ylab("Baud")

ggplot(data = timing[which(timing$V2>0),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, legend=FALSE) + 
  scale_color_gradient() + 
  xlab("Frequenz [Mhz]") + 
  ylab("Baud")

ggplot(timer[which(timer$V2==10),], aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Frequenz") + 
  ylab("Baud")

ggplot(data = timer[which(timer$V2>0),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  geom_smooth(method="auto", se = FALSE, legend=FALSE) + 
  scale_color_gradient() + 
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [Baud]") + 
  labs(title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten", color = "Erfolgreiche\n‹bertragungen [%]")

attach(timing)
attach(timer)
