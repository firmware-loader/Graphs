library(ggplot2)
library(plotly)
library(gapminder)
library(ggridges)

timing <- `complete`
timer <- `complete_timer`

timing$V2 <- timing$V2 * 10
timer$V2 <- timer$V2 * 10

cleaned_timer <- timer %>%
  filter(V2==100)


ggplot(timing, aes(V3, V1)) + 
  geom_col(aes(fill = V2))



ggplot(timing[which(timing$V2==100),], aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Frequenz") + 
  ylab("Baud")

ggplot(data = timing[which(timing$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, legend=FALSE) + 
  scale_color_gradient() + 
  xlab("Frequenz [Mhz]") + 
  ylab("Baud")

ggplot(data = timing[which(timing$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, legend=FALSE) + 
  scale_color_gradient() + 
  xlab("Frequenz [Mhz]") + 
  ylab("Baud")

ggplot(cleaned_timer, aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Frequenz") + 
  ylab("Baud")

ggplot(cleaned_timer, aes(x=as.factor(V3), y=V1, color = V2)) + 
  geom_point()

ggplot(data = timer[which(timer$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  geom_smooth(method="auto", se = FALSE, legend=FALSE) + 
  scale_color_gradient() + 
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [Baud]") + 
  labs(title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten", color = "Erfolgreiche\n‹bertragungen [%]")

#ignore "random match" (extl. 1 match, because it could be coincidence)

attach(timing)
attach(timer)
