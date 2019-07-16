library(ggplot2)

elm <- `complete`

ggplot(elm[which(elm$V2>0),], aes(V3, V1)) + 
  geom_smooth(aes(color = V2)) 

ggplot(elm[which(elm$V2>0),], aes(V3, V1)) + 
  geom_col(aes(fill = V2))

ggplot(elm[which(elm$V2>0),], aes(V3, V1)) + 
  geom_point(aes(color = V2)) + 
#  geom_smooth(aes(color = V2)) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(log(x), bs = "cs")) 

ggplot(data = elm[which(elm$V2>0),], aes(x = V3, y = V1, color = V2, weight = V1 * V2)) + 
  geom_point() + 
  #  geom_smooth(aes(color = V2)) +
  geom_smooth(method="lm", se = FALSE, legend=FALSE) + 
  scale_color_gradient(low = "#004D40", high = "#FFC107")

attach(elm)
