#Day2
library(tidyverse)
library(boot)

camera <- read_csv("camera_dataset.csv")
colnames(camera)

# predict probability of failure give the read error rate
model <- glm(`Effective pixels` ~ Price, data = camera, family = "poisson")

glm.diag.plots(model)

summary(model)

ggplot(camera, aes(x = Price, y = `Effective pixels`)) + 
  geom_point()+
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"))

#Price has reduced the deviance residuals from 1967 to 1912 and the estimate although to be low has low standard deviation



