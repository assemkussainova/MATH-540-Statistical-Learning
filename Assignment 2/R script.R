#load preprocessed hsb2 dataset from SPSS file
library(foreign)
f <- read.spss("R.sav", to.data.frame = TRUE)
names(f)

#check for normality
qqnorm(f$read, pch = 1, frame = FALSE)
qqline(f$read, col = "steelblue", lwd = 2)

qqnorm(f$math, pch = 1, frame = FALSE)
qqline(f$math, col = "steelblue", lwd = 2)

qqnorm(f$science, pch = 1, frame = FALSE)
qqline(f$science, col = "steelblue", lwd = 2)

qqnorm(f$socst, pch = 1, frame = FALSE)
qqline(f$socst, col = "steelblue", lwd = 2)

#build regression model
glm.fit1 <- lm(write ~ female + read + math + science + socst, data = f)
summary(glm.fit1)

#check for multicollinearity
library(car)
vif(glm.fit1)

predict(glm.fit1, data.frame(female=0, read=49, math=45, science=57, socst=52), type="response")
