library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(e1071)
library(latex2exp)
library(nlme)

file <- file.choose()
lincoln_daily <- read.csv(file, stringsAsFactors=FALSE)

head(lincoln_daily)
str(lincoln_daily)

date <- as.Date(lincoln_daily$date, format = "%m/%d/%y", origin = "01/01/10")
temp <- as.numeric(lincoln_daily$temp)
pm25 <- lincoln_daily$pm25
precip <- lincoln_daily$precip


april_2020 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/20", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/20", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2020[i] <- 1
  }
}

april_2019 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/19", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/19", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2019[i] <- 1
  }
}

april_2018 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/18", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/18", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2018[i] <- 1
  }
}

april_2017 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/17", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/17", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2017[i] <- 1
  }
}

april_2016 <- array(0,dim = c(nrow(lincoln_daily)))
for (i in 1:(nrow(lincoln_daily))) {
  if ((date[i] > as.Date("03/31/16", format = "%m/%d/%y", origin = "01/01/10")) & (date[i] < as.Date("05/01/16", format = "%m/%d/%y", origin = "01/01/10"))) {
    april_2016[i] <- 1
  }
}

lincoln_daily <- data.frame(date, pm25, precip, temp, april_2020, april_2019, april_2018, april_2017, april_2016)
lincoln_daily$month <- format(lincoln_daily$date, '%m')
pm25_april <- lincoln_daily[which(lincoln_daily$month == "04"), names(lincoln_daily) %in% c("date", "pm25", "temp", "precip", "april_2020", "april_2019", "april_2018", "april_2017", "april_2016")]

#checking for normal distribution
hist(pm25_april$pm25)
# hist(1/pm25_april$pm25)
# mean(pm25_april$pm25, na.rm = TRUE)
# 1/0.078533
# 1/(0.078533 + 0.043025)
# (12.7335-8.226526)/12.7335

# fixing zero values
j <- 0
for (i in 1:nrow(lincoln_daily)) {
  if (is.na(lincoln_daily$pm25[i]) == FALSE) {
    if (lincoln_daily$pm25[i] == 0) {
      lincoln_daily$pm25[i] <- NA
      # lincoln_daily$pm25log[i] <- NA
      j <- j + 1
    }
  }
}


#glm april 2020 v. all previous months
model5=glm(pm25~april_2020 + precip, data = lincoln_daily, family = Gamma(link = "log"))
summary(model5)
confint(model5)

#glm april 2020 v. previous aprils
model6=glm(pm25~april_2020 + precip, data = pm25_april, family = Gamma(link = "log"))
summary(model6)
confint(model6)

#glm previous aprils with indicators
model7=glm(pm25~april_2019 + april_2018 + april_2017 + april_2016 + precip, data = pm25_april, family = Gamma(link = "log"))
summary(model7)
confint(model7)

# lm_pm25 <- array(0, dim = c(nrow(pm25_april)))
# for (i in 1:nrow(pm25_april)) {
#   lm_pm25[i] = 12.3680 - 3.7896*pm25_april$april_2020[i] - 6.9520*pm25_april$precip[i] }

r_gamma <- residuals(model7)
hist(r_gamma)
plot(r_gamma)

# r2 <- lm_pm25 - pm25_april$pm25
# plot(r,r2[1:148], type = "p")
# 
# #visual
# ggplot() +
#   geom_point(data = lincoln_daily, mapping = aes(x = date, y = pm25))
