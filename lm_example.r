library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(e1071)
library(latex2exp)
library(nlme)
library(lubridate)

# file <- file.choose()
lincoln_daily <- read.csv("lincoln_pm25_daily.csv", stringsAsFactors=FALSE)
lincoln_daily <- rename(lincoln_daily, date = ï..date) # This appears to be needed for Windows... 

head(lincoln_daily)
str(lincoln_daily)

date <- as.Date(lincoln_daily$date, format = "%m/%d/%y", origin = "01/01/10")
temp <- as.numeric(lincoln_daily$temp)
pm25 <- lincoln_daily$pm25
precip <- lincoln_daily$precip

# k <- 0.1 # weighting factor, must be less than unity
# d <- 3 # days
# n <- length(temp) # enter daily precip here, may need to use nrow
# API <- array(0, dim = c(n))
# for (i in 1:(d-1)) {
#   API[i] <- temp[i] # this fills the days prior to the start with the current precip, not an API
# }
# for (i in d:n) {
#   for (j in 1:d) {
#     t <- j-1
#     API[i] <- API[i]+temp[i-t]*(k^t)
#   }
# }

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

#checking for normal distribution
hist(pm25_april$pm25)
# n = length(pm25_april$pm25)
# h = hist(pm25_april$pm25, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40))
# b = h$counts
# k = length(h$breaks)-1 # number of intervals
# x1 = h$breaks
# m = mean(pm25_april$pm25, na.rm = TRUE)
# s = sd(pm25_april$pm25, na.rm = TRUE)
# 
# # Assemble table of X_0^2
# v = array(0, dim = c(k))             # Array for the normalized value
# p = v                                # Array for the F(x) or CDF of the normalized variable, v
# e = v                                # Variable, e_j = n * p_j, where n is the number of variables
# for (j in 1:k) {
#   v[j] = (x1[j+1] - m)/s         # Normalized variable
#   p[j] = pnorm(v[j])             # F(x_j)
#   if (j==k) {
#     p[j] = 1
#   }
#   e[j] = n*p[j]
#   if (j>1) {
#     e[j] = e[j] - (n*p[j-1]) # adjustment for integral of p(j) from v(j-1) to v(j)
#   }
# }
# X_0 = sum(((b-e)^2)/e)               # = 2.809 Summation.  Equation (1) from section 23.7 box.
# #k - r - 1 #k = number of intervals, r = 2 (5 - 2 - 1 = 2)


#log transforming the data for normal distribution (if not already normal)
pm25_april$pm25log <- log(pm25_april$pm25)
hist(pm25_april$pm25log)
lincoln_daily$pm25log <- log(lincoln_daily$pm25)

hist(pm25_april$pm25log)
# n = length(pm25_april$pm25log)
# h = hist(pm25_april$pm25log, breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))
# b = h$counts
# k = length(h$breaks)-1 # number of intervals
# x1 = h$breaks
# m = mean(pm25_april$pm25log, na.rm = TRUE)
# s = sd(pm25_april$pm25log, na.rm = TRUE)

# Assemble table of X_0^2
# v = array(0, dim = c(k))             # Array for the normalized value
# p = v                                # Array for the F(x) or CDF of the normalized variable, v
# e = v                                # Variable, e_j = n * p_j, where n is the number of variables
# for (j in 1:k) {
#   v[j] = (x1[j+1] - m)/s         # Normalized variable
#   p[j] = pnorm(v[j])             # F(x_j)
#   if (j==k) {
#     p[j] = 1
#   }
#   e[j] = n*p[j]
#   if (j>1) {
#     e[j] = e[j] - (n*p[j-1]) # adjustment for integral of p(j) from v(j-1) to v(j)
#   }
# }
# X_0 = sum(((b-e)^2)/e)               # = 2.809 Summation.  Equation (1) from section 23.7 box.
# #k - r - 1 #k = number of intervals, r = 2 (5 - 2 - 1 = 2)

# fixing zero values
j <- 0
for (i in 1:nrow(lincoln_daily)) {
  if (is.na(lincoln_daily$pm25[i]) == FALSE) {
    if (lincoln_daily$pm25[i] == 0) {
      lincoln_daily$pm25[i] <- NA
      lincoln_daily$pm25log[i] <- NA
      j <- j + 1
    }
  }
}

#april 2020 v. all previous months
model1=lm(pm25log~april_2020 + precip, data = lincoln_daily) 
summary(model1)
confint(model1)

#april 2020 v. previous aprils
model2=lm(pm25log~april_2020 + precip, data = pm25_april)
summary(model2)
confint(model2)

#april 2020 v. previous aprils with indicators
model3=lm(pm25log~april_2019 + april_2018 + april_2017 + april_2016 + precip, data = pm25_april)
summary(model3)
confint(model3)

#over time
# model4=lm(pm25log~date + precip, data = lincoln_daily)
# summary(model4)
# confint(model4)

# lm_pm25 <- array(0, dim = c(nrow(pm25_april)))
# for (i in 1:nrow(pm25_april)) {
#   lm_pm25[i] = 12.3680 - 3.7896*pm25_april$april_2020[i] - 6.9520*pm25_april$precip[i] }
r <- residuals(model3)
hist(r)
plot(r)

# r2 <- lm_pm25 - pm25_april$pm25
# plot(r,r2[1:148], type = "p")
# 
# #visual
# ggplot() +
#   geom_point(data = lincoln_daily, mapping = aes(x = date, y = pm25))
