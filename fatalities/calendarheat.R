source("https://github.com/iascchen/VisHealth/blob/master/R/calendarHeat.R")

dat <- 
  read.csv("fatalities.csv")

dat$date <- 
  as.Date(dat$date, format = "%m/%d/%Y")

dat$civilian_binary <-
  ifelse(dat$civilian_fatalities > 0, 1, 0)

calendarHeat(dat$date, dat$civilian_binary, varname="MSFT Adjusted Close", color=c('red', 'white'), scales=list(x=list(draw=FALSE)))
