## R code example for app

library(dplyr)
library(readr)
library(lubridate)

# Load data
trend_data <- read_csv("data.csv")
names(trend_data) <- c("date","type","count")

# define plot colors
color = "#000088"
color1 = "#BBAA55"
color2 = "#0011EE"

#Filter for selected crime trend
selected_trends <- trend_data %>% filter( type == 'Robbery',
                date > as.POSIXct('2016-01-01') & date < as.POSIXct('2020-03-01')
                )


#plot
par(mar = c(4, 4, 1, 1))
plot(x = selected_trends$date, y = selected_trends$count, type = "l",
     xlab = "Date", ylab = "Crime Count", col = color1, fg = color, col.lab = color, col.axis = color2)


# Display only if smoother is checked
smooth_curve <- lowess(x = as.numeric(selected_trends$date), y = selected_trends$count, f = .67)
lines(smooth_curve, col = "#882211", lwd = 3)
