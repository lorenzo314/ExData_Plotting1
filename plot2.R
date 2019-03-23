library(dplyr)
library(lubridate)

plot2 <- function(dt) {
	png(filename = "plot2.png",
		width = 480, height = 480, units = "px", pointsize = 12,
		bg = "white",  res = NA,
		type = c("cairo", "cairo-png", "Xlib", "quartz"))

	plot(dt$Date, dt$Global_active_power,
		t="l", xlab="", ylab="Gloabl Active Power (kilowatts)")

	dev.off()
}

get_data <- function() {
        df<-read.table("household_power_consumption.txt",
                sep=";", header = TRUE, na.strings = "?",
                as.is = TRUE, comment.char = "",
                nrows=2075260)
        dt <- tbl_df(df)
        rm("df")

        dt <- mutate(dt, Date = dmy_hms(paste(Date, Time, sep=" ")))
        dt2 <- select(dt, -Time)
        rm("dt")

        dt3 <- filter(dt2, year(Date) == 2007, month(Date) == 2,
                day(Date) == 1 | day(Date) == 2)
        rm("dt2")
	dt3
}
