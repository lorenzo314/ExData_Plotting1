library(dplyr)
library(lubridate)

plot3 <- function(dt) {
	dt1<-select(dt, Date, Sub_metering_1)
	dt2<-select(dt, Date, Sub_metering_2)
	dt3<-select(dt, Date, Sub_metering_3)

	png(filename = "plot3.png",
		width = 480, height = 480, units = "px", pointsize = 12,
		bg = "white",  res = NA,
		type = c("cairo", "cairo-png", "Xlib", "quartz"))

	plot(dt1$Date, dt1$Sub_metering_1,
		xlab="", ylab="Energy sub metering", "n")
	points(dt1$Date, dt1$Sub_metering_1, t="l", col="black")
	points(dt2$Date, dt2$Sub_metering_2, t="l", col="red")
	points(dt3$Date, dt3$Sub_metering_3, t="l", col="blue")

	leg <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
	col <- c("black", "red", "blue")
	legend("topright", legend = leg, col = col, lty = 1)

	rm("dt1")
	rm("dt2")
	rm("dt3")

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
