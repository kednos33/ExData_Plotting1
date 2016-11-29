# Loads the data
loadData <- function(fileName){
  
  ind1 <- grep("1/2/2007", readLines(fileName), value = FALSE)[1]
  ind2 <- grep("3/2/2007", readLines(fileName), value = FALSE)[1]
  nbRows <- ind2 - ind1 + 1
  
  tab <- read.table(fileName, sep = ";", skip = ind1 - 1, nrows = nbRows, col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  tab[, 1] <- paste(tab[, 1], tab[, 2], sep = " ")
  tab[, 1] <- as.POSIXct(strptime(tab[, 1], format = "%d/%m/%Y %H:%M:%S"))
  
  
  return(tab)
}

# Plots the 4 measurements into a single png file
plot4 <- function(fileName){
  
  # Loadind the data
  tab <- loadData(fileName)
  

  # Opening the png file
  png(filename = "plot4.png", width = 480, height = 480)
  
  # Making the plot
  
  # Setting graphical parameters
  par(mfrow = c(2, 2), mar = c(4, 4, 4, 2))
  
  # datetime ~ Global Active Power
  with(tab, plot(x = Date, y = Global_active_power, xlab = "", ylab = "Global Active Power", type = "l"))
  
  # datetime ~ Voltage
  with(tab, plot(x = Date, y = Voltage, xlab = "datetime", ylab = "Voltage", type = "l"))
  
  # datetime ~ Energy sub metering with no border for the legend box
  with(tab, plot(x = Date, y = Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l"))
  lines(x = tab$Date, y = tab$Sub_metering_2, col = "red")
  lines(x = tab$Date, y = tab$Sub_metering_3, col = "blue")
  legend("topright", bty = "n", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metring_1", "Sub_metring_2", "Sub_metring_3"))
  
  # datetime ~ Global_reactive_power
  with(tab, plot(x = Date, y = Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "l"))
  
  # Closing the png file
  dev.off()
  
}

