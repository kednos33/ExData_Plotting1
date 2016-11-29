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

# Plots datetime ~ Energy sub metering
plot3 <- function(fileName){
  # Loadind the data
  tab <- loadData(fileName)
  
  # Opening the png file
  png(filename = "plot3.png", width = 480, height = 480)
  
  # Setting graphical parameters
  par(mfrow = c(1, 1), mar = c(4, 4, 4, 4))
  
  # Making the plot
  with(tab, plot(x = Date, y = Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l"))
  lines(x = tab$Date, y = tab$Sub_metering_2, col = "red")
  lines(x = tab$Date, y = tab$Sub_metering_3, col = "blue")
  legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metring_1", "Sub_metring_2", "Sub_metring_3"))
  
  # Closing the png file
  dev.off()
  
} 