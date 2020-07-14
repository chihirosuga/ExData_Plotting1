Plot3 <- function() {
  
  # load file
  filename <- "../household_power_consumption.txt"	
  data_all = read.table(filename, header = TRUE, sep = ";")
  dim(data_all)
  
  # select data between cutoffs	
  cutoff_1 <- strptime("2007-02-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
  cutoff_2 <- strptime("2007-02-02 00:00:00", format="%Y-%m-%d %H:%M:%S")
  datetime_ <- strptime(paste(data_all[,1], data_all[,2]), "%d/%m/%Y %H:%M:%S")
  
  between_cutoffs = datetime_ >= cutoff_1 & datetime_ <= cutoff_2
  
  data_ <- data_all[between_cutoffs,]
  dim(data_)
  
  # remove NANs
  data_ <- data_[!is.na(data_$Date),]
  dim(data_)
  
  
  # Create Plot 3
  png("plot3.png", width = 480, height = 480)
  label_x <- c("Thu", "Fri", "Sat")
  data_length <- length(data_$Global_active_power)
  
  plot( 1:data_length,as.numeric(data_$Sub_metering_1), ylab = "Energy sub metering", xlab="", xaxt='n', ylim=c(0, 40), type="l", col="black" )
  par(new=TRUE)
  plot( 1:data_length,as.numeric(data_$Sub_metering_2), ylab = "", xlab="", xaxt='n', ylim=c(0, 40), type="l", col="red" )
  par(new=TRUE)
  plot( 1:data_length,as.numeric(data_$Sub_metering_3), ylab = "", xlab="", xaxt='n', ylim=c(0, 40), type="l", col="blue" )
  
  axis(1, at=c(0,data_length/2,data_length), labels=label_x)
  legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty=1, cex=0.8)
  
  dev.off()
  
}