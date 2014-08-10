makePlot_3 <- function(file) {

#reading data and making a data frame from it
data=read.table(file,sep=";",header=TRUE)
df<-data.frame(data)

# using date and time for plot3
timeDate<-paste(df$Date,df$Time)

#formating date and time
timeDateF<-strptime(timeDate,format="%d/%m/%Y %H:%M:%S")

# Sub-setting data from 1-Feb-2007 to 2-Feb-2007
dfSub3<-data.frame(timeDateF,df$Sub_metering_1,df$Sub_metering_2,df$Sub_metering_3)
lower<-strptime("31/01/2007 23:59:59",format="%d/%m/%Y %H:%M:%S")
upper<-strptime("3/02/2007 00:00:00",format="%d/%m/%Y %H:%M:%S")
dfSub3<-subset(dfSub3,dfSub3$timeDate>lower)
dfSub3<-subset(dfSub3,dfSub3$timeDate<upper)

#converting the variables to numeric values
series1<-as.numeric(levels(dfSub3$df.Sub_metering_1))[dfSub3$df.Sub_metering_1]
series2<-as.numeric(levels(dfSub3$df.Sub_metering_2))[dfSub3$df.Sub_metering_2]
# Sub_metering_3 is not a factor variable
series3<-as.numeric(dfSub3$df.Sub_metering_3)

#getting the ranges for each variable
r1<-range(series1)
r2<-range(series2)
r3<-range(series3)

#plot3

png(file="plot3.png",width=480,height=480)
plot(dfSub3$timeDate,series1,type="l",col=1,ylim=c(0,max(r1)),xlab="",ylab="Energy sub metering")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1),lwd=c(2.5,2.5),col=c("black","red","blue"),cex=0.75)
lines(series2~dfSub3$timeDate,type="l",col=2,ylim=c(0,max(r2)))
lines(series3~dfSub3$timeDate,type="l",col=4,ylim=c(0,max(r3)))

dev.off()

}