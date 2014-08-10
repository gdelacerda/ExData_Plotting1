makePlot_2 <- function(file) {

#reading data and making a data frame from it
data=read.table(file,sep=";",header=TRUE)
df<-data.frame(data)


# using date and time for plot2
timeDate<-paste(df$Date,df$Time)

#formating date and time
timeDateF<-strptime(timeDate,format="%d/%m/%Y %H:%M:%S")

dfSub2<-data.frame(timeDateF,df$Global_active_power)

# Sub-setting data from 1-Feb-2007 to 2-Feb-2007
lower<-strptime("31/01/2007 23:59:59",format="%d/%m/%Y %H:%M:%S")
upper<-strptime("3/02/2007 00:00:00",format="%d/%m/%Y %H:%M:%S")
dfSub2<-subset(dfSub2,dfSub2$timeDate>lower)
dfSub2<-subset(dfSub2,dfSub2$timeDate<upper)

#plot2
png(file="plot2.png",width=480,height=480)
plot(dfSub2$timeDate,as.numeric(levels(dfSub2$df.Global_active_power))[dfSub2$df.Global_active_power],type="l",xlab="",ylab="Global Active Power (kilowatts)")
dev.off()

}
