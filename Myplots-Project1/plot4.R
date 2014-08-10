makePlot_4 <- function(file) {

#reading data and making a data frame from it
data=read.table(file,sep=";",header=TRUE)
df<-data.frame(data)


# using date and time for plot4
timeDate<-paste(df$Date,df$Time)

#formating date and time
timeDateF<-strptime(timeDate,format="%d/%m/%Y %H:%M:%S")

#create a data frame with timeDateF 
dfSub4<-data.frame(timeDateF,df$Global_active_power,df$Global_reactive_power,df$Voltage,df$Sub_metering_1,df$Sub_metering_2,df$Sub_metering_3)


# Sub-setting data from 1-Feb-2007 to 2-Feb-2007

lower<-strptime("31/01/2007 23:59:59",format="%d/%m/%Y %H:%M:%S")
upper<-strptime("3/02/2007 00:00:00",format="%d/%m/%Y %H:%M:%S")
dfSub4<-subset(dfSub4,dfSub4$timeDate>lower)
dfSub4<-subset(dfSub4,dfSub4$timeDate<upper)

#making variables as numeric
glob_act_pow<-as.numeric(levels(dfSub4$df.Global_active_power))[dfSub4$df.Global_active_power]
glob_react_pow<-as.numeric(levels(dfSub4$df.Global_reactive_power))[dfSub4$df.Global_reactive_power]
voltage<-as.numeric(levels(dfSub4$df.Voltage))[dfSub4$df.Voltage]
sub_met1<-as.numeric(levels(dfSub4$df.Sub_metering_1))[dfSub4$df.Sub_metering_1]
sub_met2<-as.numeric(levels(dfSub4$df.Sub_metering_2))[dfSub4$df.Sub_metering_2]
sub_met3<-as.numeric(dfSub4$df.Sub_metering_3)

r1<-range(sub_met1)
r2<-range(sub_met2)
r3<-range(sub_met3)

#plot 4

png(file="plot4.png",width=480,height=480)

par(mfrow=c(2,2), mar=c(4,4,4,4))

with(dfSub4,{
plot(dfSub4$timeDate,glob_act_pow,type="l",xlab="",ylab="Global Active Power")
plot(dfSub4$timeDate,voltage,type="l",xlab="datetime",ylab="Voltage")
plot(dfSub4$timeDate,sub_met1,type="l",col=1,ylim=c(0,max(r1)),xlab="",ylab="Energy sub metering")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1),lwd=c(2.5,2.5),col=c("black","red","blue"),cex=0.5)
lines(sub_met2~dfSub4$timeDate,type="l",col=2,ylim=c(0,max(r2)))
lines(sub_met3~dfSub4$timeDate,type="l",col=4,ylim=c(0,max(r3)))
plot(dfSub4$timeDate,glob_react_pow,type="l",xlab="datetime",ylab="Global_reactive_power")
})

dev.off()

}