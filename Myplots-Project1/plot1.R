makePlot_1 <- function(file) {

#reading data and making a data frame from it
data=read.table(file,sep=";",header=TRUE)
df<-data.frame(data)

asDate<-as.Date(df$Date,format="%d/%m/%Y")
dfasDate<-data.frame(asDate,df[,-1])

# Sub-setting data from 1-Feb-2007 to 2-Feb-2007
from.dat<-as.Date("31/01/07","%d/%m/%y")
to.dat<-as.Date("03/02/07","%d/%m/%y")
dfSub<-subset(dfasDate,dfasDate$asDate>from.dat)
dfSub<-subset(dfSub,dfSub$asDate<to.dat)

#histogram
glob_act_pow<-as.numeric(levels(dfSub$Global_active_power))[dfSub$Global_active_power]
png(file="plot1.png",width=480,height=480)
hist(glob_act_pow,main="Global Active Powder", col="red", xlab="Global Active Power (kilowatts)")
dev.off()

}