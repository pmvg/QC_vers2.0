anomally <- c()
for(i in 1:data_len){
  val1 <- Data$Value[i]
  val2 <- Data_calc$Mean[Data_calc$Year == Data$Year[i] & Data_calc$Month == Data$Month[i]]
  anomally[i] <- val1 - val2
}
plot(anomally)
m <- max(anomally)+5
mi <- min(anomally)
barplot(anomally,ylim = c(mi,m))

data_sub <- Data[Data$Year == 1887,]
plot(Data$Year,Data$Value)
date_l <- as.Date(paste(data_sub$Year,"/",data_sub$Month,"/",data_sub$Day,sep = ""))
plot(data_sub$Value~date_l,type="l",xlab = "Date",ylab = vname)
print(date_l)








