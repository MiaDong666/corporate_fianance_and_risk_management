library(lubridate)
library(xts)
library(readxl)
library(moments)

raw_data=read_excel("stocks.xlsx")
raw_data$market_cap=abs(raw_data$`Price or Bid/Ask Average`)*raw_data$`Shares Outstanding`/1000
raw_data$date=ymd(raw_data$`Names Date`)
raw_data$Returns=as.numeric(raw_data$Returns)
raw_data=raw_data[complete.cases(raw_data),]
N=length(raw_data$PERMNO)
yr=vector()
cusip = vector()
MC = vector()
date=vector()
permno=vector()



mc=raw_data$market_cap[1]
X=1+raw_data$Returns[1]
count=1
for(i in 2:N){
  if(raw_data$PERMNO[i]==raw_data$PERMNO[i-1] && 
     year(raw_data$date[i])==year(raw_data$date[i-1])){
    
    X=X*(1+raw_data$Returns[i])
    mc=mc+raw_data$market_cap[i]
    count=count+1
  }else{
    Y=X^(12/count)-1
    yr=append(yr,Y)
    permno=append(permno,raw_data$PERMNO[i-1])
    MC=append(MC,(mc/count))
    date=append(date,year(raw_data$date[i-1]))
    cusip=append(cusip,raw_data$`CUSIP Header`[i-1])

    X=1+raw_data$Returns[i]
    count=1
    mc=raw_data$market_cap[i]
  }
}

result=data.frame(permno,cusip,date,MC,yr)

summary(result$yr)
mean(result$yr)
sqrt(var(result$yr))
skewness(result$yr)
kurtosis(result$yr)
summary(result$MC)
result[which.max(result$MC),]

write.csv(result,file="annretset.csv")

quantile(result$yr,c(0.005,0.995))

return=result[-which(result$yr>4.0317063  ),]
return=return[-which(return$yr<(-0.9404624)),]
hist(return$yr,breaks = 80,main="99% quantile data")




mean(return$yr)
median(return$yr)
sqrt(var(return$yr))
skewness(return$yr)
kurtosis(return$yr)


