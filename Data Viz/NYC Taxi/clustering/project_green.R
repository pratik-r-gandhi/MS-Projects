install.packages("data.table")
library(data.table)
install.packages("ff")
setwd('C:/Users/sxs140732/project/dataset/green/')
getwd()
dt=data.frame()
?list.files
for(i in 1:6)
{
filepath=paste(c("green_tripdata_2015-0",i,".csv"),collapse="")
df1<-fread(filepath,select=c(1:20))
colnames(df1)=c("VendorID","lpep_pickup_datetime","Lpep_dropoff_datetime","Store_and_fwd_flag","RateCodeID","Pickup_longitude","Pickup_latitude","Dropoff_longitude","Dropoff_latitude","Passenger_count","Trip_distance","Fare_amount","Extra","MTA_tax","Trip_amount","Tolls_amount","Ehail_fee","improvement_surcharge","Total_amount","Payment_type")
df1$Ehail_fee=NULL
dt<-rbind(dt,df1)
}
View(dtemp)
summary(dt)
summary(df1)
install.packages("maps")
library(maps)
install.packages("ggplot2")
library(ggplot2)
all_states<-map_data('state')
colnames(df1)
dt$Pickup_latitude<-round(dt$Pickup_latitude,2)
dt$Pickup_longitude<-round(dt$Pickup_longitude,2)
View(head(df1,n=6))
df1=NULL
df3=NULL
all_states=NULL
gc()
install.packages("ggplot2")
class(df1)
ny<-subset(all_states,subregion %in% c("manhattan"))
print(ny)
colnames(ny)
ny$long<-round(ny$long,2)
ny$lat<-round(ny$lat,2)
ny
head(df1,n=6)
df2$hour=0
colnames(df2)
install.packages("sqldf")
library(sqldf)
sf<-sqldf("select distinct long,lat from ny order by long,lat")
range(ny$long)
range(ny$lat)
length(df2$hour)
df3=NULL
unique(df2$hour)
df2<-sqldf("select * from dt where pickup_longitude between -74.03 and -73.93 and pickup_latitude between 40.71 and 40.84")
class(df2)
warnings()
df2$manhattan=NULL
unique(df2$pickup_latitude)
colnames(df2)
df3<-sqldf("select sum(Fare_amount),sum(Trip_amount),sum(Trip_distance),sum(Total_amount),count(*),Pickup_longitude,Pickup_latitude,strftime('%H',lpep_pickup_datetime)HOUR from df2 group by Pickup_longitude,Pickup_latitude,HOUR")
View(df3)
colnames(df2)
summary(df3)
write.csv(df3,'green_2015_consolidated.csv')
write.csv(dt,'green_all.csv')
class(df2$tpep_pickup_datetime)
tt<-data.frame(head(df1,n=1000))
df1=NULL
df3<-sqldf("select sum(fare_amount),sum(trip_amount),sum(trip_distance),sum(total_amount),pickup_longitude,pickup_latitude,strftime('%H',tpep_pickup_datetime)HOUR from tt group by pickup_longitude,pickup_latitude,HOUR")
colnames(tt)
class(tt$tpep_pickup_datetime)


