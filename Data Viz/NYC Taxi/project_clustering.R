getwd()
setwd('E:/dataset/')
df<-read.csv("Consolidated.csv")
df.scaled<-scale(data.frame(df$fare,df$tip,df$dist,df$Count))
colnames(df.scaled)<-c("fare","tip amount","distnace","count")
totwss<-vector()
btwss<-vector()
for (i in 2:10)
{
  set.seed(1234)
  temp<-kmeans(df.scaled,centers=i)
  totwss[i]<-temp$tot.withinss
  btwss[i]<-temp$betweenss
}

df$cluster<-result$cluster
plot(totwss,xlab="Cluster Number",type="b",ylab="Total Within Sum of Square")
plot(btwss,xlab="Cluster Number",type="b",ylab="Total Between Sum of Square")
set.seed(1234)
result<-kmeans(df.scaled,centers=3)
df.center<-aggregate(data.frame(df$fare,df$tip,df$dist,df$Count),by=list(df$cluster),mean)
df.sum<-aggregate(data.frame(df$fare,df$tip,df$dist,df$Count),by=list(df$cluster),sum)
df.sum
write.csv(df,'Consolidated_processed.csv')
colnames(df)
df2<-subset(df,cluster==2)
df.center
