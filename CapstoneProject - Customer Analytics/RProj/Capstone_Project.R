setwd("C:\\Users\\PratikGandhi\\Desktop\\Fall 2015\\CapstoneProject\\RProj")
options(scipen = 999)
load("clust.RData")

#--Initial Load
colType <- c("YEAR"="integer","CustomerId"="character","CompanyName"="character","City"="character",
             "State"="character","Country"="character","MemberType"="character","MembershipTypeCode"="character",
             "MembershipPeriodBegin"="character","MembershipPeriodEnd"="character",
             "CompanySize"="character","JobTitleCode"="character","IndustryCode"="character",
             "SICCode"="character","JobFunctionCode"="character","RateCode"="character",
             "ConsecutiveYearsAsMember"="integer","InitialConsecutiveYear"="Date",
             "BenefitAllocations"="integer","BenefitsUsed"="integer","AllocationUsage"="numeric",
             "SetCOPPreference"="character","SSOExists"="character","Purchase Amt"="numeric",
             "Purchase Qty"="integer", "Webcast Registration"="integer",
             "Webcast Attendance"="integer","Web Visits"="integer", "Web Page Views"="integer",
             "Blog Visits"="integer","Blog Page Views"="integer","Forum Visits"="integer",
             "Forum Page Views"="integer","ParatureTickets"="integer","ParatureChats"="integer",
             "Registered for Edu"="integer","Attended ICE"="integer","Attended TK"="integer","Frugal"="character",
             "Chapter Board"="character", "InitialMembership"="character","Retained"="factor",
             "Bundle"="character","ProfileCompletion"="numeric",
             "NumberofLogins"="integer", "Downloads"="integer","ForumMember"="factor","FreeUpgrade"="factor")

init_data <- read.csv("EngagementMetricsNoEmail.csv", header = TRUE, sep = ",", colClasses = colType, 
                      stringsAsFactors = FALSE, na.strings = c("NA",""))
#str(init_data)
#colnames(init_data)

#--Changing data types, filtering out future dated data, creating Id column and deleting unnecessary columns

#init_data$YEAR <- as.numeric(as.character(init_data$YEAR))
init_data$MembershipPeriodBegin <- as.Date(init_data$MembershipPeriodBegin,format = "%m/%d/%Y")
init_data$MembershipPeriodEnd <- as.Date(init_data$MembershipPeriodEnd,format = "%m/%d/%Y")

init_data <- init_data[init_data$YEAR >= 2011 & init_data$YEAR <= 2015,]
init_data <- init_data[format(init_data$MembershipPeriodEnd,"%Y") >= "2011" & format(init_data$MembershipPeriodEnd,"%Y") <= "2016",]

init_data$Id <- paste(init_data$YEAR,init_data$CustomerId, sep = "")
init_data$Id <- as.numeric(init_data$Id)


init_data$MemberType[init_data$MemberType == "INDIVIDUAL"] <- "1"
init_data$MemberType[init_data$MemberType == "GROUP"] <- "2"
init_data$MemberType[init_data$MemberType == "PARTNER"] <- "3"
init_data$MemberType <- factor(init_data$MemberType)

init_data$MembershipTypeCode <- ifelse(grepl("PROF", init_data$MembershipTypeCode, ignore.case = T), "1", 
                                       ifelse(grepl("PLUS", init_data$MembershipTypeCode, ignore.case = T), "2", "3"))
init_data$MembershipTypeCode <- factor(init_data$MembershipTypeCode)

init_data$Attended.ICE[init_data$Attended.ICE > 0] <- "1"
init_data$Attended.ICE[init_data$Attended.ICE == 0] <- "0"
init_data$Attended.ICE <- factor(init_data$Attended.ICE)

init_data$Attended.TK[init_data$Attended.TK > 0] <- "1"
init_data$Attended.TK[init_data$Attended.TK == 0] <- "0"
init_data$Attended.TK <- factor(init_data$Attended.TK)

init_data$Frugal[!is.na(init_data$Frugal)] <- "1"
init_data$Frugal[is.na(init_data$Frugal)] <- "0"
init_data$Frugal <- factor(init_data$Frugal)

init_data$Chapter.Board[!is.na(init_data$Chapter.Board)] <- "1"
init_data$Chapter.Board[is.na(init_data$Chapter.Board)] <- "0"
init_data$Chapter.Board <- factor(init_data$Chapter.Board)

init_data$SetCOPPreference[init_data$SetCOPPreference == "Y"] <- "1"
init_data$SetCOPPreference[init_data$SetCOPPreference == "N"] <- "0"
init_data$SetCOPPreference <- factor(init_data$SetCOPPreference)

colnames(init_data)[42] <- "Retention"
init_data$Target[init_data$Retention == "Retained"] <- 1
init_data$Target[init_data$Retention == "Lost"] <- 0
init_data$Target[init_data$Retention != "Retained" & init_data$Retention != "Lost" ] <- 2
init_data$Target <- factor(init_data$Target)

init_data <- init_data[c(49,1:48,50)]
init_data <- init_data[,c(-2:-7,-10:-17,-19:-21,-24,-28,-42:-44,-47:-49)]

str(init_data)
colnames(init_data)

#imputing missing values in Purchase.Qty
library(mice)
tmp <- mice(init_data, maxit = 0)
pred <- tmp$predictorMatrix
pred[,"Id"] <- 0
tmp <- mice(init_data, m = 5, maxit = 5, pred = pred, seed = 12345)
dat <- complete(tmp, action = "broad", include = TRUE)

for (i in which(is.na(init_data$Purchase.Qty))) 
{
    init_data$Purchase.Qty[i] <- floor(mean(as.numeric(dat[i,c("Purchase.Qty.1",
                                                               "Purchase.Qty.2",
                                                               "Purchase.Qty.3",
                                                               "Purchase.Qty.4",
                                                               "Purchase.Qty.5")])))
}

for (i in which(is.na(init_data$Purchase.Amt))) 
{
    init_data$Purchase.Amt[i] <- floor(mean(as.numeric(dat[i,c("Purchase.Amt.1",
                                                               "Purchase.Amt.2",
                                                               "Purchase.Amt.3",
                                                               "Purchase.Amt.4",
                                                               "Purchase.Amt.5")])))
}


# View(init_data)
# d1 <- densityplot(init_data$Purchase.Qty)
# d2 <- densityplot(init_data$PQ)
# grid.arrange(d1,d2, nrow = 2, ncol = 1)


#Graphs

# pdf("var.pdf", width = 13, height = 11)
# par(mfrow=c(2,2))
# 
# #png(file = "variables.png", width = 1366, height = 768)
# #par(mfrow=c(2,4))
# barplot(table(init_data$MemberType,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "Member Types and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05), cex = 1.75))
# barplot(table(init_data$MembershipTypeCode,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "Membership Code and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05),cex = 1.75))
# barplot(table(init_data$SetCOPPreference,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "COP Preference and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05),cex = 1.75))
# barplot(table(init_data$Attended.ICE,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "ICE Attendance and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05),cex = 1.75, legend = c("Attended","Not Attended")))
# barplot(table(init_data$Attended.TK,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "TK Attendance and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05),cex = 1.75, legend = c("Attended", "Not Attended")))
# barplot(table(init_data$Frugal,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "Frugal and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05),cex = 1.75, legend = c("Yes","No")))
# barplot(table(init_data$Chapter.Board,init_data$Retention), cex.axis = 2, cex.names = 2, cex.main = 2,main = "Chapter Board and Retention",ylim = c(0,80000), legend.text = TRUE, args.legend = list(x="topright", inset = c(0,0.05),cex = 1.75, legend = c("Enrolled","Not Enrolled")))
# #dev.off()
# 
# #png(file = "num_variables.png", width = 1366, height = 768)
# 
# #pdf("num_var.pdf", width = 13, height = 11)
# #par(mfrow=c(3,2))
# 
# for (i in 2:ncol(init_data))
# {
#     if (class(init_data[,i]) == "numeric" | class(init_data[,i]) == "integer")
#     {
#         form <- paste(names(init_data)[i]," ~ Retention")
#         five <- aggregate(as.formula(form), data = init_data, summary)
#         
#         ret <- unname(unlist(five[1,2][6]))
#         lost <- unname(unlist(five[2,2][6]))
#         oth <- unname(unlist(five[3,2][6]))
#         
#         boxplot(as.formula(form), data = init_data, range = 0, boxwex = 0.5, horizontal = TRUE, staplewex = 1,
#                 main = paste(names(init_data)[i],"and Retention"), cex.axis = 1.6, cex.main = 2)
#         text(x=ret, labels =ret, y=1.45, cex = 1)
#         text(x=lost, labels =lost, y=2.45, cex = 1)
#         text(x=oth, labels =oth, y=3.45, cex = 1)
#     }
# }
# dev.off()



#Partitioning into training, testing and scoring data sets
score <- init_data[as.character(init_data$Target) == "2",]
score$Target <- factor(score$Target)
classify <- init_data[as.character(init_data$Target) == "1" | as.character(init_data$Target) == "0",]
classify$Target <- factor(classify$Target)

set.seed(12345)
test <- sort(sample(nrow(classify), nrow(classify)/3))
train_data <- classify[-test,]
test_data <- classify[test,]
test_target <- classify$Target[test]

train_data <- train_data[,-1]

#View(cor(train_data[,c(3,4,6:16,21,22)]))
#train_data <- train_data[,c(-11,-9,-12,-13)] 

library(caret)
library(ROCR)
#----------------------------------Logistic Regression-----------------------------------------------
set.seed(101)
#lr <- glm(Target ~ ., data = train_data, family = binomial)
lr <- glm(Target ~ . - Forum.Visits - Forum.Page.Views, data = train_data, family = binomial)
summary(lr)

plr <- predict.glm(lr, newdata = test_data, type = "response")
plr <- ifelse(plr > 0.5,1,0)
mean(plr != test_data$Target)
table(plr, test_data$Target)

#Confusion Matrix and AUC
confusionMatrix(plr, test_data$Target, positive = "1")
roc_pred <- prediction(plr, test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values

#----------------------------Regularized Regression------------------
# library(glmnet)
# cv.fit <- cv.glmnet(data.matrix(train_data[,-23]), train_data[,23], alpha = 0, family="binomial")
# 
# plot(cv.fit)
# coef(cv.fit)
# 
# fit <- glmnet(data.matrix(train_data[,-23]), train_data[,23], alpha = 1, family="binomial")
# 
# plot(fit)
# 
# prediction <- predict(fit, newx=data.matrix(test_data[,c(-1,-24)]), type="class")
# mean(prediction != test_data$Target)
# table(prediction, test_data$Target)
#----------------------------------------------------------------


#------------------------------------------Decision Tree---------------------------------------------

library(rpart)
library(rattle)

set.seed(101)
dt <- rpart (Target ~ ., data = train_data, method = "class")
# dt <- rpart (Target ~ ., data = train_data, method = "class", control = rpart.control(minsplit = 30, cp = 0.00, xval = 25))

summary(dt)
printcp(dt)

#plotting tree
#plot(dt,uniform=TRUE,margin=0.1)
#text(dt,use.n=T,cex=0.8)

fancyRpartPlot(dt, sub = "decision_tree", cex = 0.65)

#analysing cp and pruning tree
# plotcp(dt)
# 
# prunedt<- prune(dt, 0.013)


#Predict using decision tree
pdt <- predict(dt, test_data, type = "class")
#pdt <- predict(dt, test_data, type = "prob")

#confusion matrix and misclassification rate
mean(pdt != test_data$Target)
table(pdt, test_data$Target)

#Confusion Matrix and AUC
confusionMatrix(pdt, test_data$Target, positive = "1")
roc_pred <- prediction(pdt[,2], test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values


#-----C50-----
library(C50)

set.seed(101)
c50_dt <- C5.0(train_data[,-24], train_data[,24], trials = 50, control = C5.0Control(earlyStopping = FALSE))
#summary(c50_dt)

pc5dt <- predict(c50_dt, test_data[,c(-1,-25)], type = "class")
#pc5dt <- predict(c50_dt, test_data[,c(-1,-25)], type = "prob")
mean(pc5dt != test_data$Target)


#Confusion Matrix and AUC
confusionMatrix(pc5dt, test_data$Target, positive = "1")
roc_pred <- prediction(pc5dt[,2], test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values


scored_result <- predict(c50_dt, score[,c(-1,-25)], type = "class")
scored_result <- as.character(ifelse(as.character(scored_result) == "1", "Retained", "Lost" ))

write.csv(x = data.frame(Year = substring(score$Id, 1, 4), Id = substring(score$Id, 5), Predicted.Values = scored_result), file = "upd_scored_result.csv", quote = FALSE, row.names = FALSE)

#-----------------------------------------Random Forest----------------------------------------------

library(randomForest)

set.seed(101)
# rf <- randomForest(Target ~ ., data = train_data, importance = TRUE, proximity = TRUE)
rf1 <- randomForest(x = train_data[,-23], y = train_data[,23], ntree = 400, norm.votes = FALSE,do.trace = 25)

prf1 <- predict(rf1, test_data, type = "response")
#prf1 <- predict(rf1, test_data, type = "prob")
mean(prf1 != test_data$Target)
# table(prf1, test_data$Target)

# print(rf1)
# summary(rf1)
# plot(rf1)
# varImpPlot(rf1)

#Confusion Matrix and AUC
confusionMatrix(prf1, test_data$Target, positive = "1")
roc_pred <- prediction(prf1[,2], test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values

#-------------Partial Plots---------------------------
# partialPlot(rf1,train_data, Web.Visits, "0")

# imp <- importance(rf1)
# impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
# 
# op <- par(mfrow=c(1, 2), mar = rep(2, 4))
# for (i in seq_along(impvar[1:2])) {
#     partialPlot(rf1, train_data, impvar[i], "1", xlab=impvar[i],
#                 main=paste("Partial Dependence on", impvar[i]),
#                 ylim=c(30, 70))
# }
# par(op)
#-------------------------------------------------------


#------------------Multiple random forests---------------
#set.seed(201)
# rf2 <- randomForest(x = train_data[,-23], y = train_data[,23], ntree = 400, norm.votes = FALSE,do.trace = 25)
# prf2 <- predict(rf2, test_data, type = "class")
# mean(prf2 != test_data$Target)
# 
# for (i in 1:5)
# {
#     Sys.sleep(5)
#     gc(verbose = TRUE)
# }
# 
# #set.seed(301)
# rf3 <- randomForest(x = train_data[,-23], y = train_data[,23], ntree = 400, norm.votes = FALSE,do.trace = 25)
# prf3 <- predict(rf3, test_data, type = "class")
# mean(prf3 != test_data$Target)
# 
# #set.seed(401)
# rf4 <- randomForest(x = train_data[,-23], y = train_data[,23], ntree = 400, norm.votes = FALSE,do.trace = 25)
# prf4 <- predict(rf4, test_data, type = "class")
# mean(prf4 != test_data$Target)
# #------------------------------------------------------
# 
# for (i in 1:5)
# {
#     Sys.sleep(5)
#     gc(verbose = TRUE)
# }
# 
# 
# #------------Combining multiple forests----------------
# 
# comb_rf <- combine(rf1,rf2,rf3,rf4)
# prf <- predict(comb_rf, test_data, type = "response", predict.all = TRUE)
# #prf <- predict(comb_rf, test_data, type = "prob", predict.all = TRUE)
# mean(prf$aggregate != test_data$Target)
# table(prf, test_data$Target)
#------------------------------------------------------

#-------------------------------------------------CForest-------------------------------------------

# library(party)
# set.seed(101)
# cf1 <- cforest(Target ~ ., data = train_data, controls = cforest_unbiased(ntree = 50))
# pcf1 <- predict(cf1, test_data, OOB=TRUE, type = "response")
#--------------------------------------------------------------------------------------------


#-------------------------------------------------Gradient Boosting-------------------------------------------
library(gbm)

tdata <- train_data
tdata$Target <- as.numeric(as.character(tdata$Target))
gbdt <- gbm(Target ~ ., data = tdata, n.trees = 10000, distribution = "bernoulli", interaction.depth = 3, cv.folds = 10)#, shrinkage = 0.05)
gbm.perf(gbdt, method = "cv")

#pgbdt <- predict(gbdt, test_data[,c(-1,-24)], type = "response", n.trees = 5000)
pgbdt <- predict(gbdt, test_data[,c(-1,-24)], type = "response", n.trees = 10000)

out <- ifelse(pgbdt > 0.5, 1 ,0)
mean(out != test_data$Target)

#Confusion Matrix and AUC
confusionMatrix(out, test_data$Target, positive = "1")
roc_pred <- prediction(pgbdt, test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values


#-------------------------GBM with dismo package----------------------------------------
# library(dismo)
# gbm_dis <- gbm.step(data=tdata, gbm.x = 1:22, gbm.y = 23,
#                     family = "bernoulli", tree.complexity = 5,
#                     learning.rate = 0.01, bag.fraction = 0.5)

#png(file = "graphs.png", width = 1366, height = 768)
#gbm.plot(bm_dis, n.plots = 18, write.title = FALSE, plot.layout = c(4,5))
#gbm.plot.fits(bm_dis)
#dev.off()

# int <- gbm.interactions(bm_dis)
# int$rank.list
# int$interactions
# gbm.perspec(bm_dis, 21, 6, x.range = c(0,40), y.range = c(0,300), z.range = c(0.5,1))

# preds <- predict.gbm(bm_dis, test_data[,c(-1,-24)], n.trees = bm_dis$gbm.call$best.trees, type = "response")
# preds1 <- ifelse(preds > 0.5, 1 ,0)
# calc.deviance(obs = as.numeric(as.character(test_data$Target)), pred = preds, calc.mean = TRUE)
# mean(preds1 != test_data$Target)
# 
# d <- cbind(as.numeric(as.character(test_data$Target)), preds1)
# ret <- d[d[,1]==1, 2]
# loss <- d[d[,1]==0,2]
# e <- evaluate(p = ret, a = loss)

#-----------------------------------------------------------------------------------------------
#-------------------------------------------------Naive Bayes-------------------------------------------
library(e1071)
nb <- naiveBayes(Target ~ ., data = train_data, laplace = 1)
pnb <- predict(nb, test_data[,c(-1,-24)], type = "class")
pnb <- predict(nb, test_data[,c(-1,-24)], type = "raw")

mean(pnb != test_data$Target)
table(pnb, test_data$Target)

#Confusion Matrix and AUC
confusionMatrix(pnb, test_data$Target, positive = "1")
roc_pred <- prediction(pnb[,2], test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values


#library(bnlearn)


#------------------SVM------------------------------------------------
library(e1071)

# model.svm.linear <- svm(Target ~ ., data = train_data, method="C-classification",
#                         kernel="linear",probability = TRUE, gamma = 0.001, cost = 1000)
# 
# model.svm.radial <- svm(Target ~ ., data = train_data, method="C-classification",
#                         kernel="radial",probability = TRUE, gamma = 0.001, cost = 1000)

model.svm.radial <- svm(Target ~ ., data = train_data, method="C-classification",
                        kernel="radial",probability = TRUE)

pred.svm.radial <- predict(model.svm.radial, test_data[,c(-1,-24)],probability=TRUE, decision.values = TRUE)


mean(pred.svm.linear != test_data$Target)
table(pred.svm.linear, test_data$Target)

#svm <- tune.svm(Target ~ ., data = train_data, gamma=10^(-6:-1), cost=10^(1:4), 
#                tunecontrol = tune.control(sampling = "cross"))

#Confusion Matrix and AUC
confusionMatrix(pred.svm.radial, test_data$Target, positive = "1")
roc_pred <- prediction(attr(pred.svm.radial,"prob")[,1], test_data$Target)
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
#roc_perf <- performance(roc_pred, measure = "lift", x.measure = "rpp")
plot(roc_perf)
auc <- performance(roc_pred, measure = "auc")
auc@y.values

#---------------------------kNN-----------------------------------------
library(kknn)

#knn <- train.kknn(Target ~ ., data = train_data, kmax = 100, distance = 1, kernel = c("rectangular", "traingular", "epanechnikov", "optimal"))
knn <- kknn(Target ~ ., train_data, test_data[,c(-1,-24)], k = 82, distance = 1, kernel = "opyimal")

mean(knn$fitted.values != test_data$Target)




#------------------------------------------------Clustering------------------------------------------

scaled_data <- scale(data.frame(classify[,c(4, 5, 7:18, 23, 24)]))

totwss <- vector()
btwss <- vector()
for (i in 2:6)
{
    set.seed(1234)
    temp <- kmeans(scaled_data, centers = i, iter.max = 25) #, algorithm = "Lloyd")
    totwss[i] <- temp$tot.withinssresul
    btwss[i] <- temp$betweenss
    print(i)
}


plot(totwss, xlab="Number of Cluster", type="b", ylab="Total Within Sum of Square")

plot(btwss, xlab="Number of Cluster", type="b", ylab="Total Between Sum of Square")


set.seed(1234)
result.cal <- kmeans(scaled_data, centers = 3)       

table(classify$Target, result.cal$cluster)

plot(classify[c("ConsecutiveYearsAsMember", "Purchase.Amt")], col = result.cal$cluster)
# plot cluster centers
points(result.cal$centers[,c("ConsecutiveYearsAsMember", "Purchase.Amt")], col = 1:3, pch = 8, cex=2)


library(fpc)
pamk.result <- pamk(scaled_data, usepam = FALSE)
# number of clusters
pamk.result$nc

table(pamk.result$pamobject$clustering, classify$Target)
