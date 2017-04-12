library(sqldf)
data<-table_1_
data$totalpractions<-0
data$totalactions[is.na(data$totalactions)]<-0
data$avgactions<-0
data$practions<-0
data$days_since<-0

#create email count
data$mailCount<-NA
data$mailCount[1]<-1

for(i in 2:(nrow(data))){
  if (data$email_address[i]==data$email_address[i-1]){
    data$mailCount[i]<-data$mailCount[i-1]+1
  }else{
    data$mailCount[i]<-1
  }
}

#days of subscription
data$daysSub<-NA
data$daysSub<-as.numeric((data$send_time-data$timestamp_opt)/(60*60*24))

#total previous actions
for (i in 1:(nrow(data)-1)) {
  if (data$mailCount[i+1] !=1){
    data$totalpractions[i+1]<-data$totalactions[i]
  }else{
    data$totalpractions[i+1]<-0
  }
}

#actions per email until now
for (i in 2:(nrow(data))) {
  if (data$email_address[i]==data$email_address[i-1]){
    data$practions[i]<-data$practions[i-1]+data$totalpractions[i]
  }
}
for (i in 2:(nrow(data))) {
  if (data$email_address[i]==data$email_address[i-1]){
    data$avgactions[i]<-(data$practions[i-1]+data$totalpractions[i])/data$mail_id[i-1]
  }
}

#days since last email was sent
for (i in 1 :nrow(data)) {
  if (data$mail_id[i] !=1){
    data$days_since[i]<-data$send_time[i]-data$send_time[i-1]
  } 
}

#status
data$status<-NA
data$status<-1
data$status[is.na(data$timestamp_out)]<-0

#personal-business emails
mailService<-sapply(strsplit(as.character(data$email_address),'@',fixed=TRUE), `[`, 2)
data$mailService<-sapply(strsplit(as.character(mailService),'.',fixed=TRUE), `[`, 1)
data$personalMail<-0

mailProviders<-c("gmail", "zoho", "outlook", "yahoo", "gmx", "yandex", "hushmail", "aol")
for (i in 1:length(data$mailService)) {
  if (data$mailService[i] %in% mailProviders){
    data$personalMail[i]<-1
    i=i+1
  }
}
data$timestamp_opt[is.na(data$timestamp_opt)]<-Sys.Date()

#remove rows with NAs in list and campaign
data<-data[!with(data,is.na(list_id)& is.na(campaign_id)),]
query<-"select distinct campaign_id, send_time from data"
campaigns<-sqldf(query)

#remove campaigns with no info
data<-data[!with(data,is.na(send_time)),]

#keep only columns with no NAs
data<-data[colSums(!is.na(data)) >= dim(data)[1]]
sapply(data,function(x) sum(is.na(x)))

#train and test dataset

##create empty dataframes with the same structure as data
test<-data[0,]
train<-data[0,]
for (i in 1:nrow(data)-1) {
  if (data$mailCount[i+1] ==1){
    test<-rbind(test,data[i,])
  }else {
    train<-rbind(train,data[i,])
  }
}

#initial look 
summary(data)

#mail_id and status
subs.tr<-data[data$status==0,]
unsubs.tr<-data[data$status==1,]
ggplot(as.data.frame(data$status), aes(as.data.frame(data$mailCount), fill = "steelblue1", colour = "steelblue1")) +
  geom_density(alpha = 0.1)
par(mfrow=c(1,2))
ggplot(as.data.frame(unsubs.tr$status), aes(as.data.frame(unsubs.tr$mailCount), fill = "steelblue1", colour = "steelblue1")) +
  geom_density(alpha = 0.1)
ggplot(as.data.frame(subs.tr$status), aes(as.data.frame(subs.tr$mailCount), fill = "steelblue1", colour = "steelblue1")) +
  geom_density(alpha = 0.1)
ggplot(as.data.frame(data$status), aes(as.data.frame(log(data$mailCount+1)), fill = "steelblue1", colour = "steelblue1")) +
  geom_density(alpha = 0.1)
### 1o pick 3 mails, 2o pick 16, after 63rd mail very loyal

#Histogram for mailing lists
list_sub <- data.frame(table(data$list_id,data$status))
names(list_sub) <- c("List_id","churn","Count")
ggplot(data=list_sub, aes(x=List_id, y=Count, fill=status, alpha=0.1)) + geom_bar(stat="identity")

#separation numeric-non numeric variables
train$mail_id<-as.numeric(train$mail_id)
train.n<-sapply(train,class)=='numeric'
trainNum<-train[,train.n]

test$mail_id<-as.numeric(test$mail_id)
test.n<-sapply(test,class)=='numeric'
testNum<-test[,test.n]

#correlation plot
corr<-cor(trainNum)
corrplot(corr, type = "lower", tl.pos = "ld")

#lasso
df<-data.matrix(trainNum)
lasso<-glmnet(as.matrix(trainNum[,-9]),trainNum$status)
plot.glmnet(lasso, label=T)

crossVal<-cv.glmnet(as.matrix(trainNum[,-9]),trainNum$status) # 10-fold cross validation ensures the model is not over fitted
coefs<- coef(crossVal, s="lambda.1se")
coefs

#logistic regression
model0<- glm(as.factor(status)~mailCount+as.factor(list_id)+as.factor(personalMail)+location_latitude, family=binomial(link='logit'), data=train)
summary(model0)

#decision tree
DataTreeM <- rpart(status~., data=train[,-13])
varImp(DataTreeM)
plot(DataTreeM)
text(DataTreeM)
predictionTreeM <- predict(DataTreeM, test)