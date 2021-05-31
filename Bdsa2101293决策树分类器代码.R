#载入包
install.packages('lubridate')
iinstall.packages('pROC')
iinstall.packages('rpart.plot')
library(lubridate)
library(pROC) 
library(rpart.plot)

#预处理数据
options(digits=20)#将数字精度设置为20位
user_info=read.csv('user_info.csv',sep=',',header=TRUE,encoding='UTF-8')
login_day=read.csv('login_day.csv',sep=',',header=TRUE,encoding='UTF-8')
visit_info=read.csv('visit_info.csv',sep=',',header=TRUE,encoding='UTF-8')
result = read.csv('result.csv',sep=',',header=TRUE,encoding='UTF-8')
sum(result)
colnames(result)[1]='user_id'
colnames(user_info)[1]='user_id'
colnames(visit_info)[1]='user_id'
user_info=user_info[,c(1,2,3,4,6,7,8)]  #删除city_num的列
user_info=user_info[user_info$age_month<=144,]  # 年龄限制在12周岁内
user_info1=merge(user_info,login_day,by=c('user_id'))
user_info2=merge(user_info1,visit_info,by=c('user_id'))
user_info2$first_order_time=year(as.Date(user_info2$first_order_time))#提取年份
user_info2$result=0
for (i in 1:nrow(user_info2)){
  if (user_info2$user_id[i] %in% result$user_id){
    user_info2$result[i]=1
  }
}
sum(user_info2$result)
alldata = user_info2
alldata$result = as.factor(alldata$result)

#过采样
hang = which(alldata$result == 1,)
set.seed(1234)
hang1 = sample(hang,120000,replace = TRUE)
newdata = alldata[hang1,]
alldata1 = rbind(alldata,newdata)
nrow(alldata1)
nrow(newdata)
id = alldata[,1]
alldata1 = alldata1[,-1]

#随机生成训练集（70%）和测试集（30%）
set.seed(1234)
traingroupid = sample(nrow(alldata1),ceiling(nrow(alldata1)*0.7),replace = F)
w = alldata1[traingroupid,]#训练集
testgroup = alldata1[-traingroupid,]#测试集
nrow(w)
nrow(testgroup)

#构建决策树
b=rpart(result~.,w)
b
par(mfrow = c(1,1))
rpart.plot(b,extra = 1,type = 2)
a =predict(b,w,type = 'class')
a1 = predict(b,testgroup,type = 'class')
predict1 = predict(b,alldata,type = 'class')


table(w$result,a)#训练集混淆矩阵
table(testgroup$result,a1)#测试集混淆矩阵
mean(w$result!= a)#训练集误判率
mean(testgroup$result!= a1)#测试集误判率
mean(predict1!=alldata$result)#基于有效样本误判率
table(alldata$result,predict1)#基于有效样本混淆矩阵

#查看cp值
printcp(b)

#画ROC曲线
a1 = as.numeric(a1)
testgroup$result = as.numeric(testgroup$result)
modelroc <- roc(a1,testgroup$result) 
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", print.thres=TRUE)

plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,
     grid=c(0.1,0.2),grid.col=c("green","red"),
     max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)

#保存预测结果
predict1 = as.matrix(as.numeric(predict1)-1,nrow(alldata),1)
options(digits=20)
sample_out = cbind(id,predict1)
colnames(sample_out) = c('user_id','result')
head(sample_out)
write.csv(as.matrix(sample_out),'sample_output.csv',quote = FALSE,row.names=F)

