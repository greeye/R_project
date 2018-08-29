# 清空数据
rm(list=ls())
# 设置工作环境
setwd("Y:/DataSet/R/shock")
# 读取数据
data= read.csv("300DATA.csv",stringsAsFactors = F)
library(reshape2)
head(data)
date1="X2013Q1"
date2="X2013Q2"
date3="X2013Q3"

# 抽取数据
rawdata1 = data[c('factor','stock',date1)]
rawdata2 = data[c('factor','stock',date2)]
rawdata3 = data[c('factor','stock',date3)]
head(rawdata3)
# 长数据变为宽数据    value.var?
rawdata1 =dcast(rawdata1,stock~factor,value.var = date1)
rawdata2 =dcast(rawdata2,stock~factor,value.var = date2)
rawdata3 =dcast(rawdata3,stock~factor,value.var = date3)
head(rawdata1)
traindata = merge(rawdata1,rawdata2[c('stock','收盘�?','涨跌�?')],by='stock')
overata = traindata$涨跌�?.y-traindata[which(traindata$stock=='沪深300'),'涨跌�?.y']
traindata = cbind(traindata,overata)
# 删除空数�?
traindata=na.omit(traindata)
label=ifelse(traindata$overata>0,1,0)
traindata =cbind(traindata,label)
head(traindata)
traindata$label=as.factor(traindata$label)
traindata.svm=traindata[,-c(21,20,19,16)]

head(traindata.svm)
library(e1071)
svm.kernel=c("linear", "polynomial","sigmoid","radial")
for(i in 1:length(svm.kernel))
{
  svm.class=svm(label ~ ., data = traindata.svm[,-1],kernel=svm.kernel[i],class.weights=c('1' = 1, '0'=1.2),cross=50)
  svm.predictions <- predict(svm.class, traindata.svm[,c(-1,-18)])
  svm.agreement <- svm.predictions == traindata.svm$label   # 准确�?
  suss.table=table(svm.predictions, traindata.svm$label)    # 召回�?
  suss.prop=prop.table(table(svm.agreement)) 
  result=c(svm.kernel[i],suss.table[2,2]/sum(suss.table[2,]),suss.prop[2])
  print(result)
}

# 测试�?
testdata = merge(rawdata2,rawdata3[c('stock','收盘�?','涨跌�?')],by='stock')
overata = testdata$涨跌�?.y-testdata[which(testdata$stock=='沪深300'),'涨跌�?.y']
testdata = cbind(testdata,overata)
testdata=na.omit(testdata)
label=ifelse(testdata$overata>0,1,0)
testdata =cbind(testdata,label)
head(testdata)
testdata$label=as.factor(testdata$label)
testdata.svm=testdata[,-c(21,20,19,16)]
head(testdata.svm)


# 训练模型
svm.class = svm(label ~ .,data = traindata.svm[,-1],kernel = "radial",class.weights = c('1' = 1,'0'=1.2),cross=20)
svm.predictions <- predict(svm.class,testdata.svm[,c(-1,-18)])

svm.agreement <- svm.predictions == traindata.svm$label
suss.prop = prop.table(table(svm.agreement))
suss.table=table(svm.predictions, testdata.svm$label)    # 召回�?
result=c(suss.table[2,2]/sum(suss.table[2,]),suss.prop[2])

print(result)

# 导入混淆矩阵可视化包
#install.packages('gmodels')
library(gmodels)
CrossTable(testdata.svm$label,svm.predictions,prop.chisq = FALSE,prop.c = T,
           prop.r = T,dnn = c("actual","predicted"))
pool=cbind(testdata,svm.predictions)

callpool =pool[which(pool$svm.predictions=='1'),]
mean(callpool$overata)

####### 模型优化 ########
# 
p.cost =seq(from=0.1,to=3,by=0.01)
p.tolerance=seq(from=0.0005,to=0.01,by=0.001)
p.epsilon=seq(from=0.02,to=0.1,by=0.02)
# each 每个元素重复的次数，  time 每个向量重复的次�?
vp.cost=rep(p.cost,each=(length(p.tolerance)*length(p.epsilon)))
vp.tolerance=rep(rep(p.tolerance,times =length(p.cost)),each=length(p.epsilon))
vp.epsilon=rep(p.epsilon,times=(length(p.cost)*length(p.tolerance)))

# 合并成矩�?
param=cbind(vp.cost,vp.tolerance,vp.epsilon)
param =as.data.frame(param)

result=c()
for(i in 1:nrow(param))
{
  svm.class=svm(label ~ ., data = traindata.svm[,-1],kernel="radial",
                class.weights=c('1' = 1, '0'=1.2),cross=20,
                cost=param$vp.cost[i],tolerance=param$vp.tolerance[i]
                ,epsilon=param$vp.epsilon[i])
  svm.predictions <- predict(svm.class,  testdata.svm[,c(-1,-18)])
  svm.agreement <- svm.predictions == testdata.svm$label
  suss.table=table(svm.predictions, testdata.svm$label)
  suss.prop=prop.table(table(svm.agreement))
  #存储看涨胜率和总体胜率
  result=rbind(result,c(suss.table[2,2]/sum(suss.table[2,]),suss.prop[2]))
  print(i)#查看循环进度
}

svm.class=svm(label ~ ., data = traindata.svm[,-1],kernel="radial",
              class.weights=c('1' = 1, '0'=1.2),cross=20,
              cost=0.6,tolerance=0.0005,epsilon=0.02)
svm.predictions <- predict(svm.class, testdata.svm[,c(-1,-18)])
svm.agreement <- svm.predictions == testdata.svm$label
suss.table=table(svm.predictions, testdata.svm$label)
suss.prop=prop.table(table(svm.agreement))
print(c(suss.table[2,2]/sum(suss.table[2,]),suss.prop[2]))

pool=cbind(testdata,svm.predictions)
callpool=pool[which(pool$svm.predictions=='1'),]

