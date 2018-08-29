
library(AMORE)#载入包
rm(list = ls())
setwd("Y:/DataSet/R")
set.seed(1234)#随机数设置
samp.rate=0.7#设置训练集比例
p2pdata=read.csv("p2p_lending.csv")#导入数据
fun.dummy<- function(data)###哑变量构造函数
{
  name.level=levels(data)
  dummy=c()
  for(i in 1:(length(name.level)-1))
  {
    temp.dummy=ifelse(data==name.level[i],1,0)
    temp.dummy=as.numeric(temp.dummy)
    dummy=cbind(dummy,temp.dummy)
  }
  colnames(dummy)=name.level[1:(length(name.level)-1)]
  dummy=as.data.frame(dummy)
  return(dummy)
}
part.dummy=fun.dummy(p2pdata[,1])

for (i in 2:5)
{
  dummy=fun.dummy(p2pdata[,i])
  part.dummy=cbind(part.dummy,dummy)
}

#str(part.dummy)
p2pdata=cbind(part.dummy,p2pdata[,6:19])#将哑变量与数值型数据合并
#将整数型数据转为数值型
p2pdata$周期_月=as.numeric(p2pdata$周期_月)
p2pdata$借款描述字数=as.numeric(p2pdata$借款描述字数)
p2pdata$累积已还次数=as.numeric(p2pdata$累积已还次数)
p2pdata$累积逾期次数=as.numeric(p2pdata$累积逾期次数)
p2pdata$安全标=as.numeric(p2pdata$安全标)
#数据归一化（数据压缩到0与1之间）
min.vec=apply(p2pdata[,16:28],2,min)
max.vec=apply(p2pdata[,16:28],2,max)
range.vec=max.vec-min.vec
std=p2pdata[,16:28]
for(i in 1:ncol(std))
{
  std[,i]=(std[,i]-min.vec[i])/range.vec[i]
}
#得到归一化以后的数据clean.data
clean.data=p2pdata
clean.data[,16:28]=std
#查看归一化效果
apply(clean.data,2,max)
#抽样
samp.index=sample(1:nrow(clean.data),size=floor(samp.rate*nrow(clean.data)))
#生成训练集与测试集
train=clean.data[samp.index,]
test=clean.data[-samp.index,]

## 模型优化调参 ##
#  1、调参对象  n.neurons = c(28,x,1)
# learning.rate.global= y  momentum.global = z
# 
#
# output.layer 设置输出层神经元采用的激励函数 purelin、tansig、sigmoid、hardlim
# method ADAPTgd : 自适应梯度下降法；
#        ADAPTgdwm : 含有动量的自适应梯度下降法；
#        BATCHgd : 批梯度下降；
#        BATCHgdwm : 含有动量的批梯度下降法。
net.errorcriterium= c('LMS','LMLS','TAO')
net.hiddenlayer=c("purelin","tansig","sigmoid","hardlim")
net.outlayer=c("purelin","tansig","sigmoid","hardlim")
net.method=c("ADAPTgd","ADAPTgdwm","BATCHgd","BATCHgdwm")


dataframe=data.frame("hiddenlayer","outlayer","method","ss",
                     stringsAsFactors = FALSE
                     )
p.hidden=seq(from=5,to=18,by=1)
p.rate=seq(from=0.00001,to=0.0001,by=0.00001)
p.momentum=seq(from=0.0001,to=0.001,by=0.0001)

vp.hidden=rep(p.hidden,each=(length(p.rate)*length(p.momentum)))
vp.rate=rep(rep(p.rate,times=length(p.hidden)),each=length(p.momentum))
vp.momentum=rep(p.momentum,time=(length(vp.rate)*length(p.momentum)))

param= cbind(vp.hidden,vp.rate,vp.momentum)
param=as.data.frame(param)

#for (i in 1:length(net.hiddenlayer)){
 # for (j in 1:length(net.outlayer)){
  #  for (z in 1:length(net.method)){
      for (x in 1:nrow(param)){
        
      net=newff(n.neurons = c(28,param$vp.hidden[x],1),learning.rate.global = param$vp.rate[x],momentum.global = param$vp.momentum[x],
                error.criterium = "LMS", hidden.layer = "tansig",
                output.layer ="purelin" , method="ADAPTgdwm" )
      # 训练模型
      model=train(net,train[,-29],train[,29],error.criterium = "LMS",
          report=T,show.step=100,n.show=10)
      test.predict=sim(model$net,test[,-29])
      test.class=ifelse(test.predict>0.85,1,0)
      table(test.class,test[,29])
      #hiddenlayer=as.character(net.hiddenlayer[i])
      #outlayer=as.character(net.outlayer[j])
      #method=as.character(net.method[z])
      ss = as.numeric(sum(diag(table(test.class,test[,29])))/nrow(test))
      
      result= c("hiddenlayer","outlayer","method",ss)
      dataframe=rbind(dataframe,result)
    }
#  }
#}
#}
# 长数据 转成 宽数据
#library(reshape2)
#result2=dcast(result,,value.var = x)










#net=newff(n.neurons = c(28,10,1),learning.rate.global = 0.0001,momentum.global = 0.001,
#error.criterium = "LMS", hidden.layer = "tansig",
#output.layer = "purelin", method="ADAPTgdwm")
#model=train(net,train[,-29],train[,29],error.criterium = "LMS",
#report=T,show.step=100,n.show=10)
#test.predict=sim(model$net,test[,-29])
#test.class=ifelse(test.predict>0.85,1,0)
#table(test.class,test[,29])
#sum(diag(table(test.class,test[,29])))/nrow(test)






