###############################Linear_Model#############################################
#Part1: get familiar with your data 
#Part2: split into three data
#Part3: Profiling:(see worksheet 'Profile' in Excel)
#Part4: Means(see worksheet 'Means' in Excel)
#Part5: Recoding (see 'Recoding_Logistic_Model' txt file)
#Part6: Correlation(see worksheet 'Variable Correlation' in Excel)
#Part7: Modeling(see worksheet 'Final Model Evaluation' in Excel)
#Part8: Model evaluation(see worksheet 'Final Model Evaluation' in Excel)

######################################################################################
#working location
rm(list = ls())
setwd("Y:/DataSet/R/Regression_Model/Linear_Model") #change the location
getwd()      #check the location
list.files() #list the files under your location

#########################################################################################################################################
############################################################  Part1:get familiar with your data  ########################################
#########################################################################################################################################

#The three data already get prepared#
file_path<-"Y:/DataSet/R/Regression_Model/Linear_Model/data_revenue_model.csv"  #change the location

# read in data
raw<-read.csv(file_path,stringsAsFactors = F)#read in your csv data
str(raw)      #check the varibale type

View(table(raw$dv_response))
quantile(raw$dv_revenue,(1:20)/20)
View(table(raw$segment))
View(prop.table(table(raw$segment)))

#Separate Build Sample
train<-raw[raw$segment=='build',] #select build sample
View(table(train$segment))        #check segment
#Separate invalidation Sample
test<-raw[raw$segment=='inval',]  #select invalidation(OOS) sample
View(table(test$segment))         #check segment

#Separate out of validation Sample
validation<-raw[raw$segment=='outval',] #select out of validation(OOT) sample
View(table(validation$segment))         #check segment

#########################################################################################################################################
############################################################  Part3: Profiling  #########################################################
#########################################################################################################################################

#overall performance
overall_cnt=nrow(train)                      #calculate the total count
overall_revenue=sum(train$dv_revenue)        #calculate the total revenue
Average_revenue=overall_revenue/overall_cnt  #calculate the average revenue
overall_perf<-c(overall_count=overall_cnt,overall_revenue=overall_revenue,Average_revenue=Average_revenue)  #combine
View(t(overall_perf))  #take a look at the summary

########### 3.1 Profiling for category variables('Profile' tab in excel) 
prof<-ddply(train,.(hh_gender_m_flag),summarise,cnt=length(rid),rev=sum(dv_revenue))  #group by hh_gender_m_flg
View(prof)   #check the result
prof1<-within(prof,
              {AVG_REVENUE<-rev/cnt 
              index<-AVG_REVENUE/Average_revenue*100
              percent<-cnt/overall_cnt
              })  #add response_rate,index, percentage

View(prof1) #check the result


########### 3.2 Profiling for numeric variables('Profile' tab in excel) 

#select the numeric variable you want to do analysis
# subset   
var_data=subset(train,select=c(pos_revenue_base_sp_6mo,dv_revenue)) #select the variable and Y
# summary  
summary(var_data$pos_revenue_base_sp_6mo )  # check the distribution


#separate to 2 parts: missing,nomissing

nomissing<-data.frame(var_data[!is.na(var_data$pos_revenue_base_sp_6mo),]) #select the no missing value records 
missing<-data.frame(var_data[is.na(var_data$pos_revenue_base_sp_6mo),])    #select the missing value records

##################3.2.1 numeric Profiling:missing part 

missing2<-ddply(missing,.(pos_revenue_base_sp_6mo),summarise,cnt=length(dv_revenue),rev=sum(dv_revenue)) #group by pos_revenue_base_sp_6mo
View(missing2)

missing_perf<-within(missing2,
                     {avg_revenue<-rev/cnt 
                     index<-avg_revenue/Average_revenue*100
                     percent<-cnt/overall_cnt
                     var_category<-c('unknown')
                     })  #summary

View(missing_perf)

##################3.2.2 numeric Profiling:Non-missing part   

nomissing_value<-nomissing$pos_revenue_base_sp_6mo  #put the nomissing values into a variable
# cut 
nomissing$var_category<-cut(nomissing_value,unique(quantile(nomissing_value,(0:10)/10)),include.lowest = T) #separte into 10 groups
View(table(nomissing$var_category))  #take a look at the 10 category
prof2<-ddply(nomissing,.(var_category),summarise,cnt=length(dv_revenue),rev=sum(dv_revenue))#group by the 10 groups
View(prof2)
nonmissing_perf<-within(prof2,
                        {avg_revenue<-rev/cnt 
                        index<-avg_revenue/Average_revenue*100
                        percent<-cnt/overall_cnt
                        })#add avg_revenue,index,percent
View(nonmissing_perf)

#set missing_perf and non-missing_Perf together

pos_revenue_base_sp_6mo_perf<-rbind(nonmissing_perf,missing_perf[,-1]) #set 2 data together
View(pos_revenue_base_sp_6mo_perf)

########### 3.3 Profiling for numeric variables('pos_revenue_total_6mo') 
# 3.3.1 pos_revenue_total_6mo
var_data=subset(train,select=c(pos_revenue_total_6mo,dv_revenue)) #select the variable and Y
# summary  
summary(var_data$pos_revenue_total_6mo)  # check the distribution

#separate to 2 parts: missing,nomissing
nomissing<-data.frame(var_data[!is.na(var_data$pos_revenue_total_6mo),]) #select the no missing value records 
missing<-data.frame(var_data[is.na(var_data$pos_revenue_total_6mo),])    #select the missing value records

#  missing
missing2<-ddply(missing,.(pos_revenue_total_6mo),summarise,cnt=length(dv_revenue),rev=sum(dv_revenue)) #group by pos_revenue_base_sp_6mo
View(missing2)

missing_perf<-within(missing2,
                     {avg_revenue<-rev/cnt 
                     index<-avg_revenue/Average_revenue*100
                     percent<-cnt/overall_cnt
                     var_category<-c('unknown')
                     })  #summary

View(missing_perf)

#  nomissin

nomissing_value<-nomissing$pos_revenue_total_6mo  #put the nomissing values into a variable
# cut unique(quantile(nomissing_value,(0:10)/10))  
nomissing$var_category<-cut(nomissing_value,unique(quantile(nomissing_value,(0:10)/10)),include.lowest = T) #separte into 10 groups
View(table(nomissing$var_category))  #take a look at the 10 category
prof2<-ddply(nomissing,.(var_category),summarise,cnt=length(dv_revenue),rev=sum(dv_revenue))#group by the 10 groups
View(prof2)
nonmissing_perf<-within(prof2,
                        {avg_revenue<-rev/cnt 
                        index<-avg_revenue/Average_revenue*100
                        percent<-cnt/overall_cnt
                        })#add avg_revenue,index,percent
View(nonmissing_perf)

# rbind
pos_revenue_total_6mo<-rbind(nonmissing_perf,missing_perf[,-1]) #set 2 data together
View(pos_revenue_total_6mo)

#########################################################################################################################################
############################################################  Part4: Means  #############################################################
#########################################################################################################################################

#4.1em_months_last_open

# extract 1em_months_last_open: 
mean_var<-train$em_months_last_open #select the variable you want to do analysis

mean_em_months_last_open<-c(
  var="em_months_last_open",
  mean=mean(mean_var,na.rm=TRUE) ,
  median=median(mean_var,na.rm=TRUE) ,
  quantile(mean_var,c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.99,1),na.rm=TRUE),
  nmiss=sum(is.na(mean_var))
)# add the parameter you want
View(t(mean_em_months_last_open)) #take a look at the result

#extract: hh_income
mean_hh_income <- train$hh_income  #select the variable you want to do analysis

mean_hh_income_last_open <- c(
  var="hh_income",
  mean=mean(mean_hh_income,na.rm = TRUE),
  median = median(mean_hh_income,na.rm = TRUE),
  quantile(mean_hh_income,c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.99,1),na.rm=TRUE),
  nmiss = sum(is.na(mean_hh_income))
)
View(t(mean_hh_income_last_open)) #take a look at the result

#########################################################################################################################################
############################################################  Part5: Recoding  ####################################################### 
######################################################################################################################################### 

#2 ´¦Àí
train$m3_pos_revenue_base_sp_6mo <- ifelse(is.na(train$pos_revenue_base_sp_6mo) == T, 294.16,     #when pos_revenue_base_sp_6mo is missing ,then assign 294.16
                                           ifelse(train$pos_revenue_base_sp_6mo <= 0, 0,                   #when pos_revenue_base_sp_6mo<=0 then assign 0
                                                  ifelse(train$pos_revenue_base_sp_6mo >=4397.31, 4397.31, #when pos_revenue_base_sp_6mo>=4397.31 then assign 4397.31
                                                         train$pos_revenue_base_sp_6mo))
)                  #when 0<pos_revenue_base_sp_6mo<4397.31 and not missing then assign the raw value

summary(train$m3_pos_revenue_base_sp_6mo)  #do a summary
summary(train$m2_POS_REVENUE_BASE_SP_6MO)  #do a summary

########   em_count_valid  #######

# 1 analysis  em_count_valid
mean_var = train$em_count_valid
# 2 count mean,median,quantile,nmiss
mean_em_count_valid_last_open <- c(
  var="hh_income",
  mean=mean(mean_var,na.rm = TRUE),
  median = median(mean_var,na.rm = TRUE),
  quantile(mean_var,c(0,0.01,0.1,0.25,0.5,0.75,0.9,0.99,1),na.rm=TRUE),
  nmiss = sum(is.na(mean_var))
)

View(t(mean_em_count_valid_last_open)) #take a look at the result

# 3 change data 
train$em_count_valid <- ifelse(train$em_count_valid <= 0,1,ifelse(train$em_count_valid >=15,15,
                                                                  train$em_count_valid       
))

#########################################################################################################################################
############################################################  Part6: Correlation  ####################################################### 
#########################################################################################################################################

#pairwise correlation
#install.packages("picante")  #install package picante, you need to install it for 1 time only
library(picante)  #call picante
var_list<-c('dv_revenue','m2_POS_REVENUE_BASE',
            'm2_POS_LAST_TOTAL_REVENUE',
            'm2_POS_REVENUE_BASE_SP_6MO',
            'm2_POS_REVENUE_TOTAL',
            'm2_POS_REVENUE_TOTAL_6MO',
            'm2_SQ_POS_MARGIN_TOTAL_12MO',
            'm2_SQ_POS_SP_QTY_24MO',
            'm2_POS_MARGIN_TOTAL',
            'm2_POS_MARGIN_TOTAL_12MO',
            'm2_POS_SP_QTY_24MO',
            'm2_EM_COUNT_VALID',
            'm2_SH_MNTHS_LAST_INQUIRED',
            'm2_IID_COUNT',
            'm2_EM_NUM_OPEN_30',
            'm2_SH_INQUIRED_LAST12MO',
            'm2_WEB_MNTHS_SINCE_LAST_SES',
            'm2_WEB_CNT_IID_ACCESSED',
            'm2_HH_AGE',
            'm2_SH_INQUIRED_LAST3MO',
            'm2_POS_MNTHS_LAST_ORDER',
            'm2_POS_NUM_ORDERS',
            'm2_POS_TOT_REVPERSYS',
            'm2_EX_AUTO_USED0005_X1',
            'm2_EM_MONTHS_LAST_OPEN',
            'm2_POS_NUM_ORDERS_24MO',
            'm2_HH_INCOME')  #put the variables you want to do correlation analysis here
####6.1  all variables correlation

# select all the variables you want to do correlation analysis
corr_var<-train[, var_list]  
str(corr_var)                #check the variable type
# do the correlation
correlation<-data.frame(cor.table(corr_var,cor.method = 'pearson')) 
View(correlation)

cor_only=data.frame(row.names(correlation),correlation[, 1:ncol(corr_var)])  #select correlation result only
View(cor_only)

#write the result out
##6.1.1 write to csv
library("xlsx")
write.csv(cor_only,file = 'Correlation.csv')

##6.1.2 write to excel
#install.packages("XLConnect")  
# require(XLConnect)
# xlsx <- loadWorkbook('Correlation.xlsx',create=TRUE) 
# createSheet(xlsx,name='Correlation')  #name the worksheet as 'correlation'
# writeWorksheet(xlsx,cor_only,'Correlation',startRow=1,startCol=1, header=TRUE)  #define the startrow,startcol,header
# saveWorkbook(xlsx)

####6.2  2 variables correlation
cor.test(corr_var[,2],corr_var[,3])  #select the second and thrid variables to do correlation


#########################################################################################################################################
############################################################   Part7: Modeling  ####################################################### 
#########################################################################################################################################

var_list<-c(
  'm2_POS_REVENUE_BASE',
  'm2_POS_LAST_TOTAL_REVENUE',
  'm2_POS_MNTHS_LAST_ORDER',
  'm2_POS_REVENUE_BASE_SP_6MO',
  'm2_POS_SP_QTY_24MO',
  'm2_POS_TOT_REVPERSYS',
  'm2_WEB_MNTHS_SINCE_LAST_SES',
  'm2_SH_MNTHS_LAST_INQUIRED' 
)  #put the variables you want to try in model here


#exercise1: try other variables
var_list1<-c(
  'm2_POS_REVENUE_BASE',
  'm2_POS_LAST_TOTAL_REVENUE',
  'm2_POS_REVENUE_TOTAL',
  'm2_POS_REVENUE_TOTAL_6MO',
  'm2_POS_MARGIN_TOTAL',
  'm2_POS_MARGIN_TOTAL_12MO',
  'm2_POS_SP_QTY_24MO',
  'm2_EM_COUNT_VALID',
  'm2_IID_COUNT',
  'm2_EM_NUM_OPEN_30',
  'm2_SH_INQUIRED_LAST12MO',
  'm2_WEB_CNT_IID_ACCESSED',
  'm2_SH_INQUIRED_LAST3MO',
  'm2_POS_MNTHS_LAST_ORDER',
  'm2_POS_NUM_ORDERS',
  'm2_POS_TOT_REVPERSYS',
  'm2_POS_NUM_ORDERS_24MO',
  'm2_HH_INCOME',
  'm2_EM_MONTHS_LAST_OPEN',
  'm2_HH_AGE',
  'm2_WEB_MNTHS_SINCE_LAST_SES',
  'm2_POS_REVENUE_BASE_SP_6MO',
  'm2_SH_MNTHS_LAST_INQUIRED'
  
)  #put the variables you want to try in model here

var_list2<-c(
  'm2_POS_REVENUE_BASE',
  'm2_POS_NUM_ORDERS',
  'm2_POS_REVENUE_TOTAL',
  'm2_POS_LAST_TOTAL_REVENUE',
  'm2_POS_MNTHS_LAST_ORDER',
  'm2_POS_MARGIN_TOTAL',
  'm2_POS_SP_QTY_24MO',
  'm2_POS_REVENUE_BASE_SP_6MO',
  'm2_POS_TOT_REVPERSYS',
  'm2_SH_MNTHS_LAST_INQUIRED',
  'm2_WEB_MNTHS_SINCE_LAST_SES'
  
)  #put the variables you want to try in model here

var_list3<-c(
  'm2_POS_REVENUE_BASE',
  'm2_POS_NUM_ORDERS',
  'm2_POS_LAST_TOTAL_REVENUE',
  'm2_POS_MNTHS_LAST_ORDER',
  'm2_POS_REVENUE_BASE_SP_6MO',
  'm2_POS_SP_QTY_24MO',
  'm2_POS_TOT_REVPERSYS',
  'm2_SH_MNTHS_LAST_INQUIRED',
  'm2_WEB_MNTHS_SINCE_LAST_SES'
  
)  #put the variables you want to try in model here

#exercise2:If the boss wants to add m2_HH_INCOME varible into the model list
var_list4<-c(
  'm2_POS_REVENUE_BASE',
  'm2_POS_NUM_ORDERS',
  'm2_POS_LAST_TOTAL_REVENUE',
  'm2_POS_MNTHS_LAST_ORDER',
  'm2_POS_REVENUE_BASE_SP_6MO',
  'm2_POS_SP_QTY_24MO',
  'm2_POS_TOT_REVPERSYS',
  'm2_SH_MNTHS_LAST_INQUIRED',
  'm2_WEB_MNTHS_SINCE_LAST_SES',
  'm2_HH_INCOME'
  
)  #put the variables you want to try in model here

mods<-train[,c('dv_revenue',var_list1)] #select Y and varibales you want to try

model_lm<-lm(dv_revenue~.,data=mods)   #regression model

#Stepwise
library(MASS)
model_sel<-stepAIC(model_lm,direction ="both")  #using both backward and forward stepwise selection
summary<-summary(model_sel) #summary
model_summary<-data.frame(var=rownames(summary$coefficients),summary$coefficients) #do the model summary

View(model_summary)

#Variable VIF
require(car)  #call Car
vif=data.frame(vif(model_sel)) #get Vif
var_vif=data.frame(var=rownames(vif),vif) #get variables and corresponding Vif
View(var_vif)

#R2
summary2<-summary(model_sel) #get model summary
(R2<-data.frame(r2=summary2$r.squared,adj_r2=summary2$adj.r.squared)) #get R2

#variable correlation
cor<-data.frame(cor.table(mods,cor.method = 'pearson'))              #calculate the correlation
correlation<-data.frame(variables=rownames(cor),cor[, 1:ncol(mods)]) #get correlation only
#View(correlation)

#Output result for analysis

#output to csv
write.csv(model_summary,file = 'model_summary.csv')
write.csv(var_vif, file = 'var_vif.csv')
write.csv(correlation, file = 'correlation.csv')


#output to excel
#if (file.exists('model_tried_result.xlsx')) file.remove('model_tried_result.xlsx')
#xlsx <- loadWorkbook('model_tried_result.xlsx',create=TRUE) 
#createSheet(xlsx,name='model_tried3')
#model_summary
#writeWorksheet(xlsx,model_summary,'model_tried3',startRow=1,startCol=1, header=T,rownames=T)
#R2
#writeWorksheet(xlsx,R2,'model_tried3',startRow=20,startCol=1, header=T,rownames=T)
#VIF
#writeWorksheet(xlsx,var_vif,'model_tried3',startRow=2,startCol=7, header=T,rownames=T)
#Correlation
#writeWorksheet(xlsx,correlation,'model_tried3',startRow=20,startCol=4, header=T,rownames=T)
#saveWorkbook(xlsx)

#########################################################################################################################################
############################################################  Part8: Model Evaluation  ########################################################
#########################################################################################################################################

# Model evaluation
library(ROCR)
# lift Chart: train 
# predict result
pred <- predict(model_lm,train,type='response') 
decile <- cut(pred,unique(quantile(pred,(0:10)/10)),labels=10:1, include.lowest = T)

# decile
sum<-data.frame(actual=train$dv_revenue,pred=pred,decile=decile)  #put actual Y,predicted Y,Decile together
View(sum)
#sums<- cbind(actual=train$dv_revenue,pred,decile)
#View(sums)
decile_sum <- ddply(sum,.(decile),summarize,cnt=length(actual),rev=sum(actual))
decile_sum2<-within(decile_sum,
                    {rev_avg<-rev/cnt
                    index<-100*rev_avg/(sum(rev)/sum(cnt))
                    })  #add resp_rate,index
View(decile_sum2)
# decreased sort    decile_sum2(order(decile_sum2[,i]£¬decreasing = T),)
# [row,column]
decile_sum3 <- decile_sum2[order(decile_sum2[,1],decreasing = T),]
View(decile_sum3)
ss <- summary(model_lm)
#R2
summary2<-summary(model_sel) #get model summary
(R2<-data.frame(r2=summary2$r.squared,adj_r2=summary2$adj.r.squared)) #get R2


