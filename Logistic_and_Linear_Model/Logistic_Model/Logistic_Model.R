################### Logistic Model ###################
# author : greeye
# date   : 2018.9.2

#Part1: get familiar with your data 
#Part2: split into three data
#Part3: Profiling:(see worksheet 'Profile' in Excel)
#Part4: Means(see worksheet 'Means' in Excel)
#Part5: Recoding (see 'Recoding_Logistic_Model' txt file)
#Part6: Correlation(see worksheet 'Variable Correlation' in Excel)
#Part7: Modeling(see worksheet 'Final Model Evaluation' in Excel)
#Part8: Model evaluation(see worksheet 'Final Model Evaluation' in Excel)

###########################################################################################################
#working location
rm(list = ls())     #  clear al vaviables
setwd("Y:/DataSet/R/Regression_Model/Logistic_Model") # change the location
getwd()      # check the location
list.files() # list the files under your location


#########################################################################################################################################
############################################################  Part1:get familiar with your data  ########################################
#########################################################################################################################################

file_path  = "data_response_model.csv"
raw <- read.csv(file_path,stringsAsFactors = F)
str(raw)
View(raw) 
summary(raw)

#response varible 
View(table(raw$dv_response))              # Y
View(prop.table(table(raw$dv_response)))  # Y frequency

#########################################################################################################################################
############################################################  Part2: split into three data  #############################################
#########################################################################################################################################

# split data 
set.seed(11111)
randRows <- sample(3,nrow(raw),replace = TRUE,prob = c(0.5,0.3,0.2))
# split train data and test data
train <- raw[randRows==1,]
test <- raw [randRows==2,]
validation <- raw[randRows==3,]
View(train)
View(test)
View(validation)

# modeling segments (There isn't this step when you build the model)
train<-raw[raw$segment=='build',]  #select build sample, it should be random selected when you build the model
test<-raw[raw$segment=='inval',] #select invalidation(OOS) sample
validation<-raw[raw$segment=='outval',] #select out of validation(OOT) sample

#########################################################################################################################################
############################################################  Part3: Profiling  #########################################################
#########################################################################################################################################

#overall performance
overall_cnt=nrow(train)                      #calculate the total count
overall_revenue=sum(train$dv_revenue)        #calculate the total revenue
Average_revenue=overall_revenue/overall_cnt  #calculate the average revenue
overall_perf<-c(overall_count=overall_cnt,overall_revenue=overall_revenue,Average_revenue=Average_revenue)  #combine
View(t(overall_perf))  #take a look at the summary

library('plyr')
prof<-ddply(train,.(hh_gender_m_flag),summarise,cnt =length(rid),rev = sum(dv_revenue))
View(prof)

prof1 <-within(prof,
        {AVG_REVENUE <- rev/cnt
         index <- AVG_REVENUE/Average_revenue * 100
         percent <- cnt/overall_cnt
        }  # add response_rate,index,percentage
 )
 View(prof1)
 
 ### 2 hh_gender_m_flag
 prof<-ddply(train,.(hh_gender_m_flag),summarise,cnt=length(rid),res=sum(dv_response)) #group by hh_gender_m_flg
 View(prof)  #check the result
 
 # within () 
 prof2 <-within(prof,
                {AVG_REVENUE <- res/cnt
                index <- AVG_REVENUE/Average_revenue * 100
                percent <- cnt/overall_cnt
                }  
 )
 View(prof2)
 
 ### 3 ex_auto_used0005
 prof<-ddply(train,.(ex_auto_used0005),summarise,cnt=length(rid),res=sum(dv_response)) #group by hh_gender_m_flg
 View(prof)  #check the result
 # within 
 prof2<-within(prof,{res_rate<-res/cnt 
 index<-res_rate/Average_revenue*100
 percent<-cnt/overall_cnt
 })  #add response_rate,index, percentage
 View(prof2)   #check the result

 #########################################################################################################################################
 ############################################################  Part4: Means  #############################################################
 #########################################################################################################################################

 mean_var <- train$em_months_last_open 
 
 mean_em_months_last_open <-c(
   var = 'mean_em_months_last_open',
   mean = mean(mean_var,na.rm = TRUE),
   median = median(mean_var,na.rm = TRUE),
   minimum = min(mean_var,na.rm = TRUE),
   quantile(mean_var,na.rm = TRUE),
   nmiss =sum(is.na(mean_var)),
   maximum = max(mean_var,na.rm = TRUE)
 )
 View(mean_em_months_last_open)
 
 #########################################################################################################################################
 ############################################################  Part5: Recoding  ####################################################### 
 ######################################################################################################################################### 
 
 ######### 5.1 numeric variables ######### 
 train$m2_em_count_valid <- ifelse(is.na(train$em_count_valid) == T, 2,      #when em_count_valid is missing ,then assign 2 
                                   ifelse(train$em_count_valid <= 1, 1,         #when em_count_valid<=1 then assign 1 
                                          ifelse(train$em_count_valid >=10, 10, #when em_count_valid>=10 then assign 10
                                                 train$em_count_valid)))        #when 1<em_count_valid<10 and not missing then assign the raw value capping floor&missing value fill
 
 summary(train$m2_em_count_valid)  #do a summary
 summary(train$m1_EM_COUNT_VALID)  #do a summary(have recorded in dataset)
 
 ######### 5.2 category variables #########  
 
 train$m2_ex_auto_used0005_x5 <- ifelse(is.na(train$ex_auto_used0005), 0,    #when ex_auto_used0005 is missing then assign 0 
                                        ifelse(train$ex_auto_used0005 == 5, 1, 0))#when ex_auto_used0005 is 5 then assign 1,else 0
 summary(train$m2_ex_auto_used0005_x5)
 summary(train$m1_EX_AUTO_USED0005_X5)
 
 train$m2_ex_auto_used0005_x789 <- ifelse(train$ex_auto_used0005 %in% c(7,8,9), 1,0) #when ex_auto_used0005 is 7 or 8 or 9, then assign 1,else 0
 summary(train$m2_ex_auto_used0005_x789)
 summary(train$m1_EX_AUTO_USED0005_X789)

 #########################################################################################################################################
 ############################################################  Part6: Correlation  ####################################################### 
 #########################################################################################################################################
 
library(picante)  #call picante

 var_list<-c('dv_response','m1_POS_NUM_ORDERS_24MO',
             'm1_POS_NUM_ORDERS',
             'm1_SH_MNTHS_LAST_INQUIRED',
             'm1_POS_SP_QTY_24MO',
             'm1_POS_REVENUE_TOTAL',
             'm1_POS_LAST_ORDER_DPA',
             'm1_POS_MARGIN_TOTAL',
             'm1_pos_mo_btwn_fst_lst_order',
             'm1_POS_REVENUE_BASE',
             'm1_POS_TOT_REVPERSYS',
             'm1_EM_COUNT_VALID',
             'm1_EM_NUM_OPEN_30',
             'm1_POS_MARGIN_TOTAL_12MO',
             'm1_EX_AUTO_USED0005_X5',
             'm1_SH_INQUIRED_LAST3MO',
             'm1_EX_AUTO_USED0005_X789',
             'm1_HH_INCOME',
             'm1_SH_INQUIRED_LAST12MO',
             'm1_POS_LAST_TOTAL_REVENUE',
             'm1_EM_ALL_OPT_OUT_FLAG',
             'm1_POS_REVENUE_TOTAL_6MO',
             'm1_EM_MONTHS_LAST_OPEN',
             'm1_POS_MNTHS_LAST_ORDER',
             'm1_WEB_MNTHS_SINCE_LAST_SES')  #put the variables you want to do correlation analysis here
 
 ####6.1  all variables correlation
 corr_var<-train[, var_list]  #select all the variables you want to do correlation analysis
 str(corr_var)
 View(corr_var)
 # check the variable type
 
 correlation<-data.frame(cor.table(corr_var,cor.method = 'pearson'))  #do the correlation
 View(correlation)
 # select correlation result only
 cor_only=data.frame(row.names(correlation),correlation[, 1:ncol(corr_var)])
 View(cor_only)
 
 library('xlsx')
 write.csv(cor_only,file = 'Correlatinal.csv')
 
 #########################################################################################################################################
 ############################################################  Part7: Modeling  ####################################################### 
 #########################################################################################################################################
 
 var_list<-c('m1_WEB_MNTHS_SINCE_LAST_SES',
             'm1_POS_NUM_ORDERS_24MO',
             'm1_POS_MNTHS_LAST_ORDER',
             'm1_POS_LAST_ORDER_DPA',
             'm1_EM_COUNT_VALID',
             'm1_EM_MONTHS_LAST_OPEN',
             'm1_POS_TOT_REVPERSYS',
             'm1_pos_mo_btwn_fst_lst_order'
 )   #put the variables you want to try in model here
 
 mods = train[,c('dv_response',var_list)]   #select Y and varibales you want to try
 str(mods)
 # glm
 model_glm = glm(dv_response~.,data =mods,family = binomial(link = 'logit'))
 model_glm

# Stepwise
library(MASS)
 #using both backward and forward stepwise selection
 model_sel <- stepAIC(model_glm,direction = 'both')
 summary<-summary(model_sel)  #summary
 str(summary)
 
 model_summary<-data.frame(var=rownames(summary$coefficients),summary$coefficients) #do the model summary
 View(model_summary) 
 
 # scale
 mods2 <- scale(train[,var_list],center = T,scale = T)
 mods3 <-data.frame(dv_response = c (train$dv_response),mods2[,var_list])
 View(mods3) 
 
 # again glm
 
 (model_glm2<-glm(dv_response~.,data=mods3,family =binomial(link ="logit")))  #logistic model
 summary2<-summary(model_glm2) 
 
 model_summary2<-data.frame(var=rownames(summary2$coefficients),summary2$coefficients) #do the model summary
 View(model_summary2)
 
 # VIF
 fit <- lm(dv_response~., data=mods)  #regression model
 install.packages('car')  #Install Package 'Car' to calculate VIF
 library(car)  #call Car
 vif=data.frame(vif(fit)) #get Vif
 var_vif=data.frame(var=rownames(vif),vif) #get variables and corresponding Vif
 View(var_vif)
 
 #variable correlation
 cor<-data.frame(cor.table(mods,cor.method = 'pearson')) #calculate the correlation
 correlation<-data.frame(variables=rownames(cor),cor[, 1:ncol(mods)]) #get correlation only
 View(correlation)
 
 #output to csv
 write.csv(model_summary,file = 'model_summary.csv')
 write.csv(var_vif, file = 'var_vif.csv')
 write.csv(correlation, file = 'correlation.csv')
 
 #########################################################################################################################################
 ############################################################  Part8: Model evaluation  ####################################################### 
 #########################################################################################################################################
 
 library(gplots)
 library(ROCR)
 
 #### train data####
 pred_prob<-predict(model_glm,train,type='response') #predict Y
 
 pred <- prediction(pred_prob,train$dv_response)
  
  perf = performance(pred,'tpr','fpr') #Check the performance

  auc <- performance(pred,'auc')
  # get auc
  unlist(slot(auc,"y.values"))
 
  # plot ROC Curve
  plot(perf,col="black",lty=3, lwd=3,main='ROC Curve')
  
  
 
 
 
 
 
 
 
 
 
 
 



