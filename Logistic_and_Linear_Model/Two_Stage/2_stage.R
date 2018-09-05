########################################### Case Study: Two Stage
#This code mainly contains 4 parts:
#Part1: logistic model 
#Part2: Linear model
#Part3: application of 2 models
###########################################################################################################
setwd("Y:/DataSet/R/Regression_Model/Two_stage")
list.files()

#########################################################################################################################################
############################################################  Part1: logistic model  ####################################################
#########################################################################################################################################

file.path_logstic <- "data_response_model.csv"
raw_logistic <- read.csv(file.path_logstic,stringsAsFactors = F) #read in your csv data
train_logistic <- raw_logistic[raw_logistic$segment == 'build',]
str(train_logistic)

var_list_logistic<-c('m1_WEB_MNTHS_SINCE_LAST_SES',
                     'm1_POS_MNTHS_LAST_ORDER',
                     'm1_POS_NUM_ORDERS_24MO',
                     'm1_pos_mo_btwn_fst_lst_order',
                     'm1_EM_COUNT_VALID',
                     'm1_POS_TOT_REVPERSYS',
                     'm1_EM_MONTHS_LAST_OPEN',
                     'm1_POS_LAST_ORDER_DPA'
)   #put the final model variables

mods_logistic <- train_logistic[,c('dv_response',var_list_logistic)]  #select Y and varibales you want to try
model_glm <- glm(dv_response~.,data = mods_logistic,family = binomial(link = "logit" ))
summary(model_glm)

#########################################################################################################################################
############################################################  Part2: Linear model  ####################################################### 
######################################################################################################################################### 
file.path_linear <- "data_revenue_model.csv"
raw_linear <- read.csv(file.path_linear,stringsAsFactors = F)
train_linear <- raw_linear[raw_linear$segment == 'build',]

var_list_linear<-c('m2_POS_REVENUE_BASE',
                   'm2_POS_LAST_TOTAL_REVENUE',
                   'm2_POS_MNTHS_LAST_ORDER',
                   'm2_POS_REVENUE_BASE_SP_6MO',
                   'm2_POS_SP_QTY_24MO',
                   'm2_POS_TOT_REVPERSYS',
                   'm2_WEB_MNTHS_SINCE_LAST_SES',
                   'm2_SH_MNTHS_LAST_INQUIRED' 
)   #put the final model variables

model_linear <- train_linear[,c('dv_revenue',var_list_linear)] #select Y and varibales you want to try

model_lm <- lm(dv_revenue~.,data = model_linear) #Linear model
summary(model_lm)

#########################################################################################################################################
############################################################  Part3:application of 2 models ########################################
#########################################################################################################################################

file_path <- "two_stage_data.csv"
# read in data

raw<-read.csv(file_path,stringsAsFactors = F)
pred_prob_resp<-predict(model_glm,raw,type='response') #using the logistic model to predict response score
pred_prob_reve<-predict(model_lm,raw,type='response')  #using the linear model to predict revenue

View(pred_prob_resp)
View(pred_prob_reve)
#############################################  3.1 combo performance 
combo <- pred_prob_resp*pred_prob_reve
View(combo)

# 3.1.1 separate to 10 gorups based on combo
decile_combo <- cut(combo,unique(quantile(combo,(0:10)/10)),labels = 10:1,include.lowest = T)

View(decile_combo)
table(decile_combo)

# 3.1.2 performance for response based on decile_combo
library(plyr)
#put actual response,predicted response,decile together
combo_resp<-data.frame(actual=raw$dv_response,pred_prob_resp=pred_prob_resp,decile_combo_resp=decile_combo)
head(combo_resp)

#group by decile_combo_resp
combo_decile_sum_resp <- ddply(combo_resp,.(decile_combo_resp),summarise,cnt=length(actual),resp=sum(actual))
head(combo_decile_sum_resp)

combo_decile_sum_resp2<-within(combo_decile_sum_resp,
                               {rr<-resp/cnt
                               index<-100*rr/(sum(resp)/sum(cnt))
                               })  #add rr,index

combo_decile_sum_resp3<-combo_decile_sum_resp2[order(combo_decile_sum_resp2[,1],decreasing=T),] # order decile
View(combo_decile_sum_resp3)

#3.1.3 performance for revenue based on decile_combo
#put actual revenue,predicted revenue,decile together
combo_reve<-data.frame(actual=raw$dv_revenue,pred_prob_reve=pred_prob_reve,decile_combo_reve=decile_combo)

#group by decile_combo_reve
combo_decile_sum_reve<-ddply(combo_reve,.(decile_combo_reve),summarise,cnt=length(actual),rev=sum(actual))
combo_decile_sum_reve
combo_decile_sum_reve2<-within(combo_decile_sum_reve,
                               {rev_avg<-rev/cnt
                               index<-100*rev_avg/(sum(rev)/sum(cnt))
                               })  #add rev_avg,index
combo_decile_sum_reve3<-combo_decile_sum_reve2[order(combo_decile_sum_reve2[,1],decreasing=T),]  # order decile
View(combo_decile_sum_reve3)

#############################################  3.2 cross table for resp and revenue 
#3.2.1 response part
#separate into 10 groups based on predict_response
decile_resp<-cut(pred_prob_resp,unique(quantile(pred_prob_resp,(0:10)/10)),labels=10:1, include.lowest = T)
table(decile_resp)

#3.2.2. revenue part
#separate into 10 groups based on predict_revenue
decile_rev<-cut(pred_prob_reve,unique(quantile(pred_prob_reve,(0:10)/10)),labels=10:1, include.lowest = T)
table(decile_rev)
head(decile_rev)

#3.2.3 set together
decile_cross<-data.frame( #rid=raw$rid,
  dv_response=raw$dv_response,
  dv_revenue=raw$dv_revenue,
  pred_prob_resp=pred_prob_resp,
  pred_prob_reve=pred_prob_reve,
  decile_resp=decile_resp,
  decile_reve=decile_rev
)  #usage 2
View(decile_cross)

#3.2.4 decile_cross
#put decile_resp,decile_reve together
cross_table_freq<-table(decile_resp=decile_cross$decile_resp,decile_reve=decile_cross$decile_reve)
View(cross_table_freq)
cross_table_pct<-prop.table(cross_table_freq)
View(cross_table_pct)
write.csv(cross_table_freq,file = 'cross_table.csv')  #output cross table to csv





