library(dplyr)

library(visdat)

library(car)
## Reading the input dataset

getwd()
setwd("C:\\Users\\user\\Documents\\Edvancer R materials")
getwd()
store_test=read.csv("store_test.csv",stringsAsFactors = F)
store_train=read.csv("store_train.csv",stringsAsFactors = F)



#data engineerning

store_test$store=NA

store_train$data='train'

store_test$data='test'

store_all=rbind(store_test,store_train)





# taking first five character of storecode

store_all$storecode=substr(store_all$storecode,1,5)



# convert into character for dummy creation

store_all$country=as.character(store_all$country)

store_all$State=as.character(store_all$State)

store_all$CouSub=as.character(store_all$CouSub)

## drop the columns which are not needed

store_all$Id=NULL

store_all$Areaname=NULL

store_all$countytownname=NULL

# creating feature with sales0 sales1 sales2 etc
 
store_all$new_sale0=(store_all$sales0 - store_all$sales1)/store_all$sales0

store_all$new_sale1=(store_all$sales1 - store_all$sales2)/store_all$sales1

store_all$new_sale2=(store_all$sales2 - store_all$sales3)/store_all$sales2

store_all$new_sale3=(store_all$sales3 - store_all$sales4)/store_all$sales3

store_all$new_sale4=(store_all$sales4 - store_all$sales0)/store_all$sales4


store_all=store_all %>%
  
  mutate(#countytownname=gsub(">","",countytownname),
    
    # countytownname=gsub("<","",countytownname),
    
    # Areaname=gsub("<","",Areaname),
    
    # Areaname=gsub(">","",Areaname),
    
    #Areaname=gsub("_","",Areaname),
    
    #Areaname=gsub("-","",Areaname),
    
    countyname=gsub("<","",countyname),
    
    countyname=gsub(">","",countyname))



# dummy function

CreateDummies=function(data,var,freq_cutoff=0){
  
  t=table(data[,var])
  
  t=t[t>freq_cutoff]
  
  t=sort(t)
  
  categories=names(t)[-1]
  
  
  
  for (cat in categories) {
    
    name=paste(var,cat,sep = "_")
    
    name=gsub(" ","",name)
    
    name=gsub("-","_",name)
    
    name=gsub("\\?","Q",name)
    
    name=gsub("<","LT_",name)
    
    name=gsub("\\+","",name)
    
    
    
    data[,name]=as.numeric(data[,var]==cat)
    
  }
  
  data[,var]=NULL
  
  return(data)
  
}



char_logical=sapply(store_all,is.character)

cat_cols=names(store_all)[char_logical]

cat_cols=cat_cols[!(cat_cols %in% c('data'))]



# dummies created

for(col in cat_cols){
  
  store_all=CreateDummies(store_all,col,150)
  
}



# treating na

for (col in names(store_all)) {
  
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    
    store_all=store_all[!(is.na(store_all[,col])),]
    
  }
  
  
  
}
num_cols = sapply(store_all, is.numeric)

num_cols <- setdiff(names(num_cols), 'store')

store_all[,num_cols] <- lapply(store_all[,num_cols], 
function(x)ifelse(is.na(x), mean(x, na.rm=T), x))

#check na after treating them

colSums(is.na(store_all))

## splits

store_train=store_all %>% filter(data=='train') %>% select(-data)

store_test=store_all %>% filter(data=='test') %>% select(-data,-store)

#85:15 train:test split

s=sample(1:nrow(store_train),0.85*nrow(store_train))

store_train1=store_train[s,]

store_train2=store_train[-s,]



for_vif=lm(store~.,data=store_train1)

alias(for_vif)



## Removing columns as they are into alias

remove_cols <- c("state_alpha_CT",
                 
                 "state_alpha_TX",
                 
                 "state_alpha_VT",
                 
                 "state_alpha_NH",
                 
                 "state_alpha_MA",
                 
                 "state_alpha_ME")



store_train1 = store_train1[,setdiff(names(store_train1), remove_cols)] 

for_vif=lm(store ~ .-sales0 -sales1 -sales2 -sales3,data=store_train1,)

store_all$store=as.factor(store_all$store)

log_fit=glm(store~.-sales0-sales2-sales3,data=store_train1,family = "binomial")

log_fit=step(log_fit)



# prediction on store_train1 & 2

log_fit1=glm(formula(log_fit), data = store_train1,family='binomial')

# validation

train.score=predict(log_fit1,newdata = store_train1,type = 'response')

val.score=predict(log_fit1,newdata = store_train2,type = 'response')


# measure to check scores
windows(width = 10, height = 8) 
caTools::colAUC(train.score, store_train1$store, plotROC = T)

caTools::colAUC(val.score, store_train2$store, plotROC = T)  

## creating the final fit

log.fit.final=glm(formula(log_fit), data = store_train,
                  
                  family = 'binomial')

summary(log.fit.final)

## Now predicting everything on test

test.prob.score=predict(log.fit.final,newdata = store_test,type = 'response')
write.csv(test.prob.score,"Nikita_Monga_project2.csv" ,row.names = FALSE)
