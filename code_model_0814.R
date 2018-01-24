rm(list=ls())
gc()

###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)
setwd("D:\\by0705\\Instacart\\XGboost_order_lda_0814")

load("data_lda_order_to_user.RData")

#p_id <- read.csv("p_1000.csv")

# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
#train$user_id <- NULL
#train$product_id <- NULL
#train$order_id <- NULL
train$aisle_id <- NULL
train$department_id <- NULL
train$reordered[is.na(train$reordered)] <- 0


test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
#test$user_id <- NULL
test$reordered <- NULL
test$aisle_id <- NULL
test$department_id <- NULL

rm(data)
gc()

set.seed(1000)
val.idx<- sample(1:length(unique(train$user_id)), 50000, replace=FALSE)
val.idx<- sort(val.idx)

user.tr <- as.data.frame(unique(train$user_id)[-val.idx])
user.val <- as.data.frame(unique(train$user_id)[val.idx])

colnames(user.tr)<- c("user_id")
colnames(user.val)<- c("user_id")

write.csv(user.tr,'user.tr.csv')
write.csv(user.val,'user.val.csv')


user_nnone<- read.csv("user_nnone_add_2.csv")
user_nnone$predicted <- NULL 


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.05,
  "max_depth"           = 7,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

tr <- train %>% inner_join(user.tr, by='user_id')
val <- train %>% inner_join(user.val, by='user_id')
rm(train)
gc()

tr.x <- tr %>% inner_join(user_nnone, by='user_id')
val.x <- val %>% inner_join(user_nnone, by='user_id')

tr <- tr %>% select(user_id, order_id, product_id)
val <- val %>% select(user_id, order_id, product_id)

tr.mat<- tr.x %>% select(-user_id, -product_id, -order_id)
val.mat <- val.x %>% select(-user_id, -product_id, -order_id, -reordered)
val.mat <- xgb.DMatrix(as.matrix(val.mat))
val.x<- val.x[,1:2]
tr.x <- tr.x[,c(1,2)]
gc()

test.x <- test %>% inner_join(user_nnone, by='user_id')
test <- test %>% select(user_id, product_id, order_id)

test.mat <- xgb.DMatrix(as.matrix(test.x %>% select(-user_id,-order_id, -product_id)))
test.x <- test.x %>% select(user_id, product_id)

rm(val.idx)
gc()

X <- xgb.DMatrix(as.matrix(tr.mat %>% select(-reordered)), label = tr.mat$reordered)

#X <- xgb.DMatrix(tr.mat[,-86], label = tr.mat[,86])

gc()

a=Sys.time()
model <- xgboost(data = X, params = params, nrounds = 700)
b=Sys.time()
print(b-a)

#importance <- xgb.importance(colnames(X), model = model)
#xgb.ggplot.importance(importance)

#rm(importance)
gc()

# Apply model -------------------------------------------------------------

test.x$reordered <- predict(model, test.mat)
val.x$reordered <- predict(model, val.mat)
tr.x$reordered <- predict(model, X)

test <- test %>% left_join(select(test.x, user_id, product_id, reordered), by=c('user_id','product_id'))
val <- val %>% left_join(select(val.x, user_id, product_id, reordered), by=c('user_id','product_id'))
tr <- tr %>% left_join(select(tr.x, user_id, product_id, reordered), by=c('user_id','product_id'))

test$reordered[is.na(test$reordered)] <- 0
val$reordered[is.na(val$reordered)] <- 0
tr$reordered[is.na(tr$reordered)] <- 0

#write.csv(tr, file="result\\train_rslt_dep7_iter700_lam0.05.csv", row.names=F)
#write.csv(val, file="result\\val_rslt_dep7_iter700_lam0.05.csv", row.names=F)
#write.csv(test, file="result\\test_rslt_dep7_iter700_lam0.05.csv", row.names=F)

rm(val.mat, test.mat, test.x, val.x, tr.x, X)
gc()

thres <- 0.2



#---------- train set의 precision, recall 만드는 table ----------#

temp1<- cbind(user.tr, rep('train', nrow(user.tr)))
temp2<- cbind(user.val,rep('valid', nrow(user.val)))
colnames(temp1)<- c('user_id', 'set')
colnames(temp2) <- c('user_id', 'set')
tmp <- data.frame(rbind(temp1, temp2))
tmp$user_id<- as.character(tmp$user_id)
tmp$user_id<- as.numeric(tmp$user_id)

rm(temp1, temp2)
gc()

#---------- train 의 output 만들기 (1001개) ----------#

train.y <- ordert %>%
	left_join(select(orders,order_id),by="order_id") %>% 
	left_join(tmp, by="user_id") %>% 
	arrange(user_id)

pre.rec <- train.y %>%
	filter(set=='train') %>%
	select(order_id, user_id) %>%
	distinct(order_id, user_id) %>%
	arrange(order_id)

pre.rec.val <- train.y %>%
	filter(set=='valid') %>%
	select(order_id, user_id) %>%
	distinct(order_id, user_id) %>%
	arrange(order_id)

temp <- train.y %>%
	filter(set=='train') %>%
	select(order_id, product_id, reordered) %>%
	filter(reordered==1) %>%
	group_by(order_id) %>%
	summarize(den.rec = n()) %>%
	mutate(den.pre=0, nom=0)

pre.rec<- left_join(pre.rec, temp, by='order_id')
rm(temp)
gc()

temp <- train.y %>%
	filter(set=='valid') %>%
	select(order_id, product_id, reordered) %>%
	filter(reordered==1) %>%
	group_by(order_id) %>%
	summarize(den.rec = n()) %>%
	mutate(den.pre=0, nom=0)

pre.rec.val<- left_join(pre.rec.val, temp, by='order_id')

#train
idx=which(is.na(pre.rec$den.rec))
pre.rec[idx,]$den.rec <- 1
pre.rec[idx,]$den.pre <- 0
pre.rec[idx,]$nom <- 0

#3273명 None
idx=which(is.na(pre.rec.val$den.rec))
pre.rec.val[idx,]$den.rec <- 1
pre.rec.val[idx,]$den.pre <- 0
pre.rec.val[idx,]$nom <- 0
rm(idx)
gc()


#---------- train predict----------#

tr$y <- (tr$reordered > thres) * 1

#tr.order<- train.y %>%
#	filter(set=='train') %>%
#	distinct(user_id,order_id) 
#tr <- tr %>%  left_join(tr.order, by='user_id')
#rm(tr.order)
#gc()

submis.tr <- tr %>%
  filter(y == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(tr$order_id[!tr$order_id %in% submis.tr$order_id]),
  products = "None"
)

submis.tr <- submis.tr %>% bind_rows(missing) %>% arrange(order_id)

idx.pre.tr <- which(submis.tr$products=='None')

get.len=sapply(submis.tr[,2],function(x) strsplit(as.character(x)," "))
get.len.vec=unlist(lapply(get.len, length))
#mean(get.len.vec)

pre.rec$den.pre <- get.len.vec 



#---------- predict----------#

val$y <- (val$reordered > thres) * 1

#val.order<- train.y %>%
#	filter(set=='valid') %>%
#	distinct(user_id,order_id) 
#
#val <- val %>%  left_join(val.order, by='user_id')
#rm(val.order)
#gc()

submis.val <- val %>%
  filter(y == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )


missing <- data.frame(
  order_id = unique(val$order_id[!val$order_id %in% submis.val$order_id]),
  products = "None"
)

submis.val <- submis.val %>% bind_rows(missing) %>% arrange(order_id)

idx.pre <- which(submis.val$products=='None')

get.len.val=sapply(submis.val[,2],function(x) strsplit(as.character(x)," "))
get.len.vec.val=unlist(lapply(get.len.val, length))
#mean(get.len.vec.val)

pre.rec.val$den.pre <- get.len.vec.val


#---------- train 의 true 값 ----------#
t.y <- train.y %>% 
	filter(set=='train') %>%
	filter(reordered==1) %>%
	select(user_id, order_id, product_id) %>%
	group_by(order_id)%>%
	summarise(tmp = paste(product_id, collapse=" "))

tr.true<- submis.tr %>% select(order_id) %>%
	left_join(t.y, by='order_id')

tr.true[is.na(tr.true$tmp),]$tmp <- "None"

get.ren.true=strsplit(tr.true$tmp," ")

rm(t.y, tr.true)
gc()


#pre.rec
a=Sys.time()
for(u in 1: nrow(pre.rec)){
	pre.rec$nom[u]<- sum(!is.na(match( get.len[[u]], get.ren.true[[u]])))
}
b=Sys.time()
print(b-a)

gc()

pre.rec <- mutate(pre.rec, pre=nom/den.pre, rec=nom/den.rec)
pre.rec <- mutate(pre.rec, f1= (2*pre*rec)/(pre+rec))
pre.rec[which(pre.rec$pre==0 | pre.rec$rec==0),]$f1 <- 0


#---------- true 값 ----------#
t.y <- train.y %>% 
	filter(set=='valid') %>%
	filter(reordered==1) %>%
	select(user_id, order_id, product_id) %>%
	group_by(order_id)%>%
	summarise(tmp = paste(product_id, collapse=" "))

val.true<- submis.val %>% select(order_id) %>%
	left_join(t.y, by='order_id')

val.true[is.na(val.true$tmp),]$tmp <- "None"

get.ren.true.val=strsplit(val.true$tmp," ")

rm(t.y, val.true)
gc()

#pre.rec
a=Sys.time()
for(u in 1: nrow(pre.rec.val)){
	pre.rec.val$nom[u]<- sum(!is.na(match( get.len.val[[u]], get.ren.true.val[[u]])))
}
b=Sys.time()
print(b-a)

#pre.rec.val[which(pre.rec.val$den.pre==0 & pre.rec.val$nom==0),4:5] <- 1

#pre.rec.val <- mutate(pre.rec.val, pre=nom/den.pre, rec=nom/den.rec)
#pre.rec.val <- mutate(pre.rec.val, f1= (2*pre*rec)/(pre+rec))
#pre.rec.val[which(pre.rec.val$pre==0 | pre.rec.val$rec==0),]$f1 <- 0

pre.rec.val <- mutate(pre.rec.val, pre=nom/den.pre, rec=nom/den.rec)
pre.rec.val <- mutate(pre.rec.val, f1= (2*pre*rec)/(pre+rec))
pre.rec.val[which(pre.rec.val$pre==0 | pre.rec.val$rec==0),]$f1 <- 0


#write.csv(pre.rec.val, file="submis\\tr_f1_lbd1_0724_5.csv", row.names=F)


#---------- test submission table 만들기 ----------#

test$y <- (test$reordered > thres) * 1

submission <- test %>%
  filter(y == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)

get.len=sapply(submission[,2],function(x) strsplit(as.character(x)," "))
get.len.vec=unlist(lapply(get.len, length))
cat("recommended items in test set = ", mean(get.len.vec), "\n")



rslt <- matrix(0, nrow=3, ncol=6)
colnames(rslt)<- c("f1","preci","recall","n.recom","n.real","n.collect")
rownames(rslt) <- c("train","valid","test")

rslt[1,] <- c(mean(pre.rec$f1),
mean(pre.rec$pre),
mean(pre.rec$rec),
mean(pre.rec$den.pre),
mean(pre.rec$den.rec),
mean(pre.rec$nom))

rslt[2,] <- c(mean(pre.rec.val$f1),
mean(pre.rec.val$pre),
mean(pre.rec.val$rec),
mean(pre.rec.val$den.pre),
mean(pre.rec.val$den.rec),
mean(pre.rec.val$nom))

rslt[3,4] <- mean(get.len.vec)

print(thres)
print(rslt)


#write.csv(submission, file = "submis//submission_latent_all_p_0812.csv", row.names = F)

#val$y <- NULL
#val$order_id <- NULL
#tr$y <- NULL
#tr$order_id <- NULL
#
#test$y <- NULL

