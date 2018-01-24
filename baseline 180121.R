

#------------------------------------------baseline cording--------------------------------------------------


rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(tidyr)

setwd("C:/Users/User/Desktop/캡스톤2/거위")
path <- "C:/Users/User/Desktop/캡스톤2/거위"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))





#----------------------------------------------prior---------------------------------------------------------

item = orderp%>%
  inner_join(products)%>%
  select(order_id,product_id,-reordered,-product_name)



count_prd_id = item%>%
  inner_join(orders)%>%
  group_by(user_id)

count_prd_id0 = count_prd_id%>%
  group_by(product_id)%>%
  summarise(n=n())
  




match_p = orderp%>%
   inner_join(orders)%>%
   select(product_id,user_id,-reordered,order_id)
  





#---------------------------------------- product_id 의 갯수 구하기-------------------------------------------



count = count_prd_id0%>%
  top_n(n=1000,wt = n)%>%
  arrange(desc(n))





#prior
match_p_set = match_p%>%
  group_by(product_id)%>%
  inner_join(count)%>%
  arrange(user_id)# 오름차순 정리



prior_set = match_p_set%>%
  group_by(user_id,product_id)%>% # group by (user_id,product)
  summarise(sum_user = n())%>%
  ungroup()



# 6025373*3

prior_set$z = sort(prior_set$product_id)



#--------------------------------------------------train-----------------------------------------------------


match_train = ordert%>%
  inner_join(orders)%>%
  select(user_id,product_id,order_id,reordered)

match_train = match_train%>%
  inner_join(count)%>%
  arrange(user_id)


train_set = match_train%>%
  group_by(user_id,product_id)%>% # group by (user_id,product)
  summarise(sum_user = n())%>%  # train은 현재의 구매건이기 때문에 모두 1로 처리 됨을 확인
  ungroup()
  
train_set$reordered = match_train$reordered[match(train_set$product_id,match_train$product_id)]



train_set = train_set[,c(1,2,4)]   #  반응 변수


#--------------------------------------------------------------------------------------------------------------
z = sort(unique(train_set$product_id))
z = as.data.frame.integer(z)

 

FactoredVariable = match(train_set$product_id,sort(as.matrix(z[,1])))
matching_n = as.data.frame.integer(FactoredVariable)




#------------------------------------------------더미 변수 만들기(one hot vector)



matching = as.integer(1:721946)
matching_n = cbind(matching,matching_n)

write.csv(train_set,"train.set.csv")
write.csv(matching_n,"matching_n.csv")
rm(list=ls())          
matching_n=read.csv("matching_n.csv")
class(matching_n)
matching_n= as.matrix(matching_n[,-1])
zero = matrix(0,721946,1000)
zero[matching_n] = 1
write.csv(zero,"zero.csv")
zero[2,]



















