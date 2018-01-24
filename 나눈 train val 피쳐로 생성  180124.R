


#-----------model에서 나눈 train과 validation의 user_id에 따라 feature를 생성한다.----------------------------




rm(list=ls())



library(data.table)
library(dplyr)
library(tidyr)


setwd("C:/Users/User/Desktop/캡스톤2/거위")
path <- "C:/Users/User/Desktop/캡스톤2/거위"



train_set = read.csv('train.set.csv')
prior_set = read.csv('prior_set.csv')
train_set =train_set[,-1]
prior_set = prior_set[,-1]

user.tr = read.csv('user.tr.csv')
user.val = read.csv('user.val.csv')








#----------------------------item_purchaset_train--- product_id-----one hot vector-------------------------------





# sample 뽑을때 seed문제 때문에 object가 달라질 수 있음.




item_purchaset_train = train_set%>% # user_id 바꿔주기
  inner_join(user.tr)




item_purchaset_prior = train_set%>% # user_id 바꿔주기
  inner_join(user.val)




item_purchaset_prior = item_purchaset_prior[,-4]# x없애기
item_purchaset_train = item_purchaset_train[,-4]





z = sort(unique(train_set$product_id))
z = as.data.frame.integer(z)


FactoredVariable = match(item_purchaset_train$product_id,sort(as.matrix(z[,1])))
# product_id가 몇번째로 존재하는 지 알려준다
#이는 나중에 1000개의 열이 되는 product_id가 user_id별로 어디에 할당되어야 하는지 알려준다.
matching_n = as.data.frame.integer(FactoredVariable)













#------------------------------------------------더미 변수 만들기(one hot vector)----------------------------



matching = as.integer(1:446777)
matching_n = cbind(matching,matching_n)

          
class(matching_n)
matching_n= as.matrix(matching_n)# as.matrix로 형변환을 시켜줘야 원핫벡터 만들때 사용하는 공식을 이용가능
zero_train = matrix(0,446777,1000)
zero_train[matching_n] = 1

zero_train[1,39]


dim(zero_train)

write.csv(zero_train,"zero_train.csv")




#-----------------------------------------------이전 구매횟수 할당--------------------------------


md = as.factor(item_purchaset_train$user_id) # train_set에서 user_id 가져 옴
md =as.numeric(md)
md

item_purchaset_train$fre = prior_set$sum_user[match(item_purchaset_train$user_id,sort(matrix(prior_set$user_id)))] 
#prior_set의 이전 구매횟수를 train_set의 user_id에 따라 할당해준다. 매칭 






freq = item_purchaset_train[,4] # 이전구매 횟수



product_id = item_purchaset_train$product_id # product_id 값

z_index = sort(z$z) # 빈집의 열이 1000개이기 때문에 count/z로 product_id의 고유값 1000개를 뽑아줌
# match(product_id,z_index)는 1000개가 되어야 한다.



cbind(md,product_id,freq) # 순서 확인
cbind(md,match(product_id,z_index))

pre_order_num = matrix(0,446777,1000) # 빈집 생성
pre_order_num[cbind(md,match(product_id,z_index))]=freq 




dim(pre_order_num) # 차원을 확인 해 주자



pre_order_num[1,39]# 맞게 들어갔는지 확인
pre_order_num[1,102]# pre_order_num은 하나의 행마다 각 user의 이전구매 정보가 모두 들어 가 있다.
pre_order_num[1,1001]





write.csv(pre_order_num,'pre_order_num_train.csv')


#------------------------------------------------user_id별 같은 값 할당----------------------------------------

user_id = item_purchaset_train$user_id
user_id = as.data.frame(user_id)



pre_order_num0 = matrix(0,446777,1000)



pre_order_num0 = cbind(md,pre_order_num0)

dim(pre_order_num0)



pre_order_num = cbind(md,pre_order_num)

dim(pre_order_num)



pre_order_num0 = pre_order_num[md,] # ***** user_id 별로 행 배치시켜 주기(user_id 별로 같은 이전 구매 횟수 얻음)
# user_id를 md로 바꾼것은 item_purchaset_train의 user_id가 2부터 시작하기 때문이다 , 
# user_id보다는 md를 쓰는 것이 더 효율적
pre_order_num0 = pre_order_num0[,-1] # md 를 없애면 유실된 첫째 열이 나오면서 순서가 정확하게 맞춰진다.


pre_order_num0[2,39] # 확인



rm(pre_order_num)

input_train = cbind(zero_train,pre_order_num0)


dim(input_train)

write.csv(input_train,'input_train.csv')



#-------------------------------------item_purchaset_prior---product_id--one hot vector-------------------------

rm(list=ls())


setwd("C:/Users/User/Desktop/캡스톤2/거위")
path <- "C:/Users/User/Desktop/캡스톤2/거위"



train_set = read.csv('train.set.csv')
prior_set = read.csv('prior_set.csv')
train_set =train_set[,-1]
prior_set = prior_set[,-1]

user.tr = read.csv('user.tr.csv')
user.val = read.csv('user.val.csv')




item_purchaset_train = train_set%>% # user_id matching
  inner_join(user.tr)


item_purchaset_prior = train_set%>% # user_id matching
  inner_join(user.val)


item_purchaset_prior = item_purchaset_prior[,-4]# x없애기
item_purchaset_train = item_purchaset_train[,-4]


z = sort(unique(train_set$product_id))
z = as.data.frame.integer(z)


FactoredVariable = match(item_purchaset_prior$product_id,sort(as.matrix(z[,1]))) # 왜 z를 이용해 할당하는지?
# product_id가 몇번째로 존재하는 지 알려준다
#이는 나중에 1000개의 열이 되는 product_id가 user_id별로 어디에 할당되어야 하는지 알려준다.
matching_n = as.data.frame.integer(FactoredVariable)













#------------------------------------------------더미 변수 만들기(one hot vector)----------------------------



matching = as.integer(1:275169)
matching_n = cbind(matching,matching_n)


matching_n=read.csv("matching_n.csv")
class(matching_n)
matching_n= as.matrix(matching_n)# as.matrix로 형변환을 시켜줘야 원핫벡터 만들때 사용하는 공식을 이용가능
zero_prior = matrix(0,275169,1000)
zero_prior[matching_n] = 1






dim(zero_prior)

write.csv(zero_prior,"zero_prior.csv")



#-----------------------------------------------이전 구매횟수 할당--------------------------------


md = as.factor(item_purchaset_prior$user_id) # train_set에서 user_id 가져 옴
md =as.numeric(md)
md

item_purchaset_prior$fre = prior_set$sum_user[match(item_purchaset_prior$user_id,sort(matrix(prior_set$user_id)))] 
#prior_set의 이전 구매횟수를 train_set의 user_id에 따라 할당해준다.






freq = item_purchaset_prior[,4] # 이전구매 횟수


zzzzz = read.csv('county.csv') # product_id의 고유 1000개 값?!!!!
#train_set = train_set[,c(-1,-5)]
#z= count?

product_id = item_purchaset_prior$product_id # product_id 값

z_index = sort(z$z) # 빈집의 열이 1000개이기 때문에 count로 product_id의 고유값 1000개를 뽑아줌
# match(product_id,z_index)는 1000개가 되어야 한다.



cbind(md,product_id,freq) # 순서 확인
cbind(md,match(product_id,z_index))

pre_order_num = matrix(0,275169,1000) # 빈집 생성
pre_order_num[cbind(md,match(product_id,z_index))]=freq 




dim(pre_order_num) # 차원을 확인 해 주자



pre_order_num[1,3]# 맞게 들어갔는지 확인
pre_order_num[1,509]# pre_order_num은 하나의 행마다 각 user의 이전구매 정보가 모두 들어 가 있다.






write.csv(pre_order_num,'pre_order_num_train.csv')


#------------------------------------------------user_id별 같은 값 할당----------------------------------------

user_id = item_purchaset_prior$user_id
user_id = as.data.frame(user_id)



pre_order_num0 = matrix(0,275169,1000)



pre_order_num0 = cbind(md,pre_order_num0)

dim(pre_order_num0)



pre_order_num = cbind(md,pre_order_num)

dim(pre_order_num)



pre_order_num0 = pre_order_num[md,] # ***** user_id 별로 행 배치시켜 주기(user_id 별로 같은 이전 구매 횟수 얻음)
# user_id를 md로 바꾼것은 item_purchaset_train의 user_id가 2부터 시작하기 때문이다 , 
# user_id보다는 md를 쓰는 것이 더 효율적
pre_order_num0 = pre_order_num0[,-1] # md 를 없애면 유실된 첫째 열이 나오면서 순서가 정확하게 맞춰진다.


pre_order_num0[2,3] # 확인



rm(pre_order_num)


dim(zero_prior)

input_val = cbind(zero_prior,pre_order_num0)

dim(input_val)


write.csv(input_val,'input_val.csv')

