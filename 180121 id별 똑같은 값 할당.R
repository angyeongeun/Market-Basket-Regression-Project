

#-------------------------------------------------------------------------------------------------------------
#pre_order_num0은 1행 : 0-0-10-...10-...10, 2행 : 0...8...8의 형태를 띄고 있다.
#이를 user_id별로 똑같은 '이전 구매 횟수'를 배치시켜 줄 것이다.
#이는 모든 값이 들어 있는 행을 user_id 에 맞춰주면 된다.



rm(list=ls())
gc()



pre_order_num = read.csv('pre_order_num.csv')
head(pre_order_num)

pre_order_num=pre_order_num[,-1] # read.csv로 띄우면 앞의 순서가 매겨짐 - 그 열을 없앰
dim(pre_order_num)


user_id = train_set$user_id

pre_order_num0 = matrix(0,721946,1000)
pre_order_num0 = cbind(user_id,pre_order_num0)



dim(pre_order_num0)


pre_order_num = cbind(user_id,pre_order_num)





pre_order_num0 = pre_order_num[user_id,] # ***** user_id 별로 행 배치시켜 주기(user_id 별로 같은 이전 구매 횟수 얻음)
pre_order_num0 = pre_order_num0[,-1]

rm(pre_order_num)

pre_order_num0 = cbind(pre_order_num0,user_id)


write.csv(pre_order_num0,'pre_order_num0.csv')



