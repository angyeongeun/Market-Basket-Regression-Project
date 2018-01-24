


# 이전구매횟수를 벡터로 할당하기 위한 논리 예시

md=c(rep("user1",10),rep("user21",10),rep("user31",10))
md
class(md)



md1=as.factor(md)
md1
md2=as.numeric(md1)
md2




freq=c(sample(1:300,30,rep=T))
freq

data.frame(freq55)


product_id=c(sample(1:3000,30,rep=T))

z_index = sort(product_id)

cbind(md,product_id,freq)
cbind(md2,product_id,freq)

pre_order_num=matrix(0,3,30)
pre_order_num[cbind(md2,match(product_id,z_index))]=freq #
pre_order_num
class(pre_order_num)
pre_order_num=as.data.frame(pre_order_num)
pre_order_num$user_id = c(1,21,31)
pre_order_num
## train의 user_id 만 뽑아서 pre_order_num과 left join?


#-----------------------------------------data를 활용한 이전 구매횟수 벡터 할당-----------------------------------------------------

rm(list=ls())
gc()





prior_set = read.csv('prior_set.csv')
train_set = read.csv('train.set.csv')



md = as.factor(train_set$user_id) # train_set에서 user_id 가져 옴
md = as.numeric(md)

md

train_set$fre = prior_set$sum_user[match(train_set$user_id,sort(matrix(train_set$user_id)))] 
#prior_set의 이전 구매횟수를 train_set의 user_id에 따라 할당해준다. 매칭 





freq = train_set[,c(5)] # 이전구매 횟수


count = read.csv('county.csv') # product_id의 고유 1000개 값
#train_set = train_set[,c(-1,-5)]

product_id = train_set$product_id # product_id 값

z_index = sort(count$product_id) # 빈집의 열이 1000개이기 때문에 count로 product_id의 고유값 1000개를 뽑아줌
# match(product_id,z_index)는 1000개가 되어야 한다.



cbind(md,product_id,freq) # 순서 확인


pre_order_num = matrix(0,721946,1000) # 빈집 생성
pre_order_num[cbind(md,match(product_id,z_index))]=freq 

#1, product_id의 고유값인 z_index에서 product_id를 찾아 들어갈 자리를 알아낸다
#2. user_id의 오름차순으로 정리 된 열을 들어갈 자리와 (행,열)관계로 묶는다.
#3. [행,열]관계로 들어있으므로 pre_order_num의 [행,열] 값을 나타낸다.
#4. [행,열] = c(이전구매 횟수)의 관계가 되어 [행,열]에 따라 freq 값이 빈집에 들어간다.



dim(pre_order_num) # 차원을 확인 해 주자



write.csv(pre_order_num,'pre_order_num.csv')

#pre_order_num[1:721946,] = pre_order_num[,1:1000] - 잘못짠 코딩,  user_id별 똑같은 행을 항당 시켜주려 했음

pre_order_num[1,3]# 맞게 들어갔는지 확인
pre_order_num[1,509]# pre_order_num은 하나의 행마다 각 user의 이전구매 정보가 모두 들어 가 있다.



user_id = train_set$user_id


pre_order_num = cbind(pre_order_num,user_id)



pre_order_num0 = matrix(0,721946,1000)
pre_order_num0[cbind(md,match(product_id,z_index))] = freq
pre_order_num0[cbind(md1,match(product_id,z_index))] = freq


pre_order_num0[70000,3]

pre_order_num0 = cbind(pre_order_num0,user_id)
write.csv(pre_order_num0,'pre_order_num0.csv')





