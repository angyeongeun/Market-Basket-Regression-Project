pre_order_num0 = read.csv('pre_order_num0.csv')

pre_order_num0 =pre_order_num0[,-1001]
pre_order_num0[1,1000]


pre_order_num = cbind(zero,pre_order_num0)
write.csv(pre_order_num,'pre_order_num_first.csv')



dim(pre_order_num)
