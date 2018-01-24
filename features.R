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
setwd("C:/Users/User/Desktop/캡스톤2/거위")


# Load Data ---------------------------------------------------------------
path <- "C:/Users/User/Desktop/캡스톤2/거위"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))


#thres =0.18


# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle, -department)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")

#rm(orderp)
#gc()



#---------------- 날짜세기 ----------------#
temp <- orders %>% select(-order_dow,-order_hour_of_day)
new.orders <- data.frame()

#a=Sys.time()
for(k in 1:max(orders$order_number, na.rm=T)){
	temp1 <- temp %>% filter(order_number > k) %>% group_by(user_id) %>%
		select(user_id,days_since_prior_order) %>% summarize(t.days = sum(days_since_prior_order, na.rm=T)) %>% ungroup()
	temp2 <- temp %>% filter(order_number ==k) %>% left_join(temp1, by='user_id')
	new.orders <- rbind(new.orders,temp2)
	rm(temp1,temp2)
	gc()
}
#b=Sys.time()
#print(b-a)
new.orders <- new.orders %>% arrange(user_id)


user_period <- new.orders %>% arrange(user_id, -order_number)%>% group_by(user_id) %>% 
	mutate(product_time = row_number())%>%
	summarise(
		user_last = min(t.days, na.rm=T),
		user_last2 = t.days[product_time == 3],
		user_last3 = t.days[product_time == 4]
)

user_period <- user_period %>% mutate(user_mean_last3=(user_last+user_last2+user_last3)/3 )

rm(temp,k)
gc()

#days_interval=read.csv("days_interval.csv")
#days_interval$product_time <- NULL
#days_interval$days_interval[is.na(days_interval$days_interval)] <- 366

# Products ----------------------------------------------------------------
prd <- orders_products %>%
	arrange(user_id, order_number, product_id) %>%
	group_by(user_id, product_id) %>%
	mutate(product_time = row_number()) %>%
	ungroup() %>%
	group_by(product_id) %>%
	summarise(
		prod_orders = n(),
		prod_reorders = sum(reordered),
		prod_first_orders = sum(product_time == 1),
		prod_second_orders = sum(product_time == 2),
		prod_add_to_cart_mean = mean(add_to_cart_order),
		prod_users_unq = n_distinct(user_id),
		prod_users_uni_reordered = uniqueN(user_id[reordered == 1])
)

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% select(-prod_first_orders, -prod_second_orders)

#rm(products)
#gc()

#featue 만들기------------------------------------------------------------------

nine_to_five = orders %>% # 9시부터 5시까지의 주문이 가장 많음, 그때 주문한 유저들의 주문수
  filter(order_hour_of_day >= 9 & order_hour_of_day <= 17) %>%
  group_by(user_id) %>%
  summarise(nine_to_five_user_order_num = sum(order_number>1))


after_one_week = orders %>% # 주문한지 1주일 된 고객의 주문이 제일 많다, 그 때 주문한 유저들의 주문수
  filter(days_since_prior_order == 7 | days_since_prior_order ) %>%
  group_by(user_id) %>%
  summarise(after_one_week_user_order_num = sum(order_number>1))


last_order_least_three = orders %>% # 고객들은 최소 3번의 주문을 한다. 최소 3번 이상은 주문한 고객의 주문 수
  group_by(user_id) %>%
  summarise(least_three_times_orders = sum(order_number>=3))


# Users -------------------------------------------------------------------
users <- orders %>%
	filter(eval_set == "prior") %>%
	group_by(user_id) %>%
	summarise(
		user_orders = max(order_number),
		user_period = sum(days_since_prior_order, na.rm = T),
		user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
		user_mean_order_dow = mean(order_dow, na.rm=T),
		user_mean_order_hour_of_day=mean(order_hour_of_day, na.rm=T)
		nine_to_five_user = 
		
)

# organic feature -------------------------------------------------------------------


o_user = products[grep("Organic",products$product_name),] # organic 상품만 추출



o_user = o_user%>%
  inner_join(orders_products, by = 'product_id')


o_user = o_user%>%
  group_by(user_id)%>%
  summarise(
    organic_user = sum(reordered == 1) / sum(order_number > 1)
  )

# sum은  ordre_number가 1이 넘는 것을 유저 기준으로 세는 것
  
  

  
  
  
  
us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

users <- users %>% inner_join(us) %>%
  left_join(o_user) %>%
  left_join(after_one_week) %>%
  left_join(last_order_least_three) %>%
  left_join(nine_to_five)

users$user_average_basket <- users$user_total_products / users$user_orders

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set)

#us <- orders %>%
#  filter(eval_set != "prior") %>%
#  select(user_id, order_id, eval_set,
#         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

rm(us)
gc()


# Aisle ---------------------------------------------------------------
temp_orderp = orderp %>% inner_join(select(orders, order_id, user_id), by='order_id') %>% 
	inner_join(select(products, product_id, aisle_id, department_id), by='product_id' )

user_aisle = temp_orderp %>% group_by(user_id, aisle_id) %>%
	summarize(
		aisle_products = n_distinct(product_id),
		aisle_reordered = sum(reordered)
	)

user_dep = temp_orderp %>% group_by(user_id, department_id) %>%
	summarize(
		dep_products = n_distinct(product_id),
		dep_reordered = sum(reordered)
	)

rm(temp_orderp)
gc()

# Orders ---------------------------------------------------------------

orders_up = orderp %>%  group_by(order_id) %>% 
	mutate(order_size = n())
orders_up$add_to_cart_order_inverted = orders_up$order_size-orders_up$add_to_cart_order
orders_up$add_to_cart_order_relative = orders_up$add_to_cart_order/ orders_up$order_size

orders_info<- orders %>% filter(eval_set!='prior') %>% select(-c(order_id, eval_set)) 

# Database ----------------------------------------------------------------
orderp<- orderp %>% left_join(select(new.orders, order_id, t.days), by='order_id')
orderpp <- orderp %>% 
	inner_join(orders, by='order_id')%>% 
	left_join(select(orders_up, order_id, product_id, add_to_cart_order_inverted, add_to_cart_order_relative), 
	by=c('order_id','product_id'))

rm(new.orders, orders_up)
gc()

data <- orderpp %>%
	group_by(user_id, product_id) %>% 
	summarise(
		up_orders = n(),
		up_first_order = min(order_number),
		up_last_order = max(order_number),
		up_average_cart_position = mean(add_to_cart_order),
		up_days = min(t.days),
		up_order_dow_mean = mean(order_dow),
		days_since_prior_order_mean = mean(days_since_prior_order, na.rm=T),
		add_to_cart_order_inverted_mean = mean(add_to_cart_order_inverted),
		add_to_cart_order_relative_mean = mean(add_to_cart_order_relative),
		reordered_sum = sum(reordered)
) %>% ungroup()


rm(orders_products, orderpp)
#rm(orders_products, orders)
gc()

data <- data %>% 
	inner_join(select(products, product_id, aisle_id, department_id), by='product_id' )

data <- data %>% 
	inner_join(prd, by = "product_id") %>%
	inner_join(users, by = "user_id") %>% 
	inner_join(orders_info, by='user_id') %>%
	inner_join(user_aisle , by=c('user_id','aisle_id')) %>%
	inner_join(user_dep, by=c('user_id','department_id')) %>% 
	inner_join(select(user_period, user_id, user_last2, user_last3, user_mean_last3), by='user_id')

rm(orders_info, user_aisle, user_dep, user_period)

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
data$reordered_prob = ifelse(data$reordered_sum > 0, 1/data$up_orders, 0)
data$user_product_reordered_ratio= (data$reordered_sum+1)/data$up_orders
data$aisle_reordered_ratio = data$aisle_reordered/data$user_orders
data$dep_reordered_ratio = data$dep_reordered/data$user_orders

#user_order_feature<- read.csv("user_order_feature_lag5.csv")
#user_order_feature <- user_order_feature %>% 
#	select(-order_id1, -order_id2, -order_id3, -order_id4, -order_id5, -user_last, -user_last2, -user_last3)
#data <- data %>% inner_join(user_order_feature, by='user_id')

rm(prd,users,aisles, departments, path, products)
gc()

#rm(prd,users,aisles, departments, user_order_feature, path,products)
#rm(ordert, prd, users)
#gc()

#p_id <- read.csv("p_1000.csv")
#save.image(file = "data_with_5lag.RData")

data <- data %>% inner_join(theta, by='user_id')

#rm(theta, o_id)
gc()


data <- data %>% inner_join(phi, by='product_id')
data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))


#rm(p_id, phi)
#gc()

save.image(file = "data_lda_order_to_user.RData")
