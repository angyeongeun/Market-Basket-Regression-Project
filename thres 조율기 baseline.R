x = c(0.11,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1)



par(mfrow = c(1, 1))

for(thres in x){
  val$y <- (val$reordered > thres) * 1

#val.order<- train.y %>%
#	filter(set=='valid') %>%
#	distinct(user_id,order_id) 
#
#val <- val %>%  left_join(val.order, by='user_id')
#rm(val.order)
#gc()


# val$order_id = ordert$order_id[match(val$product_id,ordert$product_id)]



submis.val <- val %>%
  filter(y == 1) %>%
  group_by(user_id) %>%inner_join(z)%>%
  summarise(
    products = paste(product_id, collapse = " ")
  )



user_id =as.data.frame(pre.rec.val$user_id)
colnames(user_id) = c('user_id')

val0 = user_id%>%
  inner_join(submis.val)



missing <- data.frame(
  user_id = unique(pre.rec.val$user_id[!pre.rec.val$user_id %in% val0$user_id]),
  products = "None")



submis.val <- val0 %>% bind_rows(missing) %>% arrange(user_id)

idx.pre <- which(submis.val$products=='None')

get.len.val=sapply(submis.val[,2],function(x) strsplit(as.character(x)," "))
get.len.vec.val=unlist(lapply(get.len.val, length))
#mean(get.len.vec.val)

pre.rec.val$den.pre <- get.len.vec.val


  
  #---------- true 값 ----------#
  t.y <- train.y %>% 
    filter(set=='valid') %>%
    filter(reordered==1) %>%
    select(user_id,product_id) %>%
    group_by(user_id)%>%
    summarise(tmp = paste(product_id, collapse=" "))
  
  val.true<- submis.val %>% select(user_id) %>%
    left_join(t.y, by='user_id')
  
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
  
  
  f1 = mean(pre.rec.val$f1)
  print(f1)
  
 
}


f1 =c(0.725392,0.6943252,0.6794435,0.6686977,0.6544406,0.6395641,0.6269519,0.3821223,0.2919199,0.1013466)
f1.base = c(0.3790548,0.4045941,0.3712607,0.3166594,0.2513047,0.1872107,0.1272546,0.07865653,0.05054259,0.04530452)
f1.lda = c(0.3788131,0.4054196,0.3713868,0.3169915,0.251009,0.1852826,0.125353,0.07734976,0.0496792,0.04530452)
thres =x

plot(c(thres,thres,thres),c(f1,f1.lda,f1.base),xlab = 'threshold(분류기준값)', ylab = 'F1 score')
lines(thres,f1,col = '#009ACD')
lines(thres,f1.base,col ='#FF3030' )
lines(thres,f1.lda,col = "#228B22")
legend(x = 0.85,y =0.7 , c("비교모델1","기본모델",'비교모델2'),  pch=c(1,1,1),col=c("#009ACD","#FF3030","#228B22"))


0.6544406