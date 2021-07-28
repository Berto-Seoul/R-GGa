pacman::p_load(dplyr,stringr,writexl,readxl,stringi,naniar,tidyverse)


load("p.fre.meat.Rdata")

meat <- p.fre.meat

names(meat)
head(meat)

a <- meat %>% 
  group_by(HH,middle_new, year) %>% 
  summarise(sum = sum(purchase))

# View(a)

b <- meat %>% 
  group_by(HH,middle_new, detail_new, year) %>% 
  summarise(sum = sum(purchase))

View(b)

unique(meat$store_region)
unique(meat$cate)
unique(meat$sub_detail_new)
unique(meat$HH)

# write.csv(b, file = "b.csv")

# 1. 가구별로 구분
# 1) middle과 detail 나눈 후, 그리고 유통채널별 비중을 살펴보자
# 2) 

# ================= 연도별 총 구매액 표시
whole_meat <- meat %>% 
  group_by(HH, year) %>% 
  summarise(sum_purchase = sum(purchase),
            freq = n()) 



# write.csv(whole_meat, file = "whole_meat.csv")
t_whole_meat <- t(whole_meat)
# write.csv(t_whole_meat, file = "t_whole_meat.csv")

#=================== 525
meat_525 <- meat %>% 
  filter(HH == "525가구")

sum_purchase_525 <- meat_525 %>% 
  group_by(middle_new, detail_new, year) %>% 
  summarise(sum_purchase = sum(purchase), freq = n())

# write.csv(sum_purchase_525, file = "sum_purchase_525.csv")


#================== 1040
meat_1040 <- meat %>% 
  filter(HH == "1040가구")

sum_purchase_1040 <- meat_1040 %>% 
  group_by(middle_new, detail_new, year) %>% 
  summarise(sum_purchase = sum(purchase))


#================== 1250가구
meat_1250 <- meat %>% 
  filter(HH == "1250가구")

sum_purchase_1250 <- meat_1250 %>% 
  group_by(middle_new, detail_new, year) %>% 
  summarise(sum_purchase = sum(purchase))



# ============= test -1250가구로

test.1250 <-  meat_1250 %>% 
  group_by(middle_new, detail_new, year, month) %>% 
  summarise(freq = n(), sum = sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq.1 = freq/1250,
         sum.1 = sum/1250 )

write.csv(test.1250, file = "test.1250.csv")
# squid.product <- squid %>%
#   group_by(sep, wide_new, middle_new, detail,cat, product,year,month) %>% summarise(freq=n(), sum=sum(purchase)) %>%
#   mutate(purchase.1 = sum/freq,
#          freq =
#            ifelse(sep=='1222', freq/1222, freq/835),
#          sum = 
#            ifelse(sep=='1222', sum/1222, sum/835))


# ================ 모든 가구들로 한번 가구당 구매액, 이런 저런 거 한번 봐보자
unique(meat$HH)
# [1] 525가구  1040가구 1250가구
whole_meat <- meat %>% 
  group_by(HH, wide_new, middle_new, year, month) %>% 
  summarise(freq = n(), sum = sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq.1 = ifelse(HH == '525가구', freq/525,
                         ifelse(HH == '1040가구', freq/1040, freq/1250 )),
         sum.1 = ifelse(HH == '525가구', sum/525,
                         ifelse(HH == '1040가구', sum/1040, sum/1250)))

