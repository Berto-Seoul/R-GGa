pacman::p_load(dplyr,stringr,writexl,readxl,stringi,naniar,tidyverse)
library(tidytext)
library(ggwordcloud)
library(ggplot2)

# ========== 기본 데이터 불러오기
load("p.fre.meat.Rdata")
meat <- p.fre.meat
write.csv(meat, file = "meat.csv")

# write.csv(b, file = "b.csv")

# 1. 가구별로 구분
# 1) middle과 detail 나눈 후, 그리고 유통채널별 비중을 살펴보자
# 2) 

# ================= 연도별 총 구매액 표시
whole_meat_purchase <- meat %>% 
  group_by(HH, year) %>% 
  summarise(sum_purchase = sum(purchase),
            freq = n()) 

write.csv(whole_meat_purchase, file = "whole.csv")



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
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month) %>% 
  summarise(freq = n(), sum = sum(purchase))

whole_meat2 <- whole_meat %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month) %>% 
  summarise(freq.mean=mean(freq), sum.mean = mean(sum)) %>% 
  mutate(purchase.1 = sum.mean/freq.mean,
         freq.1 = ifelse(HH == '525가구', freq.mean/525,
                         ifelse(HH == '1040가구', freq.mean/1040, freq.mean/1250 )),
         sum.1 = ifelse(HH == '525가구', sum.mean/525,
                         ifelse(HH == '1040가구', sum.mean/1040, sum.mean/1250)))

# ========== 채널도 같이 분석할 수 있는 것은 무엇일까
names(meat)

real_whole_meat <- meat %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle) %>% 
  summarise(freq = n(), sum = sum(purchase))

real_whole_meat2 <- real_whole_meat %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle) %>% 
  summarise(freq.mean=mean(freq), sum.mean = mean(sum)) %>% 
  mutate(purchase.1 = sum.mean/freq.mean,
         freq.1 = ifelse(HH == '525가구', freq.mean/525,
                         ifelse(HH == '1040가구', freq.mean/1040, freq.mean/1250 )),
         sum.1 = ifelse(HH == '525가구', sum.mean/525,
                        ifelse(HH == '1040가구', sum.mean/1040, sum.mean/1250)))


write.csv(real_whole_meat2, file = "real.whole_meat.csv")

# =========== whole
# whole <- meat %>% 
#   group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle, product) %>% 
#   summarise(freq = n(), sum = sum(purchase))
# 
# whole2 <- whole %>% 
#   group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle, product) %>% 
#   summarise(freq.mean=mean(freq), sum.mean = mean(sum)) %>% 
#   mutate(purchase.1 = sum.mean/freq.mean,
#          freq.1 = ifelse(HH == '525가구', freq.mean/525,
#                          ifelse(HH == '1040가구', freq.mean/1040, freq.mean/1250 )),
#          sum.1 = ifelse(HH == '525가구', sum.mean/525,
#                         ifelse(HH == '1040가구', sum.mean/1040, sum.mean/1250)))
# 
# 
# write.csv(whole2, file = "whole.csv")
# 


unique(meat$retail_wide)
unique(meat$retail_middle)

meat %>% group_by(retail_wide, retail_middle) %>% 
  summarise(number = n())

# == 이전 
# whole_meat <- meat %>% 
#   group_by(HH, wide_new, middle_new, year, month) %>% 
#   summarise(freq = n(), sum = sum(purchase)) %>% 
#   mutate(purchase.1 = sum/freq,
#          freq.1 = ifelse(HH == '525가구', freq/525,
#                          ifelse(HH == '1040가구', )))


write.csv(whole_meat2, file = "whole_meat.csv")

# =================== 축종별 
# === middel_new
mid_meat <- meat %>% 
# === detail_new

# === subdetail_new




# =================== 유통채널널
#1) 전체적인 유통 채널 별

#2) 세부 카테고리별 유통통로



# =================== 심심해서 해보는 워드클라우드
word <- meat

unique(word$product)
# 요리 용도에 맞게 뽑아낼 수 는 없을까?
# === 구이용, 양념, 불고기, 국거리, 갈비, 꼬리, 
unique(word$middle_new)
# ===== 돼지고기로 일단 해보자
word_pork <- word %>% filter(middle_new == "돼지고기")
unique(word_pork$product)
names(word_pork)
unique(word_pork$sub_detail_new)

a <- word_pork %>% group_by(sub_detail_new) %>% 
  summarise(number = n())



# =================== 총구매액 관련 데이터 만들기

purchase <- meat %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle) %>% 
  summarise(freq = n(), sum = sum(purchase))

write.csv(purchase, file = "purchase.csv")


# a <- meat %>% 
#   group_by(middle_new, sub_detail_new) %>% 
#   summarise(number = n())
# View(a)
# write.csv(a, file = "category.csv")

# ================== 베이커
unique(meat$retail_middle)
bakery <- meat %>%
  filter(retail_middle == "베이커리")

unique(meat$product)
