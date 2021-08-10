pacman::p_load("stringr","dplyr","writexl","readxl","reshape2")
#install.packages("openxlsx")

library(openxlsx)
load("p.fresh.meat.final2.Rdata")

meat <- p.fresh.meat.final


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

meat.year

write.csv(real_whole_meat2, file = "real.whole_meat.csv")

unique(meat$middle_new)
unique(meat$retail_wide)
#[1] B슈퍼마켓                   A대형마트                   C전문점                     D전통시장_농가직거래_무점포
#[5] G기타                       F편의점                     E인터넷구매  

#[1] 난류     닭고기   돼지고기 쇠고기   기타육류 오리고기
# ============= 소고기
cow <- meat %>% filter(middle_new == '쇠고기')


# ========== 1250, 1050가구 한꺼번에
# 대형마트 기준
cow %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle) %>% 
  summarise(freq.mean = mean(freq), sum.mean = mean(sum)) %>% 
  filter(retail_wide == 'A대형마트') %>% 
  mutate(purchase.1 = sum.mean/freq.mean,
         freq.1 = ifelse(HH == '525가구', freq.mean/525,
                         ifelse(HH == '1040가구', freq.mean/1040, freq.mean/1250 )),
         sum.1 = ifelse(HH == '525가구', sum.mean/525,
                        ifelse(HH == '1040가구', sum.mean/1040, sum.mean/1250))) %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, )

# ========= 이렇게 Set로 가져가면 됩니다. 
cow1 <- cow %>% 
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle) %>% 
  summarise(freq = n(), sum = sum(purchase))

cow2 <- cow1 %>% 
  filter(retail_wide == 'A대형마트') %>%
  group_by(HH, wide_new, middle_new, detail_new, sub_detail_new, year, month, retail_wide, retail_middle) %>% 
  summarise(freq.mean = mean(freq), sum.mean = mean(sum)) %>% 
  mutate(purchase.1 = sum.mean/freq.mean,
         sum.1.525 = ifelse(HH == '525가구', sum.mean/525,0),
         freq.1.525 = ifelse(HH == '525가구', freq.mean/525, 0),
         sum.1.1040 = ifelse(HH == '1040가구', sum.mean/1040,0),
         freq.1.1040 = ifelse(HH == '1040가구', freq.mean/1040, 0),
         sum.1.525 = ifelse(HH == '1250가구', sum.mean/1250,0),
         freq.1.525 = ifelse(HH == '1250가구', freq.mean/1250, 0)) %>% 
  group_by(year) %>% 
  summarise()




#============= 닭고기



#========== 다시
new_meat <- meat %>% group_by(HH, year) %>% 
  summarise(freq = n(), sum = sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         sum.1.525 = ifelse(HH == '525가구', sum/525, 0),
         freq.1.525 = ifelse(HH == '525가구', freq/525, 0),
         sum.1.1040 = ifelse(HH == '1040가구', sum/1040, 0),
         freq.1.1040 = ifelse(HH == '1040가구', freq/1040, 0),
         sum.1.1250 = ifelse(HH == '1250가구', sum/1250, 0),
         freq.1.1250 = ifelse(HH == '1250가구', freq/1250, 0))


new_meat2 <- meat %>% group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))

names(meat)
# ===================== 유통채널 기준으로
unique(meat$retail_wide)
# [1] B슈퍼마켓                   A대형마트                   C전문점                     D전통시장_농가직거래_무점포 G기타                      
# [6] F편의점                     E인터넷구매    

# A대형마트
bigmart <- meat %>% 
  filter(retail_wide == 'A대형마트') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))

# B슈퍼마켓
super <- meat %>% 
  filter(retail_wide == 'B슈퍼마켓') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))


# C전문점
expert <- meat %>% 
  filter(retail_wide == 'C전문점') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))

# D전통시장_농가직거래_무점포
tradition <- meat %>% 
  filter(retail_wide == 'D전통시장_농가직거래_무점포') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))

# E인터넷구매
online <- meat %>% 
  filter(retail_wide == 'E인터넷구매') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))

  
# F편의점
conv <- meat %>% 
  filter(retail_wide == 'F편의점') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))

# G기타
etc <- meat %>% 
  filter(retail_wide == 'G기타') %>% 
  group_by(HH, year) %>% 
  summarise(freq=n(), sum=sum(purchase)) %>% 
  mutate(purchase.1 = sum/freq,
         freq_g =
           ifelse(HH=='1250가구', freq/1250, 
                  ifelse(HH == '1040가구', freq/1040, freq/525)),
         sum_g = 
           ifelse(HH == '1250가구', sum/1250, 
                  ifelse(HH == '1040가구', sum/1040, freq/525)))


# expert <- meat %>% 
# tradition <- meat %>% 
# online <- meat %>% 
# conv <- meat %>% 
# bigmart <- meat %>% 
# super <- meat %>% 
#   etc <- meat %>%

unique(meat$retail_wide)  

write_xlsx(list(`대형마트` = bigmart, `슈퍼` = super, `전문점` = expert, `전통` = tradition,
                `인터넷` = online, `편의점` = conv), "meat.xlsx")  
  
# write_xlsx(list(gyoza.seafood.three.years = gyoza.seafood.three.years, dduari.seafood.three.years = dduari.seafood.three.years,
#                 etc.form.seafood.three.years = etc.form.seafood.three.years, junbyung.seafood.three.years = junbyung.seafood.three.years), "seafood&form.3year.xlsx")

unique(meat$middle_new)
# ================= 소고기 디테일
cow <- meat %>% 
  filter(wide)

