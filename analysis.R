pacman::p_load(dplyr,stringr,writexl,readxl,stringi,naniar,tidyverse)

load("p.fre.meat.Rdata")

meat <- p.fre.meat

names(meat)
str(meat)

# 목표
# 1) 국내 쇠고기 구매패턴을 분석
# 2) 이후 주요 육류(소, 닭, 돼지) 별, 부위(갈비, 등심 별)
# 3) 구매채널(예: 오프라인, 온라인) 별 구체적인 소비트렌드 제시

