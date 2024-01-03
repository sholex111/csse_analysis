
library(here)
library(tidyverse)
library(openxlsx)
library(janitor)


here()

#load data
raw <- read.xlsx('csse_historical.xlsx', sheet = 'raw' )
stand <- read.xlsx('csse_historical.xlsx', sheet = 'standardized' )
head(raw)


#clean data
stand <- stand %>% clean_names()
raw <- raw %>% clean_names()
stand <- as_tibble(stand)
raw <- as_tibble(raw)

#average score per year
mean_score <- stand %>% group_by(year,gender) %>% summarise(avg = mean(total))
mean_score


#how many people scored above average each year
above_avg <- stand %>% 
  group_by(year, gender) %>% 
  summarise(all = n(), 
            above_avg = sum(total > mean(total)))

above_p <- stand %>% 
  group_by(year, gender) %>% 
  mutate(p60 = quantile(total, probs = 0.6),
         p70 = quantile(total, probs = 0.7),
         p80 = quantile(total, probs = 0.8),
         p90 = quantile(total, probs = 0.9)) %>% 
  summarise(all = n(), 
            above_avg = sum(total > mean(total)),
            above_p60 = sum(total > p60),
            above_p70 = sum(total > p70),
            above_p80 = sum(total > p80),
            above_p90 = sum(total > p90),
            )


above_p_plus <- stand %>% 
  group_by(year, gender) %>% 
  mutate(p60 = quantile(total, probs = 0.6),
         p70 = quantile(total, probs = 0.7),
         p80 = quantile(total, probs = 0.8),
         p90 = quantile(total, probs = 0.9)) %>% 
  summarise(all = n(), 
            p50 = quantile(total, 0.5),
            mean = mean(total),
            above_avg = sum(total > mean(total)),
            p60 = quantile(total, 0.6),
            above_p60 = sum(total > p60),
            p70 = quantile(total, 0.7),
            above_p70 = sum(total > p70),
            p80 = quantile(total, 0.8),
            above_p80 = sum(total > p80),
            p90 = quantile(total, 0.9),
            above_p90 = sum(total > p90),
            ) %>% 
  arrange(desc(year), desc(gender))

#output the summary data
write.xlsx(above_p_plus, file = 'csse_21_23_summary.xlsx')


#more clean raw score
head(raw)

#add new columns
raw <- raw %>% mutate(full = english + maths)

#convert date for into 2 columns because of separate format
raw <- raw %>% mutate(month_pre1 = as.Date(as.numeric(raw$birth_date), origin= "1899-12-30"))


raw <- raw %>%
  mutate(month_pre2 = parse_date_time(birth_date, 
                                      orders = c("mdy"), 
                                      truncated = 2))


raw <- raw %>% mutate(bd = as.Date(as.numeric(birth_date), origin= "1899-12-30"))

#combine (colaesce) the 2 date columns
raw <- raw %>% mutate(bd = coalesce(month_pre1,month_pre2))

# Format the date column as "mmm dd"
#raw$bd <- format(raw$bd, "%b %d")

#raw$bd_full <- parse_date_time(raw$birth_date, orders = c("mdy", "b-y"))



#extract month number
raw <- raw %>% mutate(month_bd = month(bd))



#standardize month_bd such that aug = 1 and sep = 12
raw$mth_st <- case_when(
  raw$month_bd == 1 ~ 8, raw$month_bd == 2 ~ 7, raw$month_bd == 3 ~ 6,
  raw$month_bd == 4 ~ 5, raw$month_bd == 5 ~ 4, raw$month_bd == 6 ~ 3,
  raw$month_bd == 7 ~ 2, raw$month_bd == 8 ~ 1, raw$month_bd == 9 ~ 12,
  raw$month_bd == 10 ~ 11, raw$month_bd == 11 ~ 10, raw$month_bd == 12 ~ 9
)

#insights from raw scores

#what is the average min and max score per gender per year
score_year_param <- raw %>% 
  group_by(year,gender) %>% 
  summarise(avg_math = mean(maths),
            avg_eng = mean(english),
            max_math = max(maths),
            max_eng = max(english),
            min_math = min(maths),
            min_eng = min(english),
            min_full = min(full),
            max_full = max(full),
            avg_full = mean(full)) %>% 
  arrange(desc(year), desc(gender))


#do scores vary with month of birth
score_month_param<- raw %>% 
  group_by(year, mth_st, gender) %>% 
  summarise(avg_math = mean(maths),
            avg_eng = mean(english),
            avg_full = mean(full)) %>% 
  arrange(desc(year), desc(gender), desc(mth_st))


#output the data

#create a list of the main df
csse_analysis <- list(above_p_plus, score_year_param, score_month_param)
write.xlsx(csse_analysis, file = "full_csse_analysis_21_23.xlsx")



#how many people scored 60 in maths
raw %>% filter(maths == 60) %>% select(year, maths) %>%  group_by(year) %>%  count()

#how many people scored >50 in maths and English
raw %>% filter(maths >= 50 ) %>% select(year, gender, maths , english) %>%  group_by(year, gender) %>%  count()
raw %>% filter(english >= 50) %>% select(year, gender, maths , english) %>%  group_by(year, gender) %>%  count()





