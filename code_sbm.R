library(tidyverse)
library(here)
library(janitor)

setwd(here::here())

sbm_panch <- read_csv('data/targ_vs_achieved.csv')

# https://sbm.gov.in/sbmReport/Report/Physical/SBM_TargetVsAchievement.aspx

# to get Total HH Detail with Toilet	 = Total Detail Entered (With & Without Toilet) - Detail entered for no. of Households not having toilet	(BPL+APL)		
# we need to 



sbm_panch <- sbm_panch %>% 
  mutate(total_pre_treat_toilet = total_hh_entered - no_toilet_alp_blp,
         pre_toilet_share = total_pre_treat_toilet/total_hh_entered)


sbm_panch %>% 
  ggplot(aes(x = pre_toilet_share)) + geom_histogram()


sbm_panch %>% 
  filter(pre_toilet_share == 1) %>% 
  View()


sbm_panch %>% 
  filter(pre_toilet_share > 0.9) 

sbm_panch_long <- sbm_panch %>% 
  pivot_longer(starts_with('entered_'), names_to = 'year', values_to = 'entered')


sbm_panch_long <- sbm_panch_long %>% 
  mutate(entered_pct = entered/total_hh_entered,
         entered_pct_progress = entered/hh_covered_after_bls,
         year = as.integer(str_remove(year, 'entered_')))


sbm_panch_long %>% 
  filter(year == 2015) %>% 
  ggplot(aes(x = entered_pct)) + geom_histogram()

sbm_panch_long %>% 
  filter(entered_pct != 0) %>% 
  #-filter(year == 2015) %>% 
  ggplot(aes(x = entered_pct)) + geom_histogram() + facet_wrap(~ year)

sbm_panch_long %>% 
  filter(entered_pct_progress != 0) %>% 
  filter(entered_pct_progress < 1.01) %>% 
  #-filter(year == 2015) %>% 
  ggplot(aes(x = entered_pct_progress)) + geom_histogram() + facet_wrap(~ year)


sbm_panch_long %>% 
  filter(year == 2015) %>% 
  ggplot(aes(x =  pre_toilet_share, y = entered_pct)) + geom_point(alpha = 0.2)


