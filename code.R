library(tidyverse)
library(here)
library(janitor)
library(rdrobust)
library(DBI)
library(lubridate)
library(fixest)
library(broom)
library(scales)

setwd(here::here())

enr1_2018 <- read_csv('data/UDISEplus/2018-2019/100_enr1/nationalEnrol1.csv')

fac_2018 <- read_csv('data/UDISEplus/2018-2019/100_fac.csv')
prof1_2018 <- read_csv('data/UDISEplus/2018-2019/100_prof1.csv')
prof2_2018 <- read_csv('data/UDISEplus/2018-2019/100_prof2.csv')


schemes %>% 
  mutate(year_of_competion = year(date_of_completion)) %>% 
  filter(year_of_competion > 1996, year_of_competion < 2014) %>% 
  mutate(source_type_lump = fct_lump_prop(source_type , 0.01)) %>% 
  count(year_of_competion, source_type_lump) %>% 
  ggplot(aes(x = year_of_competion, y = n, fill = source_type_lump)) + geom_bar(stat = "identity")+ 
  scale_x_continuous(breaks = 1997:2013)+ theme_minimal()+
  scale_y_continuous(labels = comma)+
  labs(x= "Year of completion", y= "Number of schemes", fill = "Type of source")+ 
    theme(axis.line = element_line(size = 1),
          axis.ticks.x = element_line(size = 1, colour = "gray"), 
          axis.ticks.y = element_line(size = 1, colour = "gray"),
          axis.ticks.length = unit(5, "pt"),
          legend.position="bottom",
          # axis.ticks.length = unit(-0.3, "lines"),
          #  axis.ticks.margin = unit(0.5, "lines"),
          #axis.ticks.length=unit(.25, "cm"),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          #axis.ticks.margin = unit(0.5, "lines"),
          text = element_text(size=16)#, axis.text.x = element_text(angle = x_labels_angle, hjust = x_labels_hjust)
    )
  

ggsave('outputs/figures/n_schemes_by_year_and_type_of_source.pdf')


pws_habs <- read_csv('data/HABITATIONS_COVERED_UNDER_PIPE_WATER_SCHEME_PWS_AS_ON_15_04_2013.csv')

pws_habs <- pws_habs %>% clean_names()





schemes <- read_csv('data/LIST_OF_SOURCES_AND_SCHEMES_IN_HABITATIONS_AS_ON_15_APR_13.csv')
schemes <- schemes %>% clean_names()






qual_habs_2009 <- read_csv('data/QUALITY_AFFECTED_HABITATIONS_AS_ON_1_APR_09.csv')
qual_habs_2009 <- qual_habs_2009 %>% 
  clean_names()

schemes <- schemes %>%  
#  filter(date_of_completion > "1901-01-01 00:00:00") %>% 
#  filter(date_of_completion > "1901-01-01 00:00:00", date_of_completion > "1998-01-01 00:00:00") %>% 
  group_by(state_name, district_name, block_name, panchayat_name, village_name, habitation_name) %>% 
  mutate(n_schemes = n()) %>% 
  ungroup()


schemes <- schemes %>% 
  add_count(state_name, district_name, block_name, panchayat_name, village_name, name = "n_schemes_village") 



# 
one_scheme_villages <- schemes %>% 
#  filter(date_of_completion > "1901-01-01 00:00:00")
  filter(n_schemes_village == 1, sanction_year != "Not Known")



qual_villages_2009 <- qual_habs_2009 %>% 
  distinct(state_name, district_name, block_name, panchayat_name, village_name, quality_parameter, .keep_all = T)

one_scheme_villages <- one_scheme_villages %>% 
  left_join(qual_villages_2009, by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name"))


one_scheme_villages %>% 
  count(quality_parameter)




one_scheme_villages %>% 
  semi_join(qual_villages_2009, by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name"))






con <- dbConnect(drv=RSQLite::SQLite(), dbname="data/DISE database/pythonsqlite.db")
dbListTables(con)
village_enrol <- dbGetQuery(con, 'SELECT * FROM village_enrol_table')
dbDisconnect(con)

village_enrol_2017 <- village_enrol %>% 
  filter(AC_YEAR == '2017-18')

village_enrol_2017 <- village_enrol_2017 %>% 
  mutate(BLOCK_NAME = str_trim(BLOCK_NAME),
         VILLAGE_NAME = str_trim(VILLAGE_NAME),
         block_name = str_to_lower(BLOCK_NAME),
         village_name = str_to_lower(VILLAGE_NAME),
         district_name = str_to_lower(DISTNAME)
         )
  


one_scheme_villages %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name_clean = str_remove(district_name, '\\(\\d+\\)$'),
         block_name_clean = str_remove(block_name, '\\(\\d+\\)$'),
         village_name_clean = str_remove(village_name, '\\(\\d+\\s?\\)$')) %>% 
  semi_join(village_enrol_2017,
            by = c( "district_name" = "DISTNAME", "block_name" = "BLOCK_NAME", "village_name" = "VILLAGE_NAME"))


one_scheme_villages %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name),
         district_name_clean = str_remove(district_name, '\\(\\d+\\)$') %>% str_trim(),
         block_name = str_to_lower(block_name),
         block_name_clean = str_remove(block_name, '\\(\\d+\\)$') %>% str_trim(),
         village_name = str_to_lower(village_name),
         village_name_clean = str_remove(village_name, '\\(\\d+\\s?\\)$') %>% str_trim()) %>% 
  semi_join(village_enrol_2017,
            by = c( "district_name", "block_name", "village_name"))



one_scheme_villages %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name),
         district_name = str_remove(district_name, '\\(\\d+\\)$') %>% str_trim(),
         block_name = str_to_lower(block_name),
         block_name = str_remove(block_name, '\\(\\d+\\)$') %>% str_trim(),
         village_name = str_to_lower(village_name),
         village_name = str_remove(village_name, '\\(\\d+\\s?\\)$') %>% str_trim()) %>% 
  semi_join(village_enrol_2017,
            by = c( "district_name", "village_name"))




one_scheme_villages %>% 
  semi_join(village_enrol_2017, by = c("district_name" = "DISTNAME", "block_name" = "BLOCK_NAME", "village_name" = "VILLAGE_NAME")) %>% 
  View()
  count(quality_parameter)

one_scheme_villages %>% 
  semi_join(village_enrol_2017, by = c("district_name" = "DISTNAME", "village_name" = "VILLAGE_NAME")) %>% 
  count(quality_parameter)


village_enrol_2017 %>% 
  as_tibble() %>% 
  semi_join(one_scheme_villages, by = c("DISTNAME" = "district_name", "BLOCK_NAME" = "block_name", "VILLAGE_NAME" = "village_name")) 


village_enrol %>% 
  as_tibble() %>% 
  semi_join(one_scheme_villages, by = c("DISTNAME" = "district_name", "BLOCK_NAME" = "block_name", "VILLAGE_NAME" = "village_name")) %>% 
  View()


village_enrol %>% 
  as_tibble() %>% 
  mutate(BLOCK_NAME = str_trim(BLOCK_NAME),
         VILLAGE_NAME = str_trim(VILLAGE_NAME)) %>% 
  semi_join(one_scheme_villages, by = c("DISTNAME" = "district_name", "BLOCK_NAME" = "block_name", "VILLAGE_NAME" = "village_name")) %>% View()
  count(AC_YEAR)


village_enrol %>% 
  count(AC_YEAR)

 village_enrol %>% 
   filter(AC_YEAR == "2008-09") %>% 
  View()


 village_enrol %>% 
   filter(AC_YEAR == "2008-09") %>% 
   mutate(BLOCK_NAME = str_trim(BLOCK_NAME)) %>% 
   pull(BLOCK_NAME)
 
 
 
one_scheme_villages <- one_scheme_villages %>% 
   mutate(state_name = str_to_lower(state_name),
          district_name = str_to_lower(district_name),
          district_name = str_remove(district_name, '\\(\\d+\\)$') %>% str_trim(),
          block_name = str_to_lower(block_name),
          block_name = str_remove(block_name, '\\(\\d+\\)$') %>% str_trim(),
          village_name = str_to_lower(village_name),
          village_name = str_remove(village_name, '\\(\\d+\\s?\\)$') %>% str_trim())
 
 
 
village_enrol_merged <- village_enrol %>% 
   as_tibble() %>% 
  mutate(BLOCK_NAME = str_trim(BLOCK_NAME),
         VILLAGE_NAME = str_trim(VILLAGE_NAME),
         block_name = str_to_lower(BLOCK_NAME),
         village_name = str_to_lower(VILLAGE_NAME),
         district_name = str_to_lower(DISTNAME)) %>% 
inner_join(one_scheme_villages, by = c("DISTNAME" = "district_name", "BLOCK_NAME" = "block_name", "VILLAGE_NAME" = "village_name")) 
 
village_enrol_merged <- village_enrol %>% 
  as_tibble() %>% 
  mutate(BLOCK_NAME = str_trim(BLOCK_NAME),
         VILLAGE_NAME = str_trim(VILLAGE_NAME),
         block_name = str_to_lower(BLOCK_NAME),
         village_name = str_to_lower(VILLAGE_NAME),
         district_name = str_to_lower(DISTNAME)) %>% 
  inner_join(one_scheme_villages, by = c("district_name", "block_name",  "village_name")) 

village_enrol_merged <- village_enrol %>% 
  as_tibble() %>% 
  mutate(BLOCK_NAME = str_trim(BLOCK_NAME),
         VILLAGE_NAME = str_trim(VILLAGE_NAME),
         block_name = str_to_lower(BLOCK_NAME),
         village_name = str_to_lower(VILLAGE_NAME),
         district_name = str_to_lower(DISTNAME)) %>% 
  inner_join(one_scheme_villages, by = c("district_name",  "village_name")) 


 

village_enrol_merged <- village_enrol_merged %>% 
  mutate(
    ac_year_numeric = str_sub(AC_YEAR, start = 1, end = 4) %>% as.numeric(),
    ac_year_date = ymd(as.numeric(str_c(ac_year_numeric, '0901'))),
    panel_id = str_c(state_name.x, district_name, block_name, village_name, sep = '-'),
    treatment = ifelse(date_of_completion <= ac_year_date, 1 , 0),
    log_total_enrol = log(1 + C1_TOTB + C1_TOTG),
    boys_to_girls = (C1_TOTB +0.1)/(C1_TOTB + C1_TOTG +0.1),
    st_dist_year = str_c(state_name.x, district_name, AC_YEAR, sep = '-')
    ) %>% 
  arrange(state_name.x, district_name, block_name, village_name, ac_year_date)
 
 

village_enrol_merged <- village_enrol_merged %>% 
  mutate(
    ac_year_numeric = str_sub(AC_YEAR, start = 1, end = 4) %>% as.numeric(),
    ac_year_date = ymd(as.numeric(str_c(ac_year_numeric, '0901'))),
    panel_id = str_c(state_name.x, district_name, village_name, sep = '-'),
    treatment = ifelse(date_of_completion <= ac_year_date, 1 , 0),
    log_total_enrol = log(1 + C1_TOTB + C1_TOTG),
    boys_to_girls = (C1_TOTB +0.1)/(C1_TOTB + C1_TOTG +0.1),
    st_dist_year = str_c(state_name.x, district_name, AC_YEAR, sep = '-')
  ) %>% 
  arrange(state_name.x, district_name, village_name, ac_year_date)



village_enrol_merged_panel <- panel(village_enrol_merged, ~ panel_id + ac_year_numeric, duplicate.method = 'first')

village_enrol_merged_panel <- village_enrol_merged_panel %>% 
  group_by(panel_id) %>% 
  mutate(switch_dummy = treatment - lag(treatment),
         switch_dummy = ifelse(is.na(switch_dummy), 0, switch_dummy),
         row_id = row_number(), 
         any_treatment = sum(switch_dummy),
         switch_id = ifelse(any_treatment == 0, -1000,  row_id[ceiling(abs(switch_dummy)) == 1]),
         window_id  = row_id - switch_id) %>% 
  ungroup()# %>%   arrange(org_region, row_id)



(fe_model <- feols(log_total_enrol ~ treatment|state_name.x^DISTNAME^BLOCK_NAME^VILLAGE_NAME + AC_YEAR, data = village_enrol_merged))


(fe_model <- feols(log_total_enrol ~ l(treatment,  -3:3), village_enrol_merged, panel.id = ~panel_id + ac_year_numeric))

(fe_model <- feols(log_total_enrol ~ l(treatment,  -2:3)|panel_id + AC_YEAR, village_enrol_merged_panel))
(fe_model <- feols(log_total_enrol ~ l(treatment,  -4:4)|panel_id + st_dist_year, village_enrol_merged_panel))


(fe_model <- feols(log_total_enrol ~ l(treatment,  -2:4)|panel_id + AC_YEAR, village_enrol_merged_panel))
(fe_model <- feols(boys_to_girls ~ l(treatment,  -2:4)|panel_id + AC_YEAR, village_enrol_merged_panel))


# i(time_to_treatment, ref = c(-1, -1000))

(tw_fe <- feols(log_total_enrol ~  i(window_id, ref = c(-1, -1000)) |panel_id + AC_YEAR, village_enrol_merged_panel))

(tw_fe <- feols(log_total_enrol ~  i(window_id, any_treatment,  ref = c(-1, -1000)) |panel_id + AC_YEAR, village_enrol_merged_panel %>%  filter(date_of_completion > "1901-01-01 00:00:00")))

(tw_fe <- feols(log_total_enrol ~  i(window_id, any_treatment,  ref = c(-1)) |panel_id + AC_YEAR, village_enrol_merged_panel %>%  filter(window_id != -1000), cluster = ~panel_id + AC_YEAR))

# switch_id is period of treatment

(tw_fe <- feols(log_total_enrol ~  i(window_id, ref = c(-1, -1000)) |panel_id + st_dist_year, village_enrol_merged_panel))

summary(tw_fe)


res_sa20 <- feols(log_total_enrol ~  sunab(switch_id, window_id, ref.c = -1000) | panel_id + AC_YEAR, village_enrol_merged_panel%>% 
                    filter(source_type == "Deep Tubewell") %>%  filter(date_of_completion > "1901-01-01 00:00:00", window_id < 12, window_id >-12))


res_sa20 <- feols(log_total_enrol ~  sunab(switch_id, window_id) | panel_id + st_dist_year, village_enrol_merged_panel,
                  cluster = ~panel_id)



#♥ Tohle dát do prezentace (jen vyhodit ty plus minus 12 lead lags)
res_sa20 <- feols(log_total_enrol ~  sunab(switch_id, window_id, ref.c = -1000) | panel_id + st_dist_year, village_enrol_merged_panel,
                  cluster = ~ district_name + AC_YEAR)

summary(res_sa20)

res_sa20 %>% #summary() %>% 
  tidy(conf.int = T)


res_sa20 %>% tidy() %>% 
  mutate(conf_low = estimate - std.error * qnorm(0.975),
         conf_high = estimate + std.error * qnorm(0.975),
         year_relative_to_treat = str_remove(term, 'window_id::') %>% as.integer()) %>% 
  filter(year_relative_to_treat > -7, year_relative_to_treat < 7) %>% 
  ggplot(aes(x = year_relative_to_treat, y = estimate, ymin = conf_low, ymax = conf_high)) + geom_pointrange() +
  labs(x= 'Year relative to tubewell introduction', y = element_blank()) +
  theme_minimal()+ scale_x_continuous(breaks = -6:6)+
  theme(axis.line = element_line(size = 1),
        axis.ticks.x = element_line(size = 1, colour = "gray"), 
        axis.ticks.y = element_line(size = 1, colour = "gray"),
        axis.ticks.length = unit(5, "pt"),
        legend.position="bottom",
        # axis.ticks.length = unit(-0.3, "lines"),
        #  axis.ticks.margin = unit(0.5, "lines"),
        #axis.ticks.length=unit(.25, "cm"),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
        #axis.ticks.margin = unit(0.5, "lines"),
        text = element_text(size=16)#, axis.text.x = element_text(angle = x_labels_angle, hjust = x_labels_hjust)
  )
  

ggsave('outputs/figures/tw_evenstudy_sa_total_enrol.pdf')



tw_fe %>% tidy(conf.int = T) %>% 
  mutate(year_relative_to_treat = str_remove(term, 'window_id::') %>% as.integer()) %>% 
  filter(year_relative_to_treat > -6, year_relative_to_treat < 8) %>% 
  ggplot(aes(x = year_relative_to_treat, y = estimate, ymin = conf.low, ymax = conf.high)) + geom_pointrange()


iplot(list(tw_fe, res_sa20), sep = 0.5)

village_enrol_merged_panel <- village_enrol_merged_panel %>% 
  group_by(panel_id) %>% 
  mutate(first_ac_year = first(ac_year_numeric)) %>% 
  ungroup() %>% 
  mutate(first_treat = ifelse(switch_id == -1000, 0, first_ac_year + switch_id - 1),
         panel_id_num = as.numeric(as.factor(panel_id)))

village_enrol_merged_panel %>% 
  count(panel_id_num, ac_year_numeric, first_treat, sort = T)


village_enrol_merged_panel <- village_enrol_merged_panel %>% 
  group_by(panel_id_num) %>% 
  mutate(first_treat_uniq = first(first_treat)) %>% 
  ungroup(panel_id_num)
  

village_enrol_merged_panel <- village_enrol_merged_panel %>% 
  distinct(panel_id_num, ac_year_numeric, .keep_all = T)

out <- att_gt(yname = "log_total_enrol",
              gname = "first_treat_uniq",
              idname = "panel_id_num",
              tname = "ac_year_numeric",
              xformla = ~1,
              data = village_enrol_merged_panel,
              est_method = "reg"
)

summary(out)

out <- did_multiplegt(village_enrol_merged_panel, "log_total_enrol", "panel_id", "ac_year_numeric", "treatment")

summary(out)

"placebo"
out <- did_multiplegt(village_enrol_merged_panel, "log_total_enrol", "panel_id", "ac_year_numeric", "treatment", placebo = 3, dynamic = 3)


data(mpdta)
library(did)
library(DIDmultiplegt)

summary(tw_fe)

summary(fe_model, cluster = ~panel_id + AC_YEAR)


village_enrol_merged %>% 
  filter(source_type == "Deep Tubewell")

village_enrol_merged %>% 
  filter(date_of_completion <= "1901-01-01 00:00:00")


village_enrol_merged %>% 
  filter(date_of_completion > "1901-01-01 00:00:00")






schemes %>% 
  count(scheme_name, sort = T) %>% View()



habs_2009 <- read_csv('data/BASIC_HABITATION_INFORMATION_AS_ON_1_APR_09.csv')

habs_2010 <- read_csv('data/BASIC_HABITATION_INFORMATION_AS_ON_1_APR_10.csv')

qual_habs_2009 <- read_csv('data/QUALITY_AFFECTED_HABITATIONS_AS_ON_1_APR_09.csv')
qual_habs_2009 <- qual_habs_2009 %>% 
  clean_names()


qual_habs_2009 %>% 
  count(quality_parameter)

habs_2010 <- habs_2010 %>% 
  clean_names()


habs_2009 <- habs_2009 %>% 
  clean_names()

habs_2009 <- habs_2009 %>% 
  mutate(sc_pop_share = sc_current_population/(general_current_population + sc_current_population + st_current_population),
         st_pop_share = st_current_population/(general_current_population + sc_current_population + st_current_population),
         covered_share = (general_covered_population + sc_covered_population + st_covered_population)/(general_current_population + sc_current_population + st_current_population))

library(readxl)
Export_Data_For_House_Connections_Title_2_ <- read_excel("data/List of House Connections provided/2014-2015/Export Data For House Connections _ Title (2).xls")
all_states_pws <- read_excel("data/List of House Connections provided/2014-2015/all_states.xlsx")



all_states_pws <- all_states_pws %>% 
  mutate_at(vars(c('TotHouseHolds', 'TotPWS', 'HCUptodate', 'PrivateConnection')), as.numeric) %>% 
  mutate(pws_hh_gain = HCUptodate - TotPWS,
         pre_pws_share = (TotPWS)/TotHouseHolds)


all_states_pws %>% 
  filter(pre_pws_share != 0.25, pre_pws_share != 0.5) %>% 
  ggplot(aes(x = pre_pws_share)) + geom_histogram(bins = 50)


all_states_pws %>% 
  filter(pre_pws_share != 0.25, pre_pws_share != 0.5) %>% 
  ggplot(aes(x = pre_pws_share)) + geom_histogram(bins = 70)




Export_Data_For_House_Connections_Title_2_ <- read_excel("data/List of House Connections provided/2014-2015/Export Data For House Connections _ Title (3).xls")
Export_Data_For_House_Connections_Title_2_ <- read_excel("data/List of House Connections provided/2014-2015/Export Data For House Connections _ Title (3).xls")

joined_habs <- pws_habs %>% 
  inner_join(habs_2009, by = c('state_name', ))
  

joined_habs <- pws_habs %>% 
  inner_join(habs_2009, by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name", "habitation_name"))

habs_2009_pws <- habs_2009 %>% 
  left_join(pws_habs %>% rename(pws_status = status), by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name", "habitation_name"))






habs_2009_pws %>% 
  count(pws_status)

habs_2009_pws <- habs_2009_pws %>% 
  mutate(pws = ifelse(is.na(pws_status), "No PWS", pws_status),
         pws_dummy = ifelse(is.na(pws_status), 0, 1), 
         total_current_pop = sc_current_population + st_current_population + general_current_population)


habs_2009_pws %>% 
  #filter(sc_concentrated == "YES") %>% 
  count(pws_dummy)


habs_2009 %>% 
  filter(sc_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(sc_pop_share < 1) %>%
  ggplot(aes(x = sc_pop_share)) + geom_histogram()



y_var <- habs_2009_pws$pws_dummy
est <- rdrobust(y=y_var, x=habs_2009_pws$sc_pop_share, p=1, h=0.2, c=0.4)



y_var <- habs_2009_pws$pws_dummy
est <- rdrobust(y=y_var, x=habs_2009_pws$sc_pop_share, p=1, h=0.2, c=0.4)
rdplot(y=y_var, x=habs_2009_pws$sc_pop_share, subset=-est$bws[1,1] + 0.4<= habs_2009_pws$sc_pop_share & habs_2009_pws$sc_pop_share <= est$bws[1,2] +0.4,
       binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1,
       title="", 
       y.label="Meters above sea level",
       x.label="Distance to CP border (in km) - negative within CP")


rdplot(y=y_var, x=habs_2009_pws$sc_pop_share, subset=-est$bws[1,1] + 0.4<= habs_2009_pws$sc_pop_share & habs_2009_pws$sc_pop_share <= est$bws[1,2] +0.4,
       binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1,c = 0.4,
       title="", 
       y.label="Meters above sea level",
       x.label="Distance to CP border (in km) - negative within CP")







y_var <- habs_2009_pws$pws_dummy
est <- rdrobust(y=y_var, x=habs_2009_pws$total_current_pop, p=1, h=200, c=1000)

cutoff <- 1000

run_var <- habs_2009_pws$total_current_pop

rdplot(y=y_var, x=run_var, subset=-est$bws[1,1] + cutoff<= run_var & run_var <= est$bws[1,2] +cutoff,
       binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1,
       title="", c = cutoff,
       y.label="Meters above sea level",
       x.label="Distance to CP border (in km) - negative within CP")


summary(est)



y_var <- all_states_pws$pws_hh_gain
run_var <- all_states_pws$pre_pws_share
cutoff <- 0.5

est <- rdrobust(y=y_var, x=run_var, p=1, h=0.25, c=cutoff)


rdplot(y=y_var, x=run_var, subset=-est$bws[1,1] + cutoff<= run_var & run_var <= est$bws[1,2] +cutoff,
       binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1,
       title="", c = cutoff,
       y.label="Meters above sea level",
       x.label="Distance to CP border (in km) - negative within CP")


summary(est)

schemes %>% 
  count(sanction_year, sort = T)


schemes_filter <- schemes %>% 
  filter(sanction_year %in% c('2012-2013', '2009-2010', '2011-2012', '2010-2011'))

schemes_filter_agg <- schemes_filter %>% 
  group_by(state_name, district_name, block_name, panchayat_name, village_name, habitation_name) %>% 
  summarise(n_schemes = n(), total_costs = sum(estimated_cost))
  


schemes_join <- schemes_filter_agg %>% 
  left_join(habs_2009, by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name", "habitation_name"))


schemes_joined <- habs_2009 %>% 
  left_join(schemes_filter_agg, by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name", "habitation_name"))




schemes_joined <- schemes_joined %>% 
  mutate(n_schemes_imp = ifelse(is.na(n_schemes), 0, n_schemes),
         total_costs_imp = ifelse(is.na(total_costs), 0, total_costs),
         any_scheme_dummy = ifelse(n_schemes_imp > 0, 1, 0), 
         total_pop = sc_current_population + st_current_population + general_current_population)





y_var <- schemes_joined$any_scheme_dummy
y_var <- log(schemes_joined$total_costs_imp +1)


est <- rdrobust(y=y_var, x=run_var, p=1, h=100, c=cutoff)

summary(est)



cutoff <- 250
run_var <- schemes_joined$sc_pop_share
run_var <- schemes_joined$total_pop


rdplot(y=y_var, x=run_var, subset=-est$bws[1,1] + cutoff<= run_var & run_var <= est$bws[1,2] +cutoff,
       binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1,
       title="", c = cutoff,
       y.label="Meters above sea level",
       x.label="Distance to CP border (in km) - negative within CP")





summary(est)



habs_2009 %>% 
  filter(sc_concentrated == "YES") %>% 
  summarize(sc_pop_share_min = min(sc_pop_share, na.rm = T))



habs_2009 %>% 
 # filter(sc_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(sc_pop_share < 1) %>%
  ggplot(aes(x = sc_pop_share)) + geom_histogram()


habs_2009 %>% 
  # filter(sc_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(sc_pop_share < 1, sc_pop_share != 0) %>%
  ggplot(aes(x = sc_pop_share)) + geom_histogram(bins = 50) + geom_vline(xintercept = 0.4)


habs_2009_pws %>% 
  # filter(sc_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(total_current_pop < 500) %>%
  ggplot(aes(x = total_current_pop)) + geom_histogram(bins = 50) + geom_vline(xintercept = 250)


habs_2009_pws %>% 
  # filter(sc_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(total_current_pop < 1500, total_current_pop >150) %>%
  ggplot(aes(x = total_current_pop)) + geom_histogram(bins = 70) + geom_vline(xintercept = 250)


habs_2009_pws %>% 
  # filter(sc_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(total_current_pop < 1500) %>%
  ggplot(aes(x = total_current_pop)) + geom_histogram(bins = 70) + geom_vline(xintercept = 250)





habs_2009 %>% 
  filter(sc_pop_share==0.5) %>% View()



habs_2009 %>% 
  #filter(st_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
  filter(st_pop_share < 1, st_pop_share!=0) %>%
  ggplot(aes(x = st_pop_share)) + geom_histogram()


habs_2009 %>% 
  #filter(st_concentrated == "YES") %>% 
  #filter(sc_pop_share < 1.001) %>%
 # filter(c) %>%
  ggplot(aes(x = covered_share)) + geom_histogram()




habs_2009 %>% 
  count(status)

habs_2010 %>% 
  count(status)

