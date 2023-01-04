path_read <- '\\\\cdc.gov\\locker\\CSELS_DHIS_SDB_MVPS\\SOT\\George_Arthur'
#df_mpx <- read.csv(paste0(path_read, "\\Data\\mpx_ed_state.csv"), header = T)

df_mpx_ed <- data.table::fread(paste0(path_read, "\\Data\\mpx_ed_state.csv"), header = T)

df_race <- df_mpx_ed %>% 
  select(mmwr_week_end,
         race,
         mpx_visits, 
         all_visits) %>% 
  group_by(mmwr_week_end, race) %>% 
  summarise(mpx_visits = sum(mpx_visits), all_visits = sum(all_visits)) %>% distinct()

#Create race/ethnicity column
spinoff <- within(df_race, RACE_ETH<-ifelse(ethnicity %in% c("Unknown","Not Reported or Null"),"Not Reported or Null",
                                            ifelse(ethnicity == "Hispanic or Latino", "Hispanic or Latino", 
                                            ifelse(race =="American Indian or Alaska Native", "American Indian or Alaska Native", 
                                            ifelse(race %in% c("Asian", "Native Hawaiian or Other Pacific Islander"), "Asian or Other Pacific Islander", 
                                            ifelse(race == "Black or African American", "Black or African American",
                                            ifelse(race == "White", "White, non-hispanic",
                                            ifelse(race %in% c("Unknown", "Not Reported or Null"), "Not Reported or Null",
                                            "Multiracial"))))))))


df_spin <- df_race %>% select(-all_visits) %>% 
  pivot_wider(names_from = race,
              values_from = mpx_visits,
              values_fn = list(mpx_visits= sum)) %>% 
  mutate(mmwr_week_end=months(mmwr_week_end)) %>% 
  group_by(mmwr_week_end) 

df_spinned <- aggregate(.~mmwr_week_end,data = df_spin, FUN = sum) %>% clean_names()

#df_spinned$mmwr_week_end <- factor(df_spinned$mmwr_week_end, levels = month.name)

df_spin_update <- data.frame(cbind(df_spinned[1],
                               t(apply(df_spinned[2:12], MARGIN = 1, 
                                      function(x) ((x /sum(x))*100) %>% round(digits = 2)))))


#spin_per <- spin %>% is.integer(apply(2,FUN = fund))

df_spin_update$mmwr_week_end <- factor(df_spin_update$mmwr_week_end, levels = month.name)

fig_alien <- plot_ly(df_spin_update, x = ~mmwr_week_end, y = ~american_indian_or_alaska_native, 
                     type = 'bar', name = 'american_indian_or_alaska_native') %>%
  add_trace(y =~asian, name = "Asian") %>% 
  add_trace(y =~black_or_african_american, name = "black_or_african_american") %>% 
  add_trace(y =~native_hawaiian_or_other_pacific_islander, name = "native_hawaiian_or_other_pacific_islander") %>% 
  add_trace(y =~unknown, name = "unknown") %>% 
  add_trace(y =~other_race, name = "other_race") %>% 
  add_trace(y =~refused_to_answer, name = "refused_to_answer") %>% 
  add_trace(y =~white, name = "white") %>%
  layout(xaxis = list(title = "Months"),
         yaxis = list(title = '% of mpx visits out of all ED visits'), barmode = "stack")

fig_alien




#trend lines by race

df_race_trend <- df_race %>% select(-all_visits) %>% 
  group_by(mmwr_week_end, race) %>% 
  summarise(mpx_visits = sum(mpx_visits)) %>% 
  ungroup()

#df_race_trend <- aggregate(.~mpx_visits, data=df_race_trend, FUN = sum)

#df_race_trend <- df_race %>% select(-all_visits) %>% group_by(mmwr_week_end)

#ggplot of trend lines
ggplot(df_race_trend, aes(x=mmwr_week_end, y=mpx_visits,color = race)) +
  geom_line(lwd = 1, stat = "identity") +
  geom_point()


#plotly trend line for race

# plotly_race <- df_race_trend %>% pivot_wider(names_from = race,
#                                values_from = mpx_visits,
#                                values_fn = list(mpx_visits= sum)) %>% 
#   clean_names() %>% group_by(mmwr_week_end)

fig_race_trend <- plot_ly(df_race_trend, x = ~mmwr_week_end, y = ~mpx_visits, 
                     type = 'scatter', mode = "lines+markers",name = ~race) %>%
  
  layout(xaxis = list(title = "Months"),
         yaxis = list(title = '% of mpx visits out of all ED visits'))

fig_race_trend



gg_spin <- df_spin_update %>% pivot_longer(2:12, names_to = "Race", values_to = "Count")

ggplot(gg_spin, aes(x=mmwr_week_end, y=Count,fill = Race)) +
  geom_bar(stat = "identity")

  