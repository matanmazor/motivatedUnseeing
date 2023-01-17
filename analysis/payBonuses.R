E7.df %>%
  group_by(subj_id) %>%
  summarize(points=mean(as.numeric(points))) %>%
  mutate(top30=points>=quantile(points,0.7), 
         top5=points>=quantile(points,0.95), 
         bonus=top30*2.25+top5*2.25) %>%
  filter(bonus>0) %>%
  arrange(-points) %>% 
  dplyr::select(subj_id,bonus) %>% 
  write.table('bonuses7.csv', sep=',', row.names=F, col.names=F, quote=F)