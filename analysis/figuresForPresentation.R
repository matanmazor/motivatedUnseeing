# num clicks

p<- E2.click_df %>%
  group_by(subj_id,test_part, genuine_first) %>%
  summarise(num_clicks=median(num_clicks)) %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=num_clicks,fill=test_part))+
  # geom_bar( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(x='median number of clicks', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(0,25)) + 
  scale_y_continuous(limits=c(0,130))+
  theme_classic()
# geom_vline(xintercept= median_simulated)+
# facet_wrap(~genuine_first, nrow=2);

ggsave('../docs/figures/forPresentation/E2_num_clicks_grid_only.png',p,width=5,height=2, dpi=300)

p<- E2.click_df %>%
  group_by(subj_id,test_part, genuine_first) %>%
  filter(test_part=='nonpretend')%>%
  summarise(num_clicks=median(num_clicks)) %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=num_clicks,fill=test_part))+
  geom_bar( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(x='median number of clicks', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(0,25)) + 
  scale_y_continuous(limits=c(0,130))+
  theme_classic()
# geom_vline(xintercept= median_simulated)+
# facet_wrap(~genuine_first, nrow=2);

ggsave('../docs/figures/forPresentation/E2_num_clicks_nonpretend_only.png',p,width=5,height=2, dpi=300)

p<- E2.click_df %>%
  group_by(subj_id,test_part, genuine_first) %>%
  summarise(num_clicks=median(num_clicks)) %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=num_clicks,fill=test_part))+
  geom_bar( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(x='median number of clicks', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(0,25)) + 
  scale_y_continuous(limits=c(0,130))+
  theme_classic()
# geom_vline(xintercept= median_simulated)+
# facet_wrap(~genuine_first, nrow=2);

ggsave('../docs/figures/forPresentation/E2_num_clicks.png',p,width=5,height=2, dpi=300)

# optimality

p<- E2.mean_P_click_rank %>%
  filter(test_part == 'random') %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=p_click_rank,fill=test_part))+
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#FDE725")) +
  labs(x='p(ship in clicked square) - rank', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(1,12)) + 
  scale_y_continuous(limits=c(0,150))+
  # facet_wrap(~genuine_first, nrow=2)+
  # scale_x_reverse() +
  theme_classic()+
  theme(legend.position='none');

ggsave('../docs/figures/forPresentation/E2_pclick_rank_random_only.png',p,width=5,height=2,dpi=300)

p<- E2.mean_P_click_rank %>%
  filter(test_part == 'random' | test_part=='nonpretend') %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=p_click_rank,fill=test_part))+
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2","#FDE725")) +
  labs(x='p(ship in clicked square) - rank', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(1,12)) + 
  scale_y_continuous(limits=c(0,150))+
  # facet_wrap(~genuine_first, nrow=2)+
  # scale_x_reverse() +
  theme_classic()+
  theme(legend.position='none');;

ggsave('../docs/figures/forPresentation/E2_pclick_rank_random_and_nonpretend.png',p,width=5,height=2,dpi=300)

p<- E2.mean_P_click_rank %>%
  filter(test_part == 'random' | test_part=='nonpretend' | test_part=='pretend') %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=p_click_rank,fill=test_part))+
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2","#404080","#FDE725")) +
  labs(x='p(ship in clicked square) - rank', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(1,12)) + 
  scale_y_continuous(limits=c(0,150))+
  # facet_wrap(~genuine_first, nrow=2)+
  # scale_x_reverse() +
  theme_classic()+
  theme(legend.position='none');;

ggsave('../docs/figures/forPresentation/E2_pclick_rank.png',p,width=5,height=2,dpi=300)

p<- E2.mean_P_click_rank_misses_only %>%
  filter(test_part == 'random' | test_part=='nonpretend' | test_part=='pretend') %>%
  mutate(genuine_first = ifelse(genuine_first, 'non-pretend then pretend', 'pretend then non-pretend'))%>%
  ggplot(aes(x=p_click_rank,fill=test_part))+
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2","#404080","#FDE725")) +
  labs(x='p(ship in clicked square) - rank', y='number of players')+
  theme(legend.position=c(0.10,0.85)) +
  scale_x_continuous(limits=c(1,12)) + 
  scale_y_continuous(limits=c(0,150))+
  # facet_wrap(~genuine_first, nrow=2)+
  # scale_x_reverse() +
  theme_classic()+
  theme(legend.position='none');;

ggsave('../docs/figures/forPresentation/E2_pclick_rank_misses_only.png',p,width=5,height=2,dpi=300)

plot_posterior <- function(posterior,file_name) {
  posterior <- scan(text= posterior, what = numeric(), sep="," , quiet = TRUE);
  board_df <- data.frame(i=rep(c(1,2,3,4,5),each=5),j=rep(c(1,2,3,4,5),5),posterior=posterior);
  p <- ggplot(board_df,aes(x=j,y=i,fill=posterior)) +
    geom_tile() +
    coord_equal() +
    scale_x_continuous(breaks=c()) +
    scale_y_continuous(breaks=c(),trans='reverse') +
    scale_fill_gradient(low='#649bc8',high='#c83237', na.value ='white')+
    labs(x='',y='') +
    theme_classic();
  
  ggsave(paste('figures/forPresentation/',file_name,'.png',sep=''),width=5,dpi=300);
}

boardA <- "NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,1,NA,NA,NA,NA"
posteriorA <- boardA%>%get_likelihood()%>%get_posterior();
plot_posterior(posteriorA,'posterior_high');

boardB <- "NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,1,NA,NA,NA"
posteriorB <- boardB%>%get_likelihood()%>%get_posterior();
plot_posterior(posteriorB,'posterior_medium');