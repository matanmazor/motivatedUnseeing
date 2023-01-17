E1.bias_first_block %>% 
  rbind(E2.bias_first_block) %>%
  rbind(E3.bias_first_block) %>%
  filter(test_part%in%c('bird_bonus','fish_bonus','bonus')) %>%
  ggplot(aes(x=bias,fill=test_part)) +
  geom_histogram(position = "stack",binwidth=0.05) +
  theme_bw() +
  scale_x_continuous(limits=c(0,1)) +
  geom_vline(xintercept=0.5)+
  theme(legend.pos='na')

ggsave('figures/bonus_effect_first_block.png',width=5,height=3)

E1.bias_first_block %>% 
  rbind(E2.bias_first_block) %>%
  rbind(E3.bias_first_block) %>%
  filter(test_part%in%c('bird_loss','fish_loss','loss')) %>%
  ggplot(aes(x=bias,fill=test_part)) +
  geom_histogram(position = "stack",binwidth=0.05) +
  theme_bw() +
  scale_x_continuous(limits=c(0,1)) +
  geom_vline(xintercept=0.5)+
  theme(legend.pos='na')

ggsave('figures/loss_effect_first_block.png',width=5,height=3)

E4.bias_first_block %>%
  rbind(E4.bias_first_block) %>%
  rbind(E6.bias_first_block) %>%
  ggplot(aes(x=bias, fill=test_part)) +
  geom_density(alpha=0.5,position='identity') +
  scale_x_continuous(limits=c(0,1)) +
  theme_bw()

ggsave('figures/detection_first_block.png',width=5,height=3)


all_discrimination <- E1.bias %>%
  dplyr::select(subj_id,loss_effect,gain_effect)%>%
  rbind(
    E2.bias %>%
      dplyr::select(subj_id,loss_effect,gain_effect)
  ) %>%
  rbind(
    E3.bias %>%
      dplyr::select(subj_id,loss_effect,gain_effect)
  );

all_detection <- E4.bias %>%
  dplyr::select(subj_id,stripes_loss_effect,noise_loss_effect)%>%
  rbind(
    E5.bias %>%
      dplyr::select(subj_id,stripes_loss_effect,noise_loss_effect)
  ) %>%
  rbind(
    E6.bias %>%
      dplyr::select(subj_id,stripes_loss_effect,noise_loss_effect)
  );

all_discrimination %>%
  ggplot(aes(x=loss_effect)) +
  geom_density(alpha=0.5,fill='red',position='identity') +
  geom_density(alpha=0.5,fill='blue',data=all_detection, aes(x=stripes_loss_effect))+
  scale_x_continuous(limits=c(-0.7,0.7))+
  labs(x='response bias effect')+
  geom_vline(xintercept=0)
  
ggsave('figures/loss_effects_block.png',width=5,height=3);

all_discrimination %>%
  ggplot(aes(x=gain_effect)) +
  geom_density(alpha=0.5,fill='red',position='identity') +
  geom_density(alpha=0.5,fill='blue',data=all_detection, aes(x=noise_loss_effect))+
  scale_x_continuous(limits=c(-0.7,0.7))+
  labs(x='response bias effect')+
  geom_vline(xintercept=0)

ggsave('figures/gain_effects_block.png',width=5,height=3);
