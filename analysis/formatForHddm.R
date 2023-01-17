# run after running all chunks in Results_exps

E1.df %>%
  filter(test_part %in% c('neutral','bird_loss','bird_bonus') & trial_type=='Disc') %>%
  filter(RT>100 & RT<5000)%>%
  mutate(subj_idx=as.numeric(factor(subj_id)),
         stim = stimulus,
         rt= RT/1000,
         response = ifelse(resp,1,0),
         condition = test_part
  ) %>%
  dplyr::select(subj_idx,stim,rt,response,condition) %>%
  write.csv('..\\experiments\\bird_fish\\data\\E1forDDM.csv')

E2.df %>%
  filter(test_part %in% c('neutral','bird_loss','bird_bonus') & trial_type=='Disc') %>%
  filter(RT>100 & RT<5000)%>%
  mutate(subj_idx=as.numeric(factor(subj_id)),
         stim = stimulus,
         rt= RT/1000,
         response = ifelse(resp,1,0),
         condition = test_part
  ) %>%
  dplyr::select(subj_idx,stim,rt,response,condition) %>%
  write.csv('..\\experiments\\fish_bird2\\data\\E2forDDM.csv')

E4.df %>%
  filter(test_part %in% c('neutral','stripes_loss','noise_loss') & trial_type=='DiscGrating') %>%
  filter(RT>100 & RT<5000)%>%
  mutate(subj_idx=as.numeric(factor(subj_id)),
         stim = stimulus,
         rt= RT/1000,
         response = ifelse(resp,'stripes','noise'),
         condition = test_part
  ) %>%
  dplyr::select(subj_idx,stim,rt,response,condition) %>%
  write.csv('..\\experiments\\detection4\\data\\E4forDDM.csv')

E5.df %>%
  filter(test_part %in% c('neutral','stripes_loss','noise_loss') & trial_type=='DiscGrating') %>%
  filter(RT>100 & RT<5000)%>%
  mutate(subj_idx=as.numeric(factor(subj_id)),
         stim = stimulus,
         rt= RT/1000,
         response = ifelse(resp,'stripes','noise'),
         condition = test_part
  ) %>%
  dplyr::select(subj_idx,stim,rt,response,condition) %>%
  write.csv('..\\experiments\\detection5\\data\\E5forDDM.csv')

E6.df %>%
  filter(test_part %in% c('neutral','stripes_loss','noise_loss') & trial_type=='DiscGrating') %>%
  filter(RT>100 & RT<5000)%>%
  mutate(subj_idx=as.numeric(factor(subj_id)),
         stim = stimulus,
         rt= RT/1000,
         response = ifelse(resp,'stripes','noise'),
         condition = test_part
  ) %>%
  dplyr::select(subj_idx,stim,rt,response,condition) %>%
  write.csv('..\\experiments\\detection6\\data\\E6forDDM.csv')
