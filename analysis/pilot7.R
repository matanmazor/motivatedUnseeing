library("papaja")
library('dplyr')
library('tidyr')
library('tidyverse')
library('ggplot2')
library('jsonlite') # for WSBI
library('stringr')
library('ltm')
library('cocor')
library("fddm")
library('rstatix') # for ANOVA
library('psych') #for comparing correlations



E7.raw_df <- read.table("../experiments/detection7/data/jatos_results_batch1.txt",sep=',', header=TRUE, colClasses=c("stripes_key"="character")) %>%
  mutate(subj_id=PROLIFIC_PID,
         RT = as.numeric(RT),
         target_time = as.numeric(target_time),
         correct = correct=='true',
         resp=response==tolower(stripes_key),
         test_part=ifelse(test_part=='WSBI','WBSI',test_part)) %>%
  filter(PROLIFIC_PID!='PROLIFIC_PID');

E7.raw_disc_df <- E7.raw_df %>%
  filter(test_part=='neutral' | test_part=='stripes_loss' | test_part=='noise_loss')


excludeSubjects <- function(raw_df, test_parts, to_exclude_technical, to_exclude_comprehension) {
  
  to_exclude_RT <- raw_df %>%
    filter(test_part %in% test_parts) %>%
    group_by(subj_id) %>%
    summarise(
      first_quartile_RT = quantile(RT,0.25),
      third_quartile_RT = quantile(RT,0.75)
    ) %>%
    filter(first_quartile_RT<100 | third_quartile_RT>5000) %>%
    pull(subj_id)
  
  # to_exclude_accuracy <- raw_df %>%
  # filter(test_part %in% test_parts) %>%
  # group_by(subj_id,test_part) %>%
  # summarise(
  #   accuracy = mean(correct)
  # ) %>%
  #   filter(accuracy<0.5) %>%
  # pull(subj_id) %>%
  # unique()
  
  to_exclude_accuracy <- raw_df %>%
    filter(test_part %in% test_parts) %>%
    group_by(subj_id,test_part) %>%
    summarise(
      accuracy = mean(correct)
    ) %>%
    group_by(subj_id) %>%
    summarise(min_acc=min(accuracy))%>%
    filter(min_acc<=0.5) %>%
    pull(subj_id) %>%
    unique()
  
  filtered_df <- raw_df %>%
    filter(!(subj_id %in% to_exclude_accuracy | 
               subj_id %in% to_exclude_RT | 
               subj_id %in% to_exclude_technical | 
               subj_id %in% to_exclude_comprehension));
  
  return(filtered_df)
}


E7.to_exclude_comprehension <- E7.raw_df %>%
  group_by(subj_id) %>%
  summarise(
    neutral_instruction_repetitions = mean(as.integer(neutral_instruction_repetitions)),
    stripes_loss_instruction_repetitions = mean(as.integer(stripes_loss_instruction_repetitions)),
    noise_loss_instruction_repetitions = mean(as.integer(noise_loss_instruction_repetitions)),
    max_instruction_repetitions = max(neutral_instruction_repetitions,stripes_loss_instruction_repetitions,noise_loss_instruction_repetitions)
  ) %>%
  filter(max_instruction_repetitions>3) %>%
  pull(subj_id) %>%
  unique()

# 613f1441f35ae1a22196e144 reported not seeing the stimuli in some blocks
E7.to_exclude_technical = c('613f1441f35ae1a22196e144');

E7.df <- excludeSubjects(E7.raw_df,c('neutral','noise_loss','stripes_loss'),E7.to_exclude_technical,E7.to_exclude_comprehension)


E7.comprehension_df <- E7.raw_df %>%
  filter(trial_type=='survey-multi-choice')%>%
  dplyr::select(subj_id,response) %>%
  filter(grepl('multichoice_detection',response, fixed=T))%>%
  mutate(json=map(response,fromJSON)) %>%  
  unnest_wider(col=json, names_repair='unique') %>%
  dplyr::select(subj_id,multichoice_detection,noise_loss,stripes_loss,neutral) %>%
  mutate(test_part=ifelse(is.na(noise_loss) & is.na(stripes_loss) & is.na(neutral),
                          'practice',
                          ifelse(is.na(noise_loss) & is.na(stripes_loss),
                                 'neutral',
                                 ifelse(is.na(noise_loss),
                                        'stripes_loss','noise_loss'))),
         Q1correct = grepl('when the target',multichoice_detection,fixed=T),
         Q2resp = ifelse(test_part=='neutral',
                         neutral,
                         ifelse(test_part=='noise_loss',
                                noise_loss,
                                ifelse(test_part=='stripes_loss',stripes_loss,NA))),
         Q2 = ifelse(grepl('regardless', Q2resp,fixed=T),
                     'regardless',
                     ifelse(grepl('I press', Q2resp,fixed=T),
                            'press',
                            ifelse(grepl('or the', Q2resp,fixed=T),
                                   'or',
                                   'never'))),
         Q2correct = (test_part=='neutral' & Q2=='never') |
           (test_part=='noise_loss' & Q2 == 'regardless') |
           (test_part=='stripes_loss' & Q2=='regardless') |
           test_part=='practice',
         correct = Q1correct & Q2correct) %>%
  dplyr::select(subj_id,test_part,Q1correct,Q2,Q2correct, correct) %>%
  group_by(subj_id,test_part) %>%
  mutate(repetition = seq_along(Q2))

E7.comprehension_df %>%
  filter(test_part %in% c('stripes_loss','noise_loss','neutral') & repetition==1) %>%
  group_by(test_part,Q2)%>%
  tally()

E7.comprehension_df %>%
  filter(!Q2correct) %>%
  group_by(test_part,Q2)%>%
  tally()

getBias <- function(df,test_parts) {
  
  bias <- df %>%
    filter(test_part %in% test_parts) %>%
    group_by(subj_id,test_part) %>%
    summarise(
      bias = mean(resp)
    ) %>%
    spread(test_part,bias) 
  
  return(bias)
}

E7.bias <- E7.df  %>%
  getBias(c('neutral','noise_loss','stripes_loss')) %>%
  mutate(noise_loss_effect = noise_loss-neutral,
         stripes_loss_effect = stripes_loss-neutral,
         interaction = noise_loss_effect+stripes_loss_effect)
