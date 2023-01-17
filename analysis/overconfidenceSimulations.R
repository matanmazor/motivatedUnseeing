
library(ggplot2)
library(dplyr)
library(tidyverse)


valueFunction <- function(points) {
  if (points==0) {
    return(0)
  } else if (points >0) {
    return(points**(0.83))
  } else if (points<0) {
    return(-1.4*(abs(points)**(0.83)))
  }
}

probWeight <- function(p) {
  return(exp(-(-log(p))**0.5))
}

plotRational <- function(accuracy,target) {
# plotting the expected gain for the two responses.
  #arguments: accuracy: points offered for correct responses
  # target: points awarded or deducted when the target stimulus is presented

  ptarget = seq(0,1,0.01)
  # this is the sensory evidence for target/nontarget, on a scale of 0-1

  expected_reward_target = ptarget*(accuracy+target)+(1-ptarget)*0;
  expected_reward_nontarget= ptarget*target+(1-ptarget)*accuracy;

  df <- data.frame(ptarget,
                   expected_reward_target,
                   expected_reward_nontarget) %>%
    pivot_longer(cols=expected_reward_target:expected_reward_nontarget,
                 names_to='response') %>%
    mutate(response=ifelse(response=='expected_reward_target','target','nontarget'))

  ggplot(df, aes(x=ptarget,y=value,color=response))+
    geom_line(size=1) +
    labs(x='p(target)', y='expected value')+
    theme_classic() +
    scale_y_continuous(limits=c(-12,24))

    ggsave(paste('figures/modelling/rational_',accuracy,'_',target,'.png',sep=''),
           width=6,height=4)
}

overconfidence_factor <- 1.2;

plotOverconfident <- function(accuracy,target,overconfidence_factor) {
  # plotting the expected gain for the two responses.
  #arguments: accuracy: points offered for correct responses
  # target: points awarded or deducted when the target stimulus is presented
  # pcorrect: prior probability that I am correct

  ptarget = seq(0,1,0.01)
  # this is the sensory evidence for target/nontarget, on a scale of 0-1

  expected_reward_target = overconfidence_factor*ptarget*(accuracy+target)+(1-ptarget)*0;
  expected_reward_nontarget= ptarget*target+overconfidence_factor*(1-ptarget)*accuracy;

  df <- data.frame(ptarget,
                   expected_reward_target,
                   expected_reward_nontarget) %>%
    pivot_longer(cols=expected_reward_target:expected_reward_nontarget,
                 names_to='response') %>%
    mutate(response=ifelse(response=='expected_reward_target','target','nontarget'))

  ggplot(df, aes(x=ptarget,y=value,color=response))+
    geom_line(size=1) +
    labs(x='p(target)', y='expected value')+
    theme_classic() +
    scale_y_continuous(limits=c(-12,24))

  ggsave(paste('figures/modelling/overconfident_',accuracy,'_',target,'_',overconfidence_factor,'.png',sep=''),
         width=6,height=4)
}
