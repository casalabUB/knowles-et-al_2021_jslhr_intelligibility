# R script for statistical models for Knowles et al., 2021
# https://doi.org/10.1044/2021_JSLHR-20-00593
# NOTE: the output of this script and of the 1_tidy_vas.R script sourced below are available in radi_intell_vas.RData
# This script is available for transparency, but will not be able to be run

# RADI intell models revised for intell manuscript

# Load libraries ----
library(plyr)
library(tidyverse)
#library("tidylog", warn.conflicts = FALSE)
library(cowplot)
library(lme4)
library(lmerTest)
library(emmeans)
library(rms)
library(psych) # for ICC
library(ggplot2)
library(thear)
library(ggridges)

# Load original data ----
source("../scripts/1_tidy_vas.R")

# Sentence duration ----
sits_dur_orig <- sits_dur
df_sits_dur <- sits_dur_orig %>%
  radi_filename_prep() %>%
     # get rid of headers
     dplyr::filter(!is.na(participant)) %>% 
  filter(participant != "512") %>% # remove 512 because his recordings were bad
     mutate(participant = revalue(participant, replace = c( # Fix 509, 303
          "5092" = "509",
          "3032" = "303")),
          id = paste(condition,participant,item,sep="_")) %>%
  
  
  # NOTE: ITEM - NWORDS CODING WAS BAD UNTIL 9/23
  # CORRECT CODING IS: 
  #   (list 1) 53 = 5, 54 = 7, 55 = 9, 
  #   (list 2) 56 = 6, 57 = 8, 58 = 10
  # Previously it was 53 = 5, 54 = 6, 55 = 7, 56 = 8, 57 = 9, 58 = 10
  mutate(nwords = item,
         nwords = recode(nwords, 
                               "53" = "5",
                               "54" = "7",
                               "55" = "9",
                               "56" = "6",
                               "57" = "8",
                               "58" = "10"),
         nwords = as.numeric(as.character(nwords)),
         wpm = nwords/(utterance_duration/60),
         rate_ordered = factor(rate, levels = c("S4","S3","S2","H1","F2","F3","F4")))

df_sits_habit_dur_p <- df_sits_dur %>%
  filter(rate=="H1") %>%
  group_by(participant, group, rate) %>%
  summarize(wpm_habit = mean(wpm, na.rm = TRUE))

df_sits_habit <- Rmisc::summarySE(data = df_sits_habit_dur_p, 
                                  measurevar = "wpm_habit",
                                  groupvars = c("group"))


# VAS ----
df_vas <- vas %>%
  mutate(rate_ordered = factor(rate, levels = c("S4", "S3", "S2", "H1", "F2", "F3", "F4")),
         vas_prop = percentage/100,
         # item ordered 5,6,7,8,9,10 words,monologue
         item = factor(item, levels = c("53","56","54","57","55","58","63")), 
         complexity = item,
         nwords = recode(item, 
                               "53" = "5",
                               "56" = "6",
                               "54" = "7",
                               "57" = "8",
                               "55" = "9",
                               "58" = "10",
                               "63" = "mon"),
        nwords = as.numeric(as.character(nwords)), # 5 - 10; monologue = NA
        task = if_else(is.na(nwords),"Spontaneous","Sentence"), 
        #task = if_else(item=="63","Spontaneous","Sentences"),
         task = factor(task)) %>%
     filter(!is.na(group)) # nothing filtered; all ok

# Remove participants 208 and 512 who have 0 SITs
# No need to do this because they do have spontaneous clips
# df_vas <- df_vas %>%
#   filter(participant!="208",
#          participant!="512") # 84 obs removed; 42 for each participant

# __Contrasts ----
# Goal in naming: name on the left (e.g., S4 in S4vH1) is associated with the POSITIVE contrast value; name on the right is NEGATIVE
# NOTE: same as in dissertation but on uncollapsed data
levels(df_vas$group)
# "YC"  "OC"  "PD"  "DBS"
contrasts(df_vas$group) <- matrix(c(3/4, -1/4, -1/4, -1/4,
                                   0, 2/3, -1/3, -1/3,
                                   0, 0, 1/2, -1/2),
                                 ncol = 3)
colnames(contrasts(df_vas$group)) <- c("YC vs rest", "OC vs rest", "PD vs DBS")
solve(cbind(1,contrasts(df_vas$group)))


# Sits dur
levels(df_sits_dur$group)
# "YC"  "OC"  "PD"  "DBS"
contrasts(df_sits_dur$group) <- matrix(c(3/4, -1/4, -1/4, -1/4,
                                   0, 2/3, -1/3, -1/3,
                                   0, 0, 1/2, -1/2),
                                 ncol = 3)
colnames(contrasts(df_sits_dur$group)) <- c("YC vs rest", "OC vs rest", "PD vs DBS")
solve(cbind(1,contrasts(df_sits_dur$group)))

# VAS: treatment contrasts. SITS DUR: Difference contrasts
# Refactor rate so that level 1 is habitual, then code as treatment so every level is compared to habitual
# First make original rate column so that it's properly ordered slow to fast
# df_vas <- df_vas %>%
#         mutate(rate = str_to_upper(rate))
df_vas$rate_ordered <- df_vas$rate
df_vas$rate <- factor(df_vas$rate, 
                     levels =c("H1", "S4", "S3", "S2", "F2", "F3", "F4"))
contrasts(df_vas$rate) <- contr.treatment(7)
solve(cbind(1,contrasts(df_vas$rate)))
# Label contrasts so the term on the left is the positively coded term
colnames(contrasts(df_vas$rate)) <- c("S4vH1","S3vH1","S2vH1","F2vH1","F3vH1","F4vH1")


# SIT DUR: rate = successive differences contrast
# From MASS::contr.sdif(); n = 7
# Explanation: https://bbolker.github.io/stat4c03/notes/contrasts.pdf
# From manual: The contrast coefficients are chosen so that the coded coefficients in a one-way layout are the differences between the means of the second and first levels, the third and second levels, and so on. This makes most sense for ordered factors, but does not assume that the levels are equally spaced.
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/#forward
dif_contrast_7 <- MASS::contr.sdif(7)
contrasts(df_sits_dur$rate_ordered) <- dif_contrast_7
solve(cbind(1,contrasts(df_sits_dur$rate_ordered)))
colnames(contrasts(df_sits_dur$rate_ordered)) <- c("S3-S4","S2-S3","H1-S2",
                                                   "F2-H1","F3-F2","F4-F3")


# Task: sum coding
contrasts(df_vas$task) <- contr.sum(2)
solve(cbind(1,contrasts(df_vas$task)))
colnames(contrasts(df_vas$task)) <- c("SentvSpont")

# Item: helmert if used as fixed eff

# Reverse helmert contrasts (so left side of contrast is positive)
contrasts(df_vas$complexity) <- contr.helmert(7)*-1
solve(cbind(1,contrasts(df_vas$complexity)))
#colnames(contrasts(df_vas$complexity)) <- c("53v54","53-54v55","53-55v56","53-56v57","53-57v58", "53-58v63")
colnames(contrasts(df_vas$complexity)) <- c("5v6","5-6v7","5-7v8","5-8v9","5-9v10", "Task")


# Item: make item_new so that each item across conditions is unique
df_vas$item_new <- factor(paste0(df_vas$item,"_",df_vas$rate))
df_sits_dur$item_new <- factor(paste0(df_sits_dur$item,"_",df_sits_dur$rate))
# 53 - 58: sentences increase in length; 63: conversation
levels(df_vas$item_new)




# Rescale item, participant
# NB: rescaling actually makes 0 difference in model fit or coefficients, so no need to do this anymore
df_vas <- df_vas %>%
        transform(participant_rescaled = arm::rescale(participant),
                  item_new_rescaled = arm::rescale(item_new))

df_sits_dur <- df_sits_dur %>%
        transform(participant_rescaled = arm::rescale(participant),
                  item_new_rescaled = arm::rescale(item_new))


# Slopes
contrasts(df_vas$group)
model.matrix(~group, df_vas) %>% head()
df_vas$YCvRest <- model.matrix(~group, df_vas)[,2]
df_vas$OCvRest <- model.matrix(~group, df_vas)[,3]
df_vas$PDvDBS <-  model.matrix(~group, df_vas)[,4]

model.matrix(~rate, df_vas) %>% head()
df_vas$S4vH1 <- model.matrix(~rate, df_vas)[,2]
df_vas$S3vH1 <- model.matrix(~rate, df_vas)[,3]
df_vas$S2vH1 <- model.matrix(~rate, df_vas)[,4]
df_vas$F2vH1 <- model.matrix(~rate, df_vas)[,5]
df_vas$F3vH1 <- model.matrix(~rate, df_vas)[,6]
df_vas$F4vH1 <- model.matrix(~rate, df_vas)[,7]




# Model ----
# Model vas proportion (logit-transformed) as a function of group, rate, and task and all possible interactions, plus listener
# 2020-09-15: Changed model from COMPLEXITY (which included sentence length) to TASK (which collapses over sentence length)
m_vas <- lmerTest::lmer(
     logit(vas_prop) ~ # now using psych::logit instead of car::logit to more easily back transform
       group*rate*task+
          listener +
          (1 + (S4vH1 + S3vH1 + S2vH1 + F2vH1 + F3vH1 + F4vH1) || participant),
     data = df_vas,
     REML = FALSE
)
beepr::beep()
anova(m_vas)
summary(m_vas)

# This is what the original model was
m_complexity <- lmerTest::lmer(
     logit(vas_prop) ~
       group*rate*complexity+
          listener +
          (1 + (S4vH1 + S3vH1 + S2vH1 + F2vH1 + F3vH1 + F4vH1) || participant),
     data = df_vas,
     REML = FALSE
)

m_sits <- lmerTest::lmer(
     logit(vas_prop) ~
       group*rate*nwords+
          listener +
          (1 + (S4vH1 + S3vH1 + S2vH1 + F2vH1 + F3vH1 + F4vH1) || participant),
     data = subset(df_vas,task=="Sentence"),
     REML = FALSE
)
beepr::beep()
summary(m_vas)
summary(m_sits)


m_sits_dur <- lmerTest::lmer(
  log(wpm) ~ #log transformed b/c of resid plots
    group*rate_ordered + 
    (1|participant),
  data=df_sits_dur,
  REML = FALSE
)
summary(m_sits_dur)

# Visualize ----
plot(m_vas)
plot(resid(m_vas),car::logit(df_vas$vas_prop))
plot(m_vas) # looks ok
qqnorm(resid(m_vas)) 
qqline(resid(m_vas))

hist(df_vas$vas_prop)
hist(logit(df_vas$vas_prop))
hist(car::logit(df_vas$vas_prop)) # identical

# WPM ~ rate condition
plot(m_sits_dur)
qqnorm(resid(m_sits_dur))
qqline(resid(m_sits_dur)) 



# Coefs ----
vasB <- summary(m_vas)$coefficients[,1] %>% round(3)
vasP <- summary(m_vas)$coefficients[,5] %>% round(3)
vas_coef <- summary(m_vas)$coefficients %>% as.data.frame() %>%
        rownames_to_column(var="Contrast")

sits_durB <- summary(m_sits_dur)$coefficients[,1] %>% round(3)
sits_durP <- summary(m_sits_dur)$coefficients[,5] %>% round(3)
sits_dur_coef <- summary(m_sits_dur)$coefficients %>% as.data.frame() %>%
  rownames_to_column(var="Contrast")

# Pairwise ----
# backtransformed from logit
vas_rgc_emm <- emmeans(m_vas, pairwise ~
                          rate | group | task, 
                       transform = "response")
vas_rgt_emm <- emmeans::emmeans(m_complexity, pairwise ~ 
                         rate | group | complexity,
                       transform = "response")
vas_rg_emm <-  emmeans(m_vas, pairwise ~
                          rate | group, 
                       transform = "response")
vas_c_emm <- emmeans(m_complexity, pairwise ~
                          complexity, 
                       transform = "response")
vas_gc_emm <- emmeans(m_vas, pairwise ~ 
                        task | group, 
                       transform = "response")

sits_dur_gr_emm <- emmeans(m_sits_dur, pairwise ~
                              rate_ordered | group,
                           transform = "response")

sits_dur_rg_emm <- emmeans(m_sits_dur, pairwise ~
                              group | rate_ordered,
                           transform = "response")


thear::plot_emm_contrasts(vas_rg_emm)+
     facet_wrap(~group)+
     coord_flip()+
     theme(legend.position = "bottom")

# WPM
emmeans::emmip(m_sits_dur, group ~ rate_ordered, CIs = TRUE, type = "response")+
    scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
  geom_vline(xintercept="H1",linetype="dashed",color="darkgrey")+
  theme_bw()+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))

# Response scale (proportion)
emmeans::emmip(m_vas, group ~ rate, CIs = TRUE, type = "response")+
  scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
  geom_vline(xintercept="H1",linetype="dashed",color="darkgrey")+
  theme_bw()+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))

# Logit scale (log-odds)
# Same pattern, but easier to see trends on log-odds
emmeans::emmip(m_vas, group ~ rate, CIs = TRUE)+  
  scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
  geom_vline(xintercept="H1",linetype="dashed",color="darkgrey")+
  theme_bw()+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))

emmip(m_vas, group~complexity, CIs = TRUE)+
  theme_bw()+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))


emmip(m_vas, ~group)
emmip(m_vas, ~complexity)

#save.image("../manuscripts/radi_intell_manuscript/radi_intell_vas.RData")
