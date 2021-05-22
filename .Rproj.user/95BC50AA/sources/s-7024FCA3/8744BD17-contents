# R code contained in original .Rmd manuscript file

## ----setup, include=FALSE-------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
#knitr::opts_knit$set(root.dir = "../../manuscripts/radi_voice_acoustics_manuscript/")
options(scipen=999)
# Paths refer to original directory structure
# root_dir = "../../manuscripts/radi_intell_manuscript/"
# data_dir = "../data/"
# knitr::opts_knit$set(root.dir = "../../manuscripts/radi_intell_manuscript/")


## -------------------------------------------------
#load("../manuscripts/radi_intell_manuscript/radi_intell_vas.RData")
load("radi_intell_vas.RData")

library(LaCroixColoR)
library(thear)
library(plyr)
library(tidyverse)
library(captioner)
library(flextable)
library(ggridges)
library(officer)

figs <- captioner(prefix="Figure")
tbls <- captioner(prefix="Table")
filter <- dplyr::filter


## ----relabel-groups-------------------------------
group_revised = c("PD" = "PD-Med","DBS" = "PD-DBS", "YC" = "YC", "OC" = "OC")


## ----demographics-pd-table------------------------

library(readxl)
message("loading demographics...")

# For output table
# NEVERMIND you can't output tables to .docx which is what AJSLP and JSLHR always neeeeed
#current_df <- read_excel(paste0(data_dir,"radi_participant_demographics_final.xlsx"), na="NA") %>% tidyr::replace_na(list('Mean intelligibility' = "NA", 'PD medications' = "NA"))

# For table in text
current_df <- readxl::read_excel("../../data/radi_participant_demographics_final.xlsx", na = "NA") %>% tidyr::replace_na(list('Mean intelligibility' = "NA", 'PD medications' = "NA")) 
  

current_caption <- "Demographic data for Parkinson's disease Levodopa group."
#current_df <- df %>%
#  mutate_if(is.character, str_replace_all, "[()]","")

tab_pd_ft <- current_df %>%
    filter(Group=="PD") %>%
    select(-Group, -'Years since DBS surgery',-'Mean intelligibility') %>%
  flextable() %>%
  #set_caption(current_caption) %>%
    #merge_at(i = 1:22, j = 1) %>%
    #merge_at(i = 23:34, j = 1) %>%
    #merge_at(i = 35:51, j = 1) %>%
    # Background alternating very light grey/white for groups
    #bg(bg = "#F1F1F1", part = "body", i = 1:22) %>%
    #bg(bg = "#F1F1F1", part = "body", i = 35:51) %>%
    # No digits for age and moca columns
    colformat_num(digits=0, col_keys = c("Age","MoCA")) %>%
    # Make font smaller
    fontsize(part = "header", size = 9) %>%
    fontsize(part = "body", size = 9) %>%
    #colformat_num(digits = 1, col_keys ="Mean intelligibility") %>%
    #autofit() %>%
    #width(j = "Deviant perceptual characteristics", width = 2) %>%
    add_footer_lines("Note: PD = Parkinson's disease; MoCA = Montreal Cognitive Assessment. Two PD participants (PD06, PD14) were unsure of their current medication list, which are listed here as NA. Deviant perceptual characteristics for the PD and DBS groups correspond to features noted during the habitual monologue speech samples. Mean intelligibility corresponds to the mean intelligibility ratings for each participant during sentence production in the habitual rate condition (i.e., this was not a standardized intelligibility assessment).") #%>%
    #height(part = "header", height = 0.5) %>%
    #autofit()
    #dim_pretty(part = "all")


## -------------------------------------------------
tab_pd_ft


## ----demographics-dbs-table-----------------------

message("loading demographics...")

current_df <- readxl::read_excel("../../data/radi_participant_demographics_final.xlsx", na = "NA") %>% tidyr::replace_na(list('Mean intelligibility' = "NA"))

current_caption <- "Demographic data for Parkinson's disease STN-DBS group."
#current_df <- df %>%
#  mutate_if(is.character, str_replace_all, "[()]","")

tab_dbs_ft <- current_df %>%
    filter(Group=="DBS") %>%
    select(-Group,-'Mean intelligibility') %>%
  flextable() %>%
  #set_caption(current_caption) %>%
    #merge_at(i = 1:22, j = 1) %>%
    #merge_at(i = 23:34, j = 1) %>%
    #merge_at(i = 35:51, j = 1) %>%
    # Background alternating very light grey/white for groups
    #bg(bg = "#F1F1F1", part = "body", i = 1:22) %>%
    #bg(bg = "#F1F1F1", part = "body", i = 35:51) %>%
    # No digits for age and moca columns
    colformat_num(digits=0, col_keys = c("Age","MoCA")) %>%
    # Make font smaller
    fontsize(part = "header", size = 9) %>%
    fontsize(part = "body", size = 9) %>%
    #colformat_num(digits = 1, col_keys ="Mean intelligibility") %>%
    add_footer_lines("Note: PD = Parkinson's disease; DBS = Deep brain stimulation; MoCA = Montreal Cognitive Assessment. Deviant perceptual characteristics for the PD and DBS groups correspond to features noted during the habitual monologue speech samples. Mean intelligibility corresponds to the mean intelligibility ratings for each participant during sentence production in the habitual rate condition (i.e., this was not a standardized intelligibility assessment).") #%>%
    #dim_pretty(part = "all")
    #height(part = "header", height = 0.5) %>%
    #autofit()
    #


## -------------------------------------------------
tab_dbs_ft


## ----vas-coefs, include = FALSE-------------------
library(flextable)
vas_coef_names <- c("Contrast","Estimate","StdError","df","t.value","p.value")
vas_tmp <- vas_coef %>%
  mutate(across(where(is.numeric),round, digits = 3))
names(vas_tmp) <- vas_coef_names
vas_ft <- vas_tmp %>%
  mutate(p.value = round(p.value,3)) %>%
  flextable() %>%
  #color(color = "#B20000", i = ~ p.value <= 0.2) %>%
  bold(i = ~ p.value < 0.05) %>%
  color(color = "#B20000", i = ~ p.value < 0.05 & Estimate < 0) %>%
  color(color = "blue", i = ~ p.value < 0.05 & Estimate > 0)
  #set_formatter(f = function(x) round(x, 2))
vas_ft


## ----plot-wmp-------------------------------------

df_sits_habit <- df_sits_habit %>%
  mutate(group = revalue(group, group_revised))


tmp1 <- df_sits_dur %>%
    mutate(group = revalue(group, group_revised)) %>%
     ggplot(aes(x=wpm, y=rate_ordered, fill=group, alpha = rate_ordered, 
                height = ..density..)) + 
     geom_density_ridges(stat="density")+
     scale_fill_manual(values = lacroix_palette("PeachPear", n=4),
                        name="Group")+
     scale_color_manual(values = lacroix_palette("PeachPear", n=4),
                        name="Group")+
  scale_alpha_manual(values = c(0.5,0.5,0.5,1,0.5,0.5,0.5))+
    geom_vline(data = df_sits_habit, aes(xintercept = wpm_habit,color=group), linetype="dashed")+
  facet_wrap(~group)+
     labs(x="Speech rate (WPM) \nSlow to fast",
          y="Rate condition (Density) \nSlow to fast")+
        guides(fill=FALSE, color=FALSE,alpha = FALSE)+
  theme_bw()

tmp2 <- emmeans::emmip(m_sits_dur, group ~ rate_ordered, CIs = TRUE, type = "response")+
    scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
  geom_vline(xintercept="H1",linetype="dashed",color="darkgrey")+
  theme_bw()+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"),
                     name="Group",
                     labels = group_revised)+
                     #labels = c("YC","OC","PD-Med","PD-DBS"))+
  labs(x="Speech rate condition \nSlow to fast",
       y = "Speech rate (WPM) \nSlow to fast")+
  theme(legend.position = "bottom")

cowplot::plot_grid(tmp1,tmp2,rel_widths = c(1.5,1),labels = "AUTO")

#tjmisc::ggpreview(width=10, units="in",device="pdf")
#ggsave("../manuscripts/radi_intell_manuscript/figs/fig1_rate.pdf", width=10, units = "in", dpi=300)


## ----tab-habit------------------------------------
df_sits_habit %>% 
  mutate(group = revalue(group, group_revised)) %>%
  mutate(across(where(is.numeric),round, digits = 0)) %>%
  flextable()


## ----tab-habit-wpm--------------------------------
sits_dur_rg_emm$contrasts %>% as.data.frame() %>%
  mutate(contrast = str_replace_all(contrast, group_revised)) %>%
  #mutate(group = revalue(group, group_revised)) %>%
  filter(rate_ordered=="H1") %>%
  select(-rate_ordered) %>%
  mutate(across(where(is.numeric),round,3)) %>%
  flextable() %>% autofit()


## ----round-reliability----------------------------
sits_inter <- sits_inter %>%
  mutate(across(where(is.numeric),round, digits = 3)) %>% 
  mutate(Task = "Sentence")
conv_inter <- conv_inter %>%
  mutate(across(where(is.numeric),round, digits = 3)) %>%
  mutate(Task = "Monologue")



## ----tab-sits-intra-------------------------------
current_caption <- "Intra-rater reliability for each listener: Sentence rating task"
current_df <- sits_intra %>% mutate(across(where(is.numeric),round, digits = 3)) %>%
  select(-df1, -df2)

if(knitr::is_latex_output()){
     current_df %>% 
     kableExtra::kable("latex",
                       booktabs=T,
                       #longtable = TRUE,
                       caption = current_caption) %>%
     kableExtra::kable_styling(full_width = T)
}else{
    current_df %>% 
    select(-type) %>%
    flextable() %>%
    #set_caption(current_caption) %>%
    set_header_labels(listener = "Listener") %>%
    theme_booktabs() %>%
    add_footer_lines("Note: ICC = Intraclass correlation coefficient (ICC 3,k).") %>%
    fontsize(size = 10, part = "head") %>%
    fontsize(size = 10, part = "body") %>%
    set_formatter(f = function(x) round(x, 2))
}


## ----tab-conv-intra-------------------------------

current_caption <- "Intra-rater reliability for each listener: Monologue rating task"
current_df <- conv_intra %>% mutate(across(where(is.numeric),round, digits = 3)) %>%
  select(-df1, -df2)

if(knitr::is_latex_output()) {
  current_df %>%
    kableExtra::kable("latex",
                      booktabs = T,
                      #longtable = TRUE,
                      caption = current_caption) %>%
    kable_styling(full_width = T)
} else{
  current_df %>%
    select(-type) %>%
    flextable() %>%
    #set_caption(current_caption)%>%
    set_header_labels(listener = "Listener") %>%
    theme_booktabs() %>%
    add_footer_lines("Note: ICC = Intraclass correlation coefficient (ICC 3,k).") %>%
    fontsize(size = 10, part = "head") %>%
    fontsize(size = 10, part = "body")
}


## -------------------------------------------------
aov_vas <- anova(m_vas, m_vas_problematic)
# See .R script for revised problematic participants
#problematic_participants %>% unique() %>% length() # 514 wasn't included

# problematic_participants <- c(
#   # Dentures
#   "203","206","211","310","314","320","322","506",
#   # L2 English
#   "301", "310", "320", "316",
#   # childhood stuttering
#   "301", "303",
#   # TIA
#   "301", "514",
#   # Distorted speech controls
#   "206", "210")


## ----plot-rg-emmip--------------------------------
# on response scale (not logit)
plot_rg_emmip <- emmeans::emmip(m_vas, group ~ rate, CIs = TRUE,type = "response")+
  scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
  geom_vline(xintercept="H1",linetype="dashed",color="darkgrey")+
  theme_bw()+
  #theme(legend.position="bottom")+
  theme(legend.position=c(0.2,0.2))+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"),
                     name = "Group",
                     labels = group_revised)+
  ylab("Predicted response (intelligibility) \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")



## ----plot-rg-participants-------------------------
plot_rg <- df_vas %>%
  mutate(group = revalue(group, group_revised)) %>%
  group_by(participant, rate_ordered, group, task, item) %>%
  summarize(vas_prop = mean(vas_prop)) %>%
     ggplot(aes(x = rate_ordered, y = vas_prop, 
                color = group, group = participant)) + 
     geom_jitter(alpha=0.075) +
     stat_smooth(method = 'loess',
                 geom = 'line', alpha=0.5) +
  geom_smooth(method = "loess", aes(group=group),
              color = "black", alpha = 1,
              size = 1)+
  geom_vline(xintercept = "H1", linetype = "dashed", color = "darkgrey")+
     facet_wrap(~group)+
     theme(legend.position = "blanks")+
     scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"),
                        labels = group_revised)+
guides(color=FALSE)+
  theme_bw()+
  coord_cartesian(ylim = c(0,1))+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")

# Note: DBS504 is the really low one who doesn't have the fast conditions (WG)


## ----plot-rg--------------------------------------
library(cowplot)
cowplot::plot_grid(plot_rg_emmip, plot_rg,labels = "AUTO")
#tjmisc::ggpreview(width=10, units="in",device="pdf")
#ggsave("../manuscripts/radi_intell_manuscript/figs/fig2_rg.pdf", width=10, units = "in", dpi=300)


## ----peek-coefs-task, include = FALSE, eval = FALSE----
## vas_tmp %>%
##   filter(str_detect(Contrast, "task")) %>%
##   filter(str_detect(Contrast, "group")) %>%
##   mutate(p.value = round(p.value,3)) %>%
##   flextable() %>%
##   #color(color = "#B20000", i = ~ p.value <= 0.2) %>%
##   bold(i = ~ p.value < 0.05) %>%
##   color(color = "#B20000", i = ~ p.value < 0.05 & Estimate < 0) %>%
##   color(color = "blue", i = ~ p.value < 0.05 & Estimate > 0) %>%
##   color(color = "purple", i = ~p.value < 0.1 & p.value > 0.05) %>%
##   autofit()
##   #set_formatter(f = function(x) round(x, 2))


## ----peek-emmip-task, eval = FALSE----------------
## # UPDATE 2020-09-15: task is now included in the final model. m_task no longer exists so this code chunk will crash
## # Use this just to visualize task effects. This is not the final model
## 
## # group x task
## emmeans::emmip(m_task, group~task, CIs = TRUE)
## 
## # group x rate x task
## emmeans::emmip(m_task, group~task~rate, CIs = TRUE)+
##   scale_linetype_manual(values = c("dashed","dashed","dashed","dashed",
##                                    "solid","solid","solid","solid"))+ #not working
##   theme_bw()+
##   scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
##   #facet_wrap(~group)
##   facet_wrap(~task)
## 
## emmeans::emmip(m_task, rate~group~task, CIs = TRUE)+
##   facet_wrap(~rate)+
##   guides(color = FALSE)
## 
## 


## ----plot-rgt-means, include = FALSE--------------
# Empirical data
# Averaged over participants
# Confidence interval = 90%
df_vas_p <- df_vas %>%
  group_by(participant, group, task, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop))

df_vas_means <- Rmisc::summarySE(df_vas_p, 
                                 measurevar = "vas_prop", 
                                 groupvars = c("task","rate_ordered","group"),
                                 conf.interval = 0.9) %>%
  mutate(lower = vas_prop - ci,
         upper = vas_prop + ci)

df_vas_means %>%
  ggplot(aes(x = rate_ordered, y = vas_prop,
    group = group,
    color = group)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,color="lightgrey")+
  geom_line(size = 1) + 
  facet_wrap(~task)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"),
                     name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")


## ----plot-rgt-loess, include = TRUE---------------
# Empirical data
# Averaged over participants
# Confidence interval = 90%
df_vas %>%
  mutate(group = revalue(group, group_revised),
         task = revalue(task, c("Sentence" = "Sentence",
                                "Spontaneous" = "Monologue"))) %>%
  group_by(participant,rate_ordered, group, task) %>%
  summarise(vas_prop = mean(vas_prop)) %>%
  group_by(rate_ordered,group,task) %>%
  ggplot(aes(x=rate_ordered, y = vas_prop, color = group, group = group))+
  #geom_jitter(stat='identity',alpha=0.075)+
  geom_smooth(method = "loess", level=0.90)+ # 90% confidence interval
  facet_wrap(~task)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"),
                     name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")

#tjmisc::ggpreview(width=10, units="in",device="pdf")
#ggsave("../manuscripts/radi_intell_manuscript/figs/fig3_rgt.pdf", width=10, units = "in", dpi=300)


## ----fig-task-difs, include = FALSE---------------
tmp1 <- df_vas %>%
  group_by(participant, group, task, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop, na.rm=TRUE)) %>%
  group_by(group, task, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop,na.rm=TRUE)) %>%
  filter(rate_ordered %in% c("H1", "F2")) %>%
  ungroup()%>%
  mutate(id = paste(rate_ordered, task ,sep="_")) %>%
  select(group,id,vas_prop) %>%
  spread(id, vas_prop) %>%
  mutate(sentence_dif = H1_Sentence - F2_Sentence,
         spont_dif = H1_Spontaneous - F2_Spontaneous,
         task_dif = sentence_dif - spont_dif,
         rate = "F2") %>%
  select(rate, group,sentence_dif, spont_dif, task_dif)

tmp2 <- df_vas %>%
  group_by(participant, group, task, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop,na.rm=TRUE)) %>%
  group_by(group, task, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop,na.rm=TRUE)) %>%
  filter(rate_ordered %in% c("H1", "S2")) %>%
  ungroup()%>%
  mutate(id = paste(rate_ordered, task ,sep="_")) %>%
  select(group,id,vas_prop) %>%
  spread(id, vas_prop) %>%
  mutate(sentence_dif = H1_Sentence - S2_Sentence,
         spont_dif = H1_Spontaneous - S2_Spontaneous,
         task_dif = sentence_dif - spont_dif,
         rate = "S2") %>%
  select(rate, group,sentence_dif, spont_dif, task_dif)

rbind(tmp1, tmp2) %>%
  ggplot(aes(x=group, y=task_dif,fill=group))+geom_bar(stat="identity")+facet_wrap(~rate)

tmp2 %>% gather("task","dif",3:4) %>%
  ggplot(aes(x=group,y=dif,fill=group))+geom_bar(stat="identity")+facet_wrap(~task)

tmp1 %>% gather("task","dif",3:4) %>%
  ggplot(aes(x=group,y=dif,fill=group))+geom_bar(stat="identity")+facet_wrap(~task)


## ----pairwise, include=FALSE----------------------

vas_rgt_emm <- emmeans::emmeans(m_vas, pairwise ~ 
                         rate | group | task,
                       transform = "response")

vas_rgt_emm$contrasts %>% as.data.frame() %>%
  mutate(across(where(is.numeric),round, digits = 3)) %>%
  mutate(id = paste(contrast, group, task, sep = "_")) %>%
  filter(str_detect(contrast,"H1 - S")) %>%
  #filter(str_detect(contrast,"H1 - F")) %>%
  # filter(str_detect(contrast,"H1") |
  #        contrast %in% c("S4 - S2", "S3 - S2") |
  #          contrast %in% c("F2 - F3", "F3 - F4"))# %>%
    flextable() %>%
    bold(i = ~ p.value < 0.05) %>%
  color(color = "#B20000", i = ~ p.value < 0.05 & estimate < 0) %>%
  color(color = "blue", i = ~ p.value < 0.05 & estimate > 0) %>%
color(color = "purple", i = ~ p.value > 0.05 & p.value < 0.1)
    
emm_task <- vas_rgt_emm$contrasts %>% as.data.frame() %>%
  mutate(across(where(is.numeric),round, digits = 3)) %>%
  mutate(id = paste(contrast, group, task, sep = "_")) %>%
  mutate(id = str_replace_all(id," - ","")) %>%
  mutate(across(where(is.character), str_trim)) %>%
  filter(str_detect(contrast,"H1") |
         contrast %in% c("S4 - S3", "S3 - S2") |
           contrast %in% c("F2 - F3", "F3 - F4")) %>%
  column_to_rownames(var = "id")

emm_taskB <- emm_task %>% select(estimate)
emm_taskP <- emm_task %>% select(p.value)


## ----sits-dur-coef, include = FALSE---------------
sits_dur_coef %>% 
  mutate(across(where(is.numeric),round,3)) %>%
  flextable()


## ----tab-pairwise-wpm-----------------------------
sits_dur_gr_emm$contrasts %>% as.data.frame() %>%
  mutate(group = str_replace_all(group, group_revised)) %>%
  filter(contrast %in% 
           c("S4 - S3", "S3 - S2", "S2 - H1",
             "H1 - F2", "F2 - F3", "F3 - F4")) %>%
  mutate(across(where(is.numeric),round,3)) %>%
  flextable()


## ----emm-sit, include = TRUE----------------------
emm_task %>% 
  filter(task == "Sentence") %>%
  mutate(group = str_replace_all(group, group_revised)) %>%
  mutate(estimate = estimate*100,
         SE = SE*100) %>%
  select(group, contrast, estimate, SE, z.ratio, p.value) %>%
  flextable() %>%
  bold(i = ~ p.value < 0.05, j=c("contrast","p.value")) %>%
  merge_v(j = "group") %>%
  border(j = 1, border.top = fp_border()) %>%
  fix_border_issues(part = "body")


## ----emm-mon, include = TRUE----------------------
emm_task %>% 
  mutate(group = str_replace_all(group, group_revised)) %>%
  filter(task == "Spontaneous") %>%
  mutate(estimate = estimate*100,
         SE = SE*100) %>%
  select(group, contrast, estimate, SE, z.ratio, p.value) %>%
  flextable() %>%
  bold(i = ~ p.value < 0.05, j=c("contrast","p.value")) %>%
  italic(i = ~ p.value < 0.1 & p.value > 0.05, j=c("contrast","p.value")) %>%
  border(j = 1, border.top = fp_border(), part = "body") %>%
  merge_v(j = "group", part = "body")
  #border_inner_h(border = fp_border())
  


## ----plot-rgt-participants-old--------------------
yc_color <- lacroix_palette("PeachPear",n=4)[1]
oc_color <- lacroix_palette("PeachPear",n=4)[2]
pd_color <- lacroix_palette("PeachPear",n=4)[3]
dbs_color <- lacroix_palette("PeachPear",n=4)[4]
pddbs_color <- lacroix_palette("PeachPear", n=6)[5]
control_colors <- c(yc_color, oc_color)
pd_colors <- c(pd_color, dbs_color)

width = 10
dpi = 300
units = "in"

df_vas_p <- df_vas %>%
  group_by(participant,group, task, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop))

# THIS HACK SUCKS just don't bother
# A terribly ugly hack to make facets the same size by adding blank participants to even out the groups.
# This is perhaps the ugliest thing ever
# Need to get up to 21 participants to get the right dimensions
#   YC: add 4
#   OC: add 5
#   DBS: add 9
# filler_partic <- as.data.frame(rbind(
#   c("5X1","DBS","Sentence","H1",0),
#   c("5X2","DBS","Sentence","H1",0),
#   c("5X3","DBS","Sentence","H1",0),
#   c("5X4","DBS","Sentence","H1",0),
#   c("5X5","DBS","Sentence","H1",0),
#   c("5X6","DBS","Sentence","H1",0),
#   c("5X7","DBS","Sentence","H1",0),
#   c("5X8","DBS","Sentence","H1",0),
#   c("5X9","DBS","Sentence","H1",0),
#   c("2X1","OC","Sentence","H1",0),
#   c("2X2","OC","Sentence","H1",0),
#   c("2X3","OC","Sentence","H1",0),
#   c("2X4","OC","Sentence","H1",0),
#   c("2X5","OC","Sentence","H1",0),
#   c("1X1","YC","Sentence","H1",0),
#   c("1X2","YC","Sentence","H1",0),
#   c("1X3","YC","Sentence","H1",0),
#   c("1X4","YC","Sentence","H1",0)))
# 
# colnames(filler_partic) <- colnames(df_vas_p)
# filler_partic <- filler_partic %>% mutate(vas_prop = as.numeric(vas_prop))
# df_vas_p <- rbind(df_vas_p, filler_partic)

df_vas_p <- df_vas_p %>%
  mutate(ID = str_sub(participant,start=2,end=3)) %>%
  mutate(group = str_replace_all(group,"PD","PD-Med"),
         group = str_replace_all(group, "DBS","PD-DBS"),
         groupID = factor(paste(group,ID,sep = " "))) %>%
  filter(task=="Sentence")

# tmp <- levels(df_vas_p$groupID)
# levels(df_vas_p$groupID) <- c(tmp, "PD-DBS X1", "PD-DBS X2", "PD-DBS X3")

# PDs
pd_partic <- df_vas_p %>%
  filter(group=="PD-Med") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = pd_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")
pd_partic

ggsave("../manuscripts/radi_intell_manuscript/figs/pd_partic.pdf", width=width, height=width, units = units, dpi=dpi)

dbs_partic <- df_vas_p %>%
  filter(group=="PD-DBS") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5, drop=TRUE)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = dbs_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  theme(axis.title.y = element_blank())
dbs_partic

ggsave("../manuscripts/radi_intell_manuscript/figs/dbs_partic.pdf", width=width, height=width, units = units, dpi=dpi)


yc_partic <- df_vas_p %>%
  filter(group == "YC") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = yc_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")
yc_partic

ggsave("../manuscripts/radi_intell_manuscript/figs/yc_partic.pdf", width=width, height=width, units = units, dpi=dpi)


oc_partic <- df_vas_p %>%
  filter(group == "OC") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = oc_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  theme(axis.title.y = element_blank())
oc_partic

ggsave("../manuscripts/radi_intell_manuscript/figs/oc_partic.pdf",width=width, height=width, units = units, dpi=dpi)

cowplot::plot_grid(yc_partic, oc_partic, pd_partic, dbs_partic, ncol=2,
                   align = 'h', axis = 'lr', labels="AUTO")
ggsave("../manuscripts/radi_intell_manuscript/figs/partic_sit.pdf",width=width, height=width, units = units, dpi=dpi)
 #rel_heights = c(1, 1, 1.5, 0.5)






## ----plot-rg-participants-no-task-----------------
yc_color <- lacroix_palette("PeachPear",n=4)[1]
oc_color <- lacroix_palette("PeachPear",n=4)[2]
pd_color <- lacroix_palette("PeachPear",n=4)[3]
dbs_color <- lacroix_palette("PeachPear",n=4)[4]
pddbs_color <- lacroix_palette("PeachPear", n=6)[5]
control_colors <- c(yc_color, oc_color)
pd_colors <- c(pd_color, dbs_color)

width = 10
dpi = 300
units = "in"

df_vas_p <- df_vas %>%
  group_by(participant,group, rate_ordered) %>%
  summarize(vas_prop = mean(vas_prop))


df_vas_p <- df_vas_p %>%
  mutate(ID = str_sub(participant,start=2,end=3)) %>%
  mutate(group = str_replace_all(group,"PD","PD-Med"),
         group = str_replace_all(group, "DBS","PD-DBS"),
         groupID = factor(paste(group,ID,sep = " ")))

# PDs
pd_partic <- df_vas_p %>%
  filter(group=="PD-Med") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = pd_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  ylim(c(0,1))
pd_partic

# ggsave("../manuscripts/radi_intell_manuscript/figs/pd_partic.pdf", width=width, height=width, units = units, dpi=dpi)

dbs_partic <- df_vas_p %>%
  filter(group=="PD-DBS") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5, drop=TRUE)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = dbs_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  theme(axis.title.y = element_blank())+
  ylim(c(0,1))
dbs_partic

# ggsave("../manuscripts/radi_intell_manuscript/figs/dbs_partic.pdf", width=width, height=width, units = units, dpi=dpi)


yc_partic <- df_vas_p %>%
  filter(group == "YC") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = yc_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  ylim(c(0,1))
yc_partic

# ggsave("../manuscripts/radi_intell_manuscript/figs/yc_partic.pdf", width=width, height=width, units = units, dpi=dpi)


oc_partic <- df_vas_p %>%
  filter(group == "OC") %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  facet_wrap(~groupID, nrow=5)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = oc_color,
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  theme(axis.title.y = element_blank())+
  ylim(c(0,1))
oc_partic

# ggsave("../manuscripts/radi_intell_manuscript/figs/oc_partic.pdf",width=width, height=width, units = units, dpi=dpi)

# cowplot::plot_grid(yc_partic, oc_partic, pd_partic, dbs_partic, ncol=2,
#                    #align = 'v', axis = 'lr', labels="AUTO",
#                    rel_widths = c(1.5, 0.5))


# Make two grids then combine
partic_controls <- cowplot::plot_grid(yc_partic, oc_partic, labels = c("A","B"))
partic_pd <- cowplot::plot_grid(pd_partic,dbs_partic,labels = c("C","D"),
                                rel_widths = c(1.25, 0.75))

cowplot::plot_grid(partic_controls,partic_pd, ncol=1)
#plot_grid(p3, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)
ggsave("../manuscripts/radi_intell_manuscript/figs/partic_sit-mon.pdf",width=width, height=width, units = units, dpi=dpi)






## ----all-participants, include = FALSE------------

df_vas_p %>%
  mutate(ID = str_sub(participant,start=2,end=3)) %>%
  mutate(group = str_replace_all(group,"PD","PD-Med"),
         group = str_replace_all(group, "DBS","PD-DBS"),
         groupID = paste(group,ID,sep = " ")) %>%
  filter(task=="Sentence") %>%
  filter(group %in% c("PD-Med","PD-DBS")) %>%
  #filter(prob_partic==TRUE) %>%
  ggplot(aes(x=rate_ordered, y=vas_prop,
             group = groupID,
             color = group))+
  geom_point()+
  geom_line(size=1)+
  #facet_grid(~group)+
  facet_grid(ID~group, space = "free_y", drop = TRUE)+
  #facet_grid(~groupID,scales = "free_x", space = "free_x")+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4),
                      name = "Group")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")


