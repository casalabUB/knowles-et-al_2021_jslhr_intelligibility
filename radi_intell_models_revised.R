# RADI intell models revised for intell manuscript

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


# "better" to include model with nwords (complexity), but this is difficult to interpret, so running simplified model instead
anova(m_vas, m_complexity) # better to include the model with nwords


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
qqnorm(resid(m_vas)) # a little wonky at the negative extremes
qqline(resid(m_vas))

hist(df_vas$vas_prop)
hist(logit(df_vas$vas_prop))
hist(car::logit(df_vas$vas_prop)) # identical

# WPM ~ rate condition
plot(m_sits_dur)
qqnorm(resid(m_sits_dur))
qqline(resid(m_sits_dur)) 


# Problematic participants ----
# Do the edgecase participants make a difference in the model? No.
problematic_participants <- c(
  # # Dentures
  # "203","206","211","310","314","320","322","506",
  # # L2 English
  # "301", "310", "320", "316",
  # # childhood stuttering
  # "301", "303",
  # # TIA
  # "301", "514",
  # # Distorted speech controls
  # "206", "210")
  
# Reviewer 2 requests
  # low MoCA
  #"302", "502", "506",
  # early onset
  "304", "309")


df_vas <- df_vas %>%
  select(-problematic_participants) %>%
  mutate(prob_partic = if_else(participant %in% problematic_participants,TRUE,FALSE))

summary(df_vas$prob_partic)
#    Mode   FALSE    TRUE 
# logical   18521     552
df_vas %>% select(participant, prob_partic) %>% filter(participant=="304")
df_vas_subset <- df_vas %>% filter(prob_partic == FALSE)

xtabs(~participant, df_vas)
xtabs(~participant+prob_partic, df_vas)
length(unique(df_vas$participant)) #69
xtabs(~participant, df_vas_subset)
length(unique(df_vas_subset$participant)) #58


# __Model ----
m_vas_problematic <- lmerTest::lmer(
     psych::logit(vas_prop) ~
          group*rate*task+
          listener +
          prob_partic +
          (1 + (S4vH1 + S3vH1 + S2vH1 + F2vH1 + F3vH1 + F4vH1) || participant),
     data = df_vas,
     REML = FALSE
)
summary(m_vas_problematic)
# problematic participants (dentures, TIA, etc) did NOT improve model fit
# low moca partics alone did NNOT improve model fit
# low moca + early onset DID improve model fit
anova(m_vas, m_vas_problematic)
beepr::beep(2)

# 12/10 done with early onsets removed, 304, 309
m_vas_problematic_removed <- lmerTest::lmer(
     psych::logit(vas_prop) ~
          group*rate*task+
          listener +
          (1 + (S4vH1 + S3vH1 + S2vH1 + F2vH1 + F3vH1 + F4vH1) || participant),
     data = filter(df_vas,prob_partic==FALSE),
     REML = FALSE
)

summary(m_vas_problematic_removed)
#Difs: 
# - main effects same
# - slow speech: 
#sjPlot::tab_model(m_vas_problematic, p.val = "kr", show.df = TRUE)

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


# Use this just to visualize task effects. This is not the final model
emmeans::emmip(m_task, group~task~rate, CIs = TRUE)+
  scale_linetype_manual(values = c("dashed","dashed","dashed","dashed",
                                   "solid","solid","solid","solid"))+ #not working
  theme_bw()+
  scale_x_discrete(limits = c("S4","S3","S2","H1","F2","F3","F4"))+
  facet_wrap(~group)#+

emmip(m_vas, ~group)
emmip(m_vas, ~complexity)

# Plot model results with sjPlot ----
# library(sjPlot)
# terms_complexity <- unique(filter(vas_coef, str_starts(Contrast, "complexity"))$Contrast)
# terms_group <- c("groupYC vs rest","groupOC vs rest","groupPD vs DBS")
# sjPlot::plot_model(m_vas)
# sjPlot::plot_model(m_vas, terms = terms_complexity, show.values = TRUE)
# sjPlot::plot_model(m_vas, terms = terms_group, show.values = TRUE)



save.image("../manuscripts/radi_intell_manuscript/radi_intell_vas.RData")

# wpm ~ rate condition
# Ridgeplots
df_sits_dur %>%
     ggplot(aes(x=wpm, y=rate_ordered, fill=group, alpha = rate_ordered, 
                height = ..density..)) + 
     geom_density_ridges(stat="density")+
     scale_fill_manual(values = lacroix_palette("PeachPear", n=4),
                        name="Group")+
     scale_color_manual(values = lacroix_palette("PeachPear", n=4),
                        name="Group")+
  scale_alpha_manual(values = c(0.5,0.5,0.5,1,0.5,0.5,0.5))+
    geom_vline(data = df_sits_habit_dur, aes(xintercept = wpm_habit,color=group), linetype="dashed")+
  facet_wrap(~group)+
     labs(x="Speech rate (WPM) \nSlow to fast",
          y="Group \nDensity")+
        guides(fill=FALSE, color=FALSE,alpha = FALSE)+
  theme_bw()

# Lines
df_sits_dur %>%
  group_by(participant, rate_ordered, group, item) %>%
  summarize(wpm = mean(wpm)) %>%
     ggplot(aes(x = rate_ordered, y = wpm, 
                color = group, group = participant)) + 
     geom_jitter(alpha=0.075) +
     stat_smooth(method = 'loess',
                 geom = 'line', alpha=0.6) +
  geom_smooth(method = "loess", aes(group=group),
              color = "black", alpha = 1,
              size = 1)+
  facet_wrap(~group)+
  geom_vline(xintercept = "H1", linetype = "dashed", color = "darkgrey")+
  #geom_hline(yintercept = mean(subset(df_sits_dur,rate=="H1")$wpm), linetype="dashed", color="darkgrey")+
  geom_hline(data = df_sits_habit_dur, aes(yintercept = wpm_habit,color=group), linetype="dashed")+#, linetype="dashed", color="darkgrey")+
     theme(legend.position = "blanks")+
     scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))+
guides(color=FALSE)+
  theme_bw()+
  ylab("WPM")+
  xlab("Rate condition \n(slow to fast)")


# No bueno
df_vas %>%
  ggplot(aes(
    #x=car::logit(vas_prop),
    x=vas_prop,
    y=rate_ordered,
    fill=group))+
  ggridges::geom_density_ridges()+
  #facet_wrap(~group,ncol=1)+
  #geom_vline(data=df_vas, aes(xintercept=mean(car::logit(vas_prop))))+
  theme_bw()

df_vas %>%
     ggplot(aes(x = rate_ordered, y = vas_prop, 
                color = group, group = participant)) + 
     geom_jitter(alpha=0.075) +
     geom_smooth(method = 'loess',alpha=0.1) +
  geom_smooth(method = "loess", aes(group=group),color = "white", se = TRUE)+
  geom_vline(xintercept = "H1", linetype = "dashed", color = "darkgrey")+
     #geom_vline(xintercept = which(vas_gr$rate=="h1"), linetype = "dashed", color="darkgrey")+
     facet_wrap(~group)+
     theme(legend.position = "blanks")+
           #strip.background = element_rect(fill=lacroix_palette("PassionFruit")[2]),
           #strip.text = element_text(color = "white"),
           #axis.text = element_blank())+
     scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))+
  guides(color=FALSE)+
  theme_bw()

# Use this one to show group trend
df_vas %>%
  ggplot(aes(x=rate_ordered, y = vas_prop, color = group, group = group))+
  #geom_jitter(stat='identity',alpha=0.075)+
  geom_smooth(method = "loess", level=0.95)+
  facet_wrap(~task)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")

# problematic participants
# Use this one to show group trend
df_vas %>%
  ggplot(aes(x=rate_ordered, y = vas_prop, color = problematic_participants, group = participant))+
  #geom_jitter(stat='identity',alpha=0.075)+
  geom_smooth(method = "loess", level=0.99)+
  facet_wrap(~task)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Rate condition \n(slow to fast)")+
  facet_wrap(~problematic_participants)

# By complexity
df_vas %>%
  ggplot(aes(x=complexity, y = vas_prop, color = group, group = group))+
  #geom_jitter(stat='identity',alpha=0.075)+
  geom_smooth(method = "loess", level=0.99)+
  #facet_wrap(~task)+
  geom_vline(xintercept="H1", linetype = "dashed", color="darkgrey")+
  scale_color_manual(values = lacroix_palette("PeachPear", n=4, type = "continuous"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Intelligibility \n(low to high)")+
  xlab("Complexity")


# sandboxing & learning ----
# TIL: the logit function defaults to psych::logit (find that out by typing the function name into the console and seeing what its namespace is). it's the same under the hood as car::logit except that car::logit allows for adjustment of 0s and 1s. Since there are no 0s and 1s in this dataset, it's ok to just use logit (not car::logit). the results are the same. critically, using logit (not car) allows me to use the argument transform = "response" in the emmeans function to backtransform from logit to the response scale. this is more useful for interpreting the responses!
# regarding the logit scale: i was trying to figure out why the logit-responses were always positive; it's because (i think) that the proportions were always greater than 0.5... since logit transforms 0 to 1 to -6 to +6, 0 is the midway (which corresponds to 0.5). Since no estimates fell below 50% intelligibility(??), the logit response was always positive.

test <- df_vas %>% select(vas_prop) %>% mutate(psych_logit = logit(vas_prop),car_logit = car::logit(vas_prop))
# > summary(test)
#     vas_prop         psych_logit       car_logit     
#  Min.   :0.000789   Min.   :-7.144   Min.   :-7.144  
#  1st Qu.:0.829519   1st Qu.: 1.582   1st Qu.: 1.582  
#  Median :0.949224   Median : 2.928   Median : 2.928  
#  Mean   :0.859456   Mean   : 3.088   Mean   : 3.088  
#  3rd Qu.:0.992634   3rd Qu.: 4.903   3rd Qu.: 4.903  
#  Max.   :0.999211   Max.   : 7.144   Max.   : 7.144 

tmp1 <- lmerTest::lmer(
     car::logit(vas_prop) ~
       group*task+
          listener +
          (1 | participant),
     data = df_vas,
     REML = FALSE
)
tmp2 <- lme4::lmer(
     car::logit(vas_prop) ~
       group*task+
          listener +
          (1 | participant),
     data = df_vas,
     REML = FALSE
)
anova(tmp1,tmp2) # identical whether fit with lmertest or lme4 package
summary(tmp1)
summary(tmp2) # identical just no pvals

tmp3 <- lme4::lmer(
     logit(vas_prop) ~
       group*task+
          listener +
          (1 | participant),
     data = df_vas,
     REML = FALSE
)
anova(tmp2,tmp3)
summary(tmp3)

tmp4 <- lme4::lmer(
     car_logit_test(vas_prop) ~
       group*task+
          listener +
          (1 | participant),
     data = df_vas,
     REML = FALSE
)
anova(tmp3,tmp4) # identical to tmp3
emmeans::emmeans(tmp4, pairwise ~ group | task, transform = "response")

