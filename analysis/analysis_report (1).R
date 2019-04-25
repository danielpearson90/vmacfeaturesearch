#' ---
#' title: "Analysis Report"
#' author: "Daniel Pearson"
#' date: "18 April 2019"
#' output: html_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE, echo=FALSE------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(knitr.duplicate.label = 'allow')
library(tidyverse)
library(R.matlab)
library(afex)
library(emmeans)
library(here)
library(patchwork)
library(cowplot)
library(BayesFactor)
library(hrbrthemes)
library(ggsignif)
library(summarytools)

st_options(bootstrap.css     = FALSE,       # Already part of the theme so no need for it
           plain.ascii       = FALSE,       # One of the essential settings
           style             = "rmarkdown", # Idem.
           dfSummary.silent  = TRUE,        # Suppresses messages about temporary files
           footnote          = NA,          # Keeping the results minimalistic
           subtitle.emphasis = FALSE)       # For the vignette theme, this gives
                                            # much better results. Your mileage may vary.

st_css()

library(knitr)
opts_chunk$set(comment=NA, prompt=FALSE, cache=FALSE, echo=TRUE, results='asis')

source(here::here("R", "dz_calculator.r"))
source(here::here("R", "vmac_pal.R"))
source(here::here("R", "summarySEwithin2.r"))

vmacCols <- vmac_pal()
my_theme <- theme_ipsum_rc(grid = F, axis_title_just = "c", axis_title_size = 14, axis_text_size = 12, strip_text_size = 14, plot_title_size = 14, subtitle_size = 14, ticks = T) +
  theme(legend.position = "none", 
        plot.margin = margin(.5,1,.5,1,"cm"),
        axis.line = element_line(colour = "black", size = .5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = "black", size = .5),
        #aspect.ratio = 1.1,
        plot.tag = element_text(size = 20, family = font_rc),
        axis.title = element_text(margin = margin(12,1,1,1, unit = "cm")),
        axis.text.x = element_text(margin = margin(t = 4, unit = 'pt'), colour = 'black'),
        axis.text.y = element_text(margin = margin(r = 4, unit = 'pt'), colour = 'black'),
        axis.title.x = element_text(margin = margin(t = 8, unit = 'pt')),
        axis.title.y = element_text(margin = margin(r = 8, unit = 'pt')),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))


#' 
#' # Omission Trial Analysis
#' 
## ----Load Behavioural Data, include=FALSE--------------------------------
files <- dir(path = here::here("analysis", "data", "raw_data", "BehavData"), pattern = "*.mat")

filedata <- here::here("analysis", "data", "raw_data", "BehavData", files) %>%
  map(readMat) %>%
  map("DATA")

#' 
## ----Tidy Behavioural Data, include = FALSE------------------------------
#This is some messy code that lets us tidy the exptTrialInfo data from the nested .mat struct files

subNums <- filedata %>%
  map(1) %>%
  flatten() %>%
  map_dbl(1)

exptdata <- filedata %>%
  map(24) %>%
  map(2) %>%
  modify(as.data.frame)
  
counterbals <- filedata %>%
  map(2) %>%
  flatten() %>%
  map_dbl(1)

ages <- filedata %>%
  map(10) %>%
  flatten() %>%
  map_dbl(1)

ages_summ <- ages %>%
  tibble() %>% 
  rename("age" = ".")%>%
  summarise(m = mean(age, na.rm = T), stdev = sd(age))

bonuses <- filedata %>%
  map(17) %>%
  flatten() %>% 
  map_dbl(1)

bonuses_sum <- bonuses %>%
  tibble() %>% 
  rename("bonus" = ".") %>%
  summarise(m = mean(bonus, na.rm = T), stdev = sd(bonus))

genders <- filedata %>%
  map(11) %>%
  flatten()  %>%
  map_chr(1) %>%
  tolower()

genders_summ <- genders %>%
  as.factor() %>% 
  tibble() %>% 
  rename("gender" = ".") %>%
  count(gender)

exptdata <- Map(cbind, exptdata, sub = subNums, counterbal = counterbals, age = ages, gender = genders) %>%
  bind_rows()

colnames(exptdata)[1:24] <- c("block", "trial", "trialCounter", "trials_since_break", "targetLoc", "targetType", "distractLoc", "distractShape", "distractType", "fixationTime", "fixationPropGoodSamples", "fixationTimeout", "trialPropGoodSamples", "timeout", "softTimeoutTrial", "omissionTrial", "rt", "trialPay", "sessionPay", paste("timeOnLoc", c(1:5))) 

exptdata <- exptdata %>%
  mutate(condition = if_else(counterbal %in% c(1,2), "Singleton Search", "Feature Search")) 

exptdata <- exptdata %>% 
  mutate(gender = tolower(gender)) %>% 
  mutate(dType = factor(distractType, levels = c(1,2,3), labels = c("High", "Low", "Absent")))

#' 
#' We first removed participants who had poor gaze data (mean proportion of valid gaze samples during the fixation or trial period < .5).
## ----Remove participants with poor gaze data-----------------------------
propCutoff <- .5
allTracks <- exptdata %>%
  group_by(sub) %>%
  summarise(fixationProp = mean(fixationPropGoodSamples),
            trialProp = mean(trialPropGoodSamples)) 

poortracks <- allTracks %>%
  filter(fixationProp < propCutoff | trialProp < propCutoff)

allTracks <- allTracks %>%
  filter(!sub %in% poortracks$sub)

allTracks_summ <- allTracks %>%
  summarise(mean_fixation_prop = round(mean(fixationProp)*100, 1), sd_fixation_prop = round(sd(fixationProp)*100, 1), mean_trial_prop = round(mean(trialProp)*100, 1), sd_trial_prop = round(sd(trialProp)*100,1))

exptdata <- exptdata %>%
 filter(!sub %in% poortracks$sub)

#' 
#' `r length(poortracks)` participants were removed due to having a poor quality eye gaze data.
#' 
## ----Remove participants for any other reasons---------------------------

remove_p_numbers <- c(37)

#  check to make sure that everyone has completed the full 480 trials (e.g., if the experiment crashed/was abandoned due to poor gaze data)
didNotComplete <- exptdata %>%
  group_by(sub) %>%
  filter(block > 0) %>%
  tally() %>%
  filter(n < 480) %>%
  select(sub)

# remove those that did not complete the full expt
remove_p_numbers <- c(remove_p_numbers, didNotComplete$sub)

exptdata <- exptdata %>%
  filter(!sub %in% remove_p_numbers)

#' 
#' During a post-experiment debrief, participant #37 reported that they thought that they needed to look at the coloured circle in order to earn rewards, and so they were removed from the data set.
#' 
#' Then we removed the first two trials of the experiment, the first two trials after breaks, and timeouts from the data (in line with previous VMAC analysis protocols, e.g. Le Pelley et al, 2015 - __JEP:General__).
#' 
## ----Additional data exclusions------------------------------------------
# first we determine the percentage of trials that were timeouts (for reporting in the MS)
hardtimeouts <- exptdata %>%
  group_by(timeout) %>%
  summarise(n = n()) %>%
  mutate(perc = round(n/sum(n)*100, 1))

exptdata <- exptdata %>%
  filter(trial > 2 & trials_since_break > 2) %>% #removing first two trials and trials after breaks
  filter(timeout != 1) # removing timeouts

#' 
#' This is the data set that we are left with:
#' 
## ----Glimpse exptdata after exclusions-----------------------------------
print(dfSummary(exptdata, style = "grid", graph.magnif = 0.75, tmp.img.dir = "/tmp"),  max.tbl.height = 300, method = "render")

#' 
#' Key for each of the variables in the table:
#' 
## ----table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'----
tabl <- "
| Variable name | Explanation                                  |
|---------------|--------------------------------------------|
| block         | Experiment block (1--8; 60 trials per block) |
| trial         | Experiment trial (1--480)      |
| trialCounter  | Trial counter within a block (1--60)      |
| trials_since_break         | Trial counter since last break (same as trialCounter for this experiment) |
| targetLoc         | Location of the target stimulus of 4 possible locations     |
| targetType  | Whether the target was a diamond or a circle (1 - diamond, 2 - circle)    |
| distractLoc         | Location of the singleton distractor (or random non-singleton on distractor absent trials) |
| distractShape         | The shape that defined the singleton distractor (1 - diamond, 2 - circle, 3 - hexagon, 4 - square)      |
| distractType  | Whether it was a high (1), low (2), or absent (3) trial     |
| fixationTime         | Length of fixation period in seconds |
| fixationPropGoodSamples         | The proportion of valid samples within the fixation period     |
| fixationTimeout  | Whether the fixation timeout period (4 seconds) was reached before a valid fixation was recorded  |
| trialPropGoodSamples         | The proportion of valid samples within the trial period |
| timeout         | Whether a hard-timeout (2 seconds) was reached before the target was fixated      |
| softTimeoutTrial  | Whether a soft-timeout (1 second) was reached, meaning that no reward was awarded   |
| omissionTrial         | Whether an omission trial was recorded  |
| rt         | Response time in seconds     |
| trialPay  | The reward for the current trial (in points)    |
| sessionPay         | Total reward accumulated during the experiment (in points) |
| timeOnLoc 1--5         | Amount of time (in seconds) that gaze was recorded on each stimulus location (1--4), and anywhere else (5)     |
| sub  | Participant identifier     |
| counterbal         | Code assigning participants to each condition (singleton vs feature) and colour--reward mapping |
| age         | Age in years     |
| gender  | Self-reported gender (m/f/o)  |
| condition         | Search condition (1 - feature search, 2 - singleton search)    |
| dType  | Distractor type (1 - high, 2 - low, 3 - absent)  |
"
cat(tabl) # output the table in a format good for HTML/PDF/docx conversion

#' 
#' ## Omission Trials
## ----Omission Trial Means, message=FALSE, warning=FALSE------------------
# Calculate mean proportion of omission trials for each trial type, by participant
exptdata_means_byparticipant <- exptdata %>%
  ungroup() %>%
  group_by(sub, condition, dType) %>%
  summarise(m = mean(omissionTrial))

# Calculate mean proportion of omission trials for each trial type, by condition
exptdata_means_bycondition <- summarySEwithin2(data = exptdata_means_byparticipant, measurevar = "m", betweenvars = "condition", withinvars = "dType", idvar = "sub") %>%
  select(-ends_with("Normed"))

#' 
#' Draw a graph of omission trials for each condition:
#' 
## ----Omission Trial Graph, fig.height=10, fig.width=5.5, message=FALSE, warning=FALSE, echo=FALSE----
omissionPlot_singleton <- exptdata %>%
  filter(condition == "Singleton Search") %>%
  group_by(sub, dType) %>%
  summarise(m = mean(omissionTrial)) %>%
  ggplot(aes(x = dType, y = m, fill = dType, shape = dType)) +
  geom_errorbar(data = filter(exptdata_means_bycondition, condition == "Singleton Search"), aes(x = dType, y = m, ymin = m-se, ymax = m+se), colour = "black", width = .15) +
  geom_line(aes(group = sub), colour = "black", alpha = .1, linetype = "solid", size = .5) +
  geom_line(data = filter(exptdata_means_bycondition, condition == "Singleton Search"), aes(x = dType, y = m, group = 1), colour = "black") + 
  geom_point(data = filter(exptdata_means_bycondition, condition == "Singleton Search"), aes(x = dType, y = m), colour = "black", size = 3) +
  geom_signif(comparisons = list(c("High", "Low"), c("Low", "Absent")), annotations = c("***","*"), y_position = c(.61,.57), colour = "black", tip_length = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,.8)) +
  scale_x_discrete(labels = c("High\nreward", "Low\nreward", "Distractor\nabsent")) +
  scale_fill_manual(values = vmacCols) +
  scale_colour_manual(values = vmacCols) +
  scale_shape_manual(name = "Trial Type", guide = guide_legend(), values = c(21,22,23)) +
  labs(y = "% Reward Omissions", x = "Trial Type", tag = "A", title = "Singleton Search") +
  my_theme

omissionPlot_feature <- 
  exptdata %>%
  filter(condition == "Feature Search") %>%
  group_by(sub, dType) %>%
  summarise(m = mean(omissionTrial)) %>%
  ggplot(aes(x = dType, y = m, fill = dType, shape = dType)) +
  geom_line(aes(group = sub), colour = "black", alpha = .1, linetype = "solid", size = .5) +
  geom_errorbar(data = filter(exptdata_means_bycondition, condition == "Feature Search"), aes(x = dType, y = m, ymin = m-se, ymax = m+se), colour = "black", width = .15) +
  geom_line(data = filter(exptdata_means_bycondition, condition == "Feature Search"), aes(x = dType, y = m, group = 1), colour = "black") + 
  geom_point(data = filter(exptdata_means_bycondition, condition == "Feature Search"), aes(x = dType, y = m), colour = "black", size = 3) +
  geom_signif(comparisons = list(c("High", "Low"), c("Low", "Absent")), annotations = c("***","***"), y_position = c(.61,.57), colour = "black", tip_length = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,.8)) +
  scale_x_discrete(labels = c("High\nreward", "Low\nreward", "Distractor\nabsent")) +
  scale_fill_manual(values = vmacCols) +
  scale_shape_manual(name = "Distractor Type", guide = guide_legend(), values = c(21,22,23)) +
  labs(y = "% Reward Omissions", x = "Trial Type", tag = "B", title = "Feature Search") +
  my_theme

omissionPlot_ms <- omissionPlot_singleton / omissionPlot_feature

save_plot(here::here("analysis", "figures", "omission_plot.png"), omissionPlot_ms, ncol = 1, nrow = 2, type = "cairo", base_aspect_ratio = 1.1, dpi = 300)

omissionPlot_ms


#' 
#' ### Effect of Reward
#' Effect of reward ANOVA:
## ----Effect of Reward - Omissions----------------------------------------
omissions_vmac.ANOVA <- aov_car(m ~ condition*dType + Error(sub/dType), data = filter(exptdata_means_byparticipant, dType != "Absent"), anova_table = list(es="pes"))

knitr::kable(nice(omissions_vmac.ANOVA))

#' 
#' 
#' Planned t-tests:
## ----Effect of Reward - Omissions - Planned t-tests, results="markup"----
singleton_high_omissions <- exptdata_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "High") %>%
  select(m) %>%
  pull()

singleton_low_omissions <-  exptdata_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "Low") %>%
  select(m) %>%
  pull()

singleton_absent_omissions <- exptdata_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "Absent") %>%
  select(m) %>%
  pull()

feature_high_omissions <- exptdata_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "High") %>%
  select(m) %>%
  pull()

feature_low_omissions <-  exptdata_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "Low") %>%
  select(m) %>%
  pull()

feature_absent_omissions <- exptdata_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "Absent") %>%
  select(m) %>%
  pull()

singleton_VMAC_omissions.ttest <- t.test(singleton_high_omissions, singleton_low_omissions, paired = T)
singleton_VMAC_omissions.ttest
singleton_VMAC_omissions.es <- dz_calculator(singleton_high_omissions, singleton_low_omissions)
singleton_VMAC_omissions.es
feature_VMAC_omissions.ttest <- t.test(feature_high_omissions, feature_low_omissions, paired = T)
feature_VMAC_omissions.ttest
feature_VMAC_omissions.es <- dz_calculator(feature_high_omissions, feature_low_omissions)
feature_VMAC_omissions.es

vmac_singleton <- exptdata_means_byparticipant %>%
  filter(dType != "Absent", condition == "Singleton Search") %>%
  spread(dType, m) %>%
  mutate(vmac = High - Low) %>%
  pull(vmac)

vmac_feature <- exptdata_means_byparticipant %>%
  filter(dType != "Absent", condition == "Feature Search") %>%
  spread(dType, m) %>%
  mutate(vmac = High - Low) %>%
  pull

vmac_condition_bayes <- ttestBF(x = vmac_singleton, y = vmac_feature)
vmac_condition_bayes

#' 
#' 
#' ### Effect of Physical Salience
#' Effect of physical salience ANOVA:
## ----Effect of Physical Salience - Omissions-----------------------------
omissions_salience.ANOVA <- aov_car(m ~ condition*dType + Error(sub/dType), data = filter(exptdata_means_byparticipant, dType != "High"), anova_table = list(es="pes"))

knitr::kable(nice(omissions_salience.ANOVA))

#' 
#' Planned t-tests:
## ----Effect of Physical Salience - Omissions - Planned t-tests, results="markup"----
singleton_physicalsalience_omissions.ttest <- t.test(singleton_low_omissions, singleton_absent_omissions, paired = T)
singleton_physicalsalience_omissions.ttest
singleton_physicalsalience_omissions.es <- dz_calculator(singleton_low_omissions, singleton_absent_omissions)
singleton_physicalsalience_omissions.es

feature_physicalsalience_omissions.ttest <- t.test(feature_absent_omissions, feature_low_omissions, paired = T) # reverse the order here so can get positive t value
feature_physicalsalience_omissions.ttest
feature_physicalsalience_omissions.es <- dz_calculator(feature_low_omissions, feature_absent_omissions)
feature_physicalsalience_omissions.es

#' 
#' # Gaze Data Analyses
#' 
## ----Load Gaze Data, include=FALSE---------------------------------------
saccade_files <- dir(path = here::here("analysis", "data", "derived_data","ProcessedSaccadeData"), pattern = "^S")

saccade_filedata <- here::here("analysis", "data", "derived_data","ProcessedSaccadeData", saccade_files) %>%
  map(readMat) %>%
  map("saccadeSessionData") %>%
  map(1) %>%
  map(2) %>%
  modify(as.data.frame)

#' 
## ----Tidy Gaze Data, include = FALSE-------------------------------------
# collapse the data into a dataframe format
saccade_filedata <- saccade_filedata %>%
  map_df(bind_rows)

# give the columns appropriate headers
colnames(saccade_filedata)[1:13] <- c("subj", "trial", "latency", paste("loc", c(1:5), sep = "_"), "discardTrial", "anticipatorySaccade", "outsideFixation", "noSaccades", "noValidData")

discardSaccadeCounter <- saccade_filedata %>%
  mutate(bad_data_discard = case_when(noSaccades == 1 ~ 1,
                                      noValidData == 1 ~ 1,
                                      TRUE ~ 0)) %>%
  group_by(subj) %>%
  summarise(prop_discarded = mean(discardTrial)*100, prop_anticipatory = mean(anticipatorySaccade)*100, prop_outside_fix = mean(outsideFixation)*100, prop_bad_data = mean(bad_data_discard)*100)

totalSaccadeTrialsDiscarded <- discardSaccadeCounter %>%
  summarise_all(mean) %>%
  summarise_all(round, 1)

saccade_filedata <- saccade_filedata %>%
  filter(discardTrial != 1) %>% # discard trials marked to be discarded
  mutate(first_saccade_loc = loc_1*1+loc_2*2+loc_3*3+loc_4*4+loc_5*5) 

saccadedata <- left_join(exptdata, saccade_filedata, by = c("sub" = "subj", "trial" = "trial")) %>% #join saccade data up with behavioural data
  mutate(saccade_to_target = if_else(targetLoc == first_saccade_loc, 1, 0)) %>% # create variables indicating whether first saccade went towards the target...
  mutate(saccade_to_singleton = if_else(dType == "Absent", 2, 
                                        if_else(distractLoc == first_saccade_loc, 1, 0))) %>% # singleton (for all except distractor absent trials)
  mutate(saccade_to_singleton = replace(saccade_to_singleton, saccade_to_singleton == 2, NA)) %>%
  mutate(saccade_to_nonsingleton = if_else((dType != "Absent" & targetLoc != first_saccade_loc & distractLoc != first_saccade_loc & first_saccade_loc < 5) | (dType == "Absent" & targetLoc != first_saccade_loc & first_saccade_loc < 5), 1, 0)) %>% # or non-singleton
  mutate(num_nonsingletons = if_else(dType == "Absent", 3, 2)) # add the number of nonsingletons for each trial type

#' 
## ----Glimpse saccadedata-------------------------------------------------
print(dfSummary(saccadedata, style = "grid", graph.magnif = 0.75, tmp.img.dir = "/tmp"),  max.tbl.height = 300, method = "render")

#' 
#' Key for each of the new variables in saccadedata data frame:
#' 
## ----table3, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'----
tabl2 <- "
| Variable name | Explanation                                  |
|---------------|--------------------------------------------|
| latency         | Saccade latency in milliseconds |
| loc 1--5         | Whether the first saccade was directed towards location 1--4, or somewhere else (loc 5)      |
| discardTrial  | Whether the trial should be excluded from saccade analyses due to exclusion criteria      |
| anticipatorySaccade         | Whether saccade latency was <80ms |
| outsideFixation         | Trials in which no gaze was reocrded within 100 pixels of fixation in the first 80 ms    |
| noSaccades  | Trials in which the saccade identification algorithm did not detect a saccade   |
| noValidData         | Trials in which there was insufficient valid gaze data to detect a saccade |
| first_saccade_loc         | The direction of the first saccade (locations 1--4, or elsewhere [loc 5])     |
| saccade_to_target  | Whether the first saccade was directed towards the target    |
| saccade_to_nonsingleton         | Whether the first saccade was directed towards one of the non-singleton distractors |
| num_nonsingletons         | The number of non-singleton distractors in the display (2 on high and low trials, 3 on distractor absent trials)     |
"
cat(tabl2) # output the table in a format good for HTML/PDF/docx conversion

#' 
#' ## First saccade direction
## ----First saccade direction - means, warning=FALSE, message=FALSE-------
# calculate summary statistics for each participant
saccadeData_means_byparticipant <- saccadedata %>%
  ungroup() %>%
  mutate(sub = as.factor(sub)) %>%
  group_by(sub, condition, dType) %>%
  summarise(saccade_to_target = mean(saccade_to_target, na.rm = T),
            saccade_to_singleton = mean(saccade_to_singleton, na.rm = T),
            saccade_to_nonsingleton = mean(saccade_to_nonsingleton, na.rm = T)/mean(num_nonsingletons, na.rm = T),
            num_nonsingletons = mean(num_nonsingletons, na.rm = T))

targetmeans <- summarySEwithin2(data = saccadeData_means_byparticipant, measurevar = "saccade_to_target", betweenvars = "condition", withinvars = c("dType"), idvar = "sub", na.rm = T) %>%
  rename(proportionSaccades = saccade_to_target) %>%
  mutate(stimType = "Target") %>%
  select(-ends_with("Normed"))

singletonmeans <- summarySEwithin2(data = saccadeData_means_byparticipant, measurevar = "saccade_to_singleton", betweenvars = "condition", withinvars = c("dType"), idvar = "sub", na.rm = T) %>%
  rename(proportionSaccades = saccade_to_singleton) %>%
  mutate(stimType = "Singleton")%>%
  select(-ends_with("Normed"))

nonsingletonmeans <- summarySEwithin2(data = saccadeData_means_byparticipant, measurevar = "saccade_to_nonsingleton", betweenvars = "condition", withinvars = c("dType"), idvar = "sub", na.rm = T) %>%
  rename(proportionSaccades = saccade_to_nonsingleton) %>%
  mutate(stimType = "Nonsingleton")%>%
  select(-ends_with("Normed"))

# collapse together
saccadeData_means_bycondition <- bind_rows(targetmeans, singletonmeans, nonsingletonmeans) %>%
  mutate(stimType = factor(stimType, levels = c("Target","Nonsingleton","Singleton")))

saccadePlotDataParticipants <- gather(saccadeData_means_byparticipant, key = "stimType", value = "proportionSaccades", starts_with("saccade_to")) %>%
  mutate(stimType = case_when(stimType == "saccade_to_target" ~ "Target",
                              stimType == "saccade_to_nonsingleton" ~ "Nonsingleton",
                              stimType == "saccade_to_singleton" ~ "Singleton",
                              TRUE ~ ""))

captureData_means_byparticipant <- saccadeData_means_byparticipant %>%
  filter(dType != "Absent") %>%
  mutate(Capture = saccade_to_singleton - saccade_to_nonsingleton)
  
captureMeans <- summarySEwithin2(captureData_means_byparticipant, measurevar = "Capture", betweenvars = "condition", withinvars = "dType", idvar = "sub") %>%
  select(-ends_with("Normed"))

#' 
#' Graph of the first saccade data and capture scores:
#' 
## ----First Saccade Direction - Plot, echo=FALSE, fig.height=10, fig.width=10, message=FALSE, warning=FALSE----
saccadePlot_singleton <- saccadeData_means_bycondition %>%
  filter(condition == "Singleton Search", dType != "Absent") %>%
  ggplot(aes(group = dType, y = proportionSaccades, fill = dType, x = stimType, shape = dType, colour = dType)) +
  geom_line(aes(linetype = dType, colour = dType), na.rm = T, size = .5, position = position_dodge(width = .6)) +
  geom_point(aes(group = dType, y = proportionSaccades, x = stimType), na.rm = T, size = 1.5, alpha = .3, stroke = F, data = filter(saccadePlotDataParticipants, condition == "Singleton Search", dType != "Absent"), position = position_jitterdodge(dodge.width = .6, jitter.width = .2)) +
  geom_errorbar(aes(ymin = proportionSaccades-se, ymax = proportionSaccades+se), na.rm = T, linetype = "solid", position = position_dodge(width = .6, preserve = "total"), show.legend = FALSE, colour = "black", width = .3) +
  geom_point(na.rm = T, size = 3, position = position_dodge(width = .6), colour = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,.8)) +
  scale_linetype_manual(name = "Trial Type", values = c("solid", "longdash", "dotdash"), guide = guide_legend(), labels = c("High-reward", "Low-reward")) +
  scale_fill_manual(name = "Trial Type", values = vmacCols, labels = c("High-reward", "Low-reward")) +
  scale_colour_manual(name = "Trial Type", values = vmacCols, labels = c("High-reward", "Low-reward")) +
  scale_shape_manual(name = "Trial Type", guide = guide_legend(), values = c(21,22,23), labels = c("High-reward", "Low-reward")) +
  ylab("% First Saccades") + xlab("Stimulus Type") + labs(tag = "A", title = "Singleton Search") +
  my_theme

saccadePlot_feature <- saccadeData_means_bycondition %>%
  filter(condition == "Feature Search", dType != "Absent") %>%
  ggplot(aes(group = dType, y = proportionSaccades, fill = dType, x = stimType, shape = dType, colour = dType)) +
  geom_line(aes(linetype = dType, colour = dType), na.rm = T, size = .5, position = position_dodge(width = .6)) +
  geom_point(aes(group = dType, y = proportionSaccades, x = stimType), na.rm = T, size = 1.5, alpha = .3, stroke = F, data = filter(saccadePlotDataParticipants, condition == "Feature Search", dType != "Absent"), position = position_jitterdodge(dodge.width = .6, jitter.width = .2)) +
  geom_errorbar(aes(ymin = proportionSaccades-se, ymax = proportionSaccades+se), na.rm = T, linetype = "solid", position = position_dodge(width = .6, preserve = "total"), show.legend = FALSE, colour = "black", width = .3) +
  geom_point(na.rm = T, size = 3, position = position_dodge(width = .6), colour = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,.8)) +
  scale_linetype_manual(name = "Distractor Type", values = c("solid", "longdash", "dotdash"), guide = guide_legend()) +
  scale_fill_manual(name = "Distractor Type", values = vmacCols) +
  scale_colour_manual(name = "Distractor Type", values = vmacCols) +
  scale_shape_manual(name = "Distractor Type", guide = guide_legend(), values = c(21,22,23)) +
  ylab("% First Saccades") + xlab("Stimulus Type") + labs(tag = "C", title = "Feature Search") +
  my_theme

legend_saccade <- get_legend(saccadePlot_singleton + theme(legend.position = "bottom",
                                                           legend.title = element_text(size = 14),
                                                           legend.text = element_text(size = 14),
                                                           legend.key.width = unit(24, 'pt')))
 

## Capture Bar Plots
captureBarPlot_singleton <- captureMeans %>%
  filter(condition == "Singleton Search") %>%
  mutate(annotation = c("***", " ")) %>%
  ggplot(aes(x = dType, y = Capture, fill = dType)) +
  annotate(geom = "text", label = "Capture", x = 2.85, y = .15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "text", label = "Suppression", x = 2.85, y = -.15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "segment", x = 2.65, xend = 2.65, y = -.03, yend = -.27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  annotate(geom = "segment", x = 2.65, xend = 2.65, y = .03, yend = .27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  geom_col(position = position_dodge(.9), size = .3) +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_errorbar(aes(ymax = Capture + se, ymin = Capture - se), position = position_dodge(.9), width = .1) +
  geom_text(aes(label = annotation), nudge_y = .05) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(-.3001, .3001)) +
  scale_x_discrete(labels = c("High\nreward", "Low\nreward")) +
  coord_cartesian(xlim = c(1,2),
                  clip = 'off') +
  scale_fill_manual(name = "Distractor Type", values = vmacCols, guide = guide_legend()) +
  ylab("Capture Score") + xlab("Trial Type") + labs(tag = "B") +
  my_theme 

captureBarPlot_feature <- 
  captureMeans %>%
  filter(condition == "Feature Search") %>%
  mutate(annotation = c("***", "*")) %>%
  ggplot(aes(x = dType, y = Capture, fill = dType)) +
  annotate(geom = "text", label = "Capture", x = 2.85, y = .15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "text", label = "Suppression", x = 2.85, y = -.15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "segment", x = 2.65, xend = 2.65, y = -.03, yend = -.27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  annotate(geom = "segment", x = 2.65, xend = 2.65, y = .03, yend = .27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  geom_col(position = position_dodge(.9), size = .3) + 
  geom_hline(yintercept = 0, size = .5) +
  geom_errorbar(aes(ymax = Capture + se, ymin = Capture - se), position = position_dodge(.9), width = .1) +
  geom_text(aes(label = annotation), nudge_y = c(.05, .07)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(-.3001, .3001)) +
  scale_x_discrete(labels = c("High\nreward", "Low\nreward")) +
  coord_cartesian(xlim = c(1,2),
                  clip = 'off') +
  scale_fill_manual(name = "Distractor Type", values = vmacCols, guide = guide_legend()) +
  ylab("Capture Score") + xlab("Trial Type") + labs(tag = "D") +
  my_theme

saccadePlot_ms_row1 <- saccadePlot_singleton + captureBarPlot_singleton + plot_spacer() + plot_layout(nrow = 1, widths = c(2,1,.1))

saccadePlot_ms_row2 <- saccadePlot_feature + captureBarPlot_feature + plot_spacer() + plot_layout(nrow = 1, widths = c(2,1,.1))

saccadePlot_ms <- saccadePlot_ms_row1 / legend_saccade / saccadePlot_ms_row2 + plot_layout(heights = c(1,.1,1))

save_plot(here("analysis", "figures", "saccade_plot.png"), saccadePlot_ms, ncol = 2, nrow = 2, type = "cairo", base_aspect_ratio = .9, dpi = 300)

saccadePlot_ms

#' 
#' 
## ----First Saccade Direction - Capture ANOVA, results="markup", message = FALSE----
capture.ANOVA <- aov_car(Capture ~ condition*dType + Error(sub/dType), data = captureData_means_byparticipant,  anova_table = list(es="pes"))

knitr::kable(nice(capture.ANOVA))

# Bayes Factor for interaction contrast
captureData_wide <- captureData_means_byparticipant %>%
  select(sub, condition, dType, Capture) %>%
  spread(dType, Capture) %>%
  mutate(vmac = High - Low)

capture_bayes <- ttestBF(x = captureData_wide$vmac[captureData_wide$condition == "Singleton Search"], 
                         y = captureData_wide$vmac[captureData_wide$condition == "Feature Search"])

capture_bayes


#' 
## ----First Saccade Direction - Planned t-tests, results = "markup"-------
Singleton_high.ttest <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "High") %>%
  select(Capture) %>%
  pull() %>%
  t.test()

Singleton_high.ttest

Singleton_high.es <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "High") %>%
  select(Capture) %>%
  summarise(mean_capture = mean(Capture), sd_capture = sd(Capture), d_z = mean_capture/sd_capture) %>%
  pull(d_z)

Singleton_high.es

Singleton_low.ttest <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "Low") %>%
  select(Capture) %>%
  pull() %>%
  t.test()

Singleton_low.ttest

Singleton_low.es <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "Low") %>%
  select(Capture) %>%
  summarise(mean_capture = mean(Capture), sd_capture = sd(Capture), d_z = mean_capture/sd_capture) %>%
  pull(d_z)

Singleton_low.es

Feature_high.ttest <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "High") %>%
  select(Capture) %>%
  pull() %>%
  t.test()

Feature_high.ttest

Feature_high.es <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "High") %>%
  select(Capture) %>%
  summarise(mean_capture = mean(Capture), sd_capture = sd(Capture), d_z = mean_capture/sd_capture) %>%
  pull(d_z)

Feature_high.es

Feature_low.ttest <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "Low") %>%
  select(Capture) %>%
  pull() %>%
  t.test()

Feature_low.ttest

Feature_low.ttest$statistic <- Feature_low.ttest$statistic*-1 # just making the t value positive for reporting in the manuscript

Feature_low.es <- captureData_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "Low") %>%
  select(Capture) %>%
  summarise(mean_capture = mean(Capture), sd_capture = sd(Capture), d_z = abs(mean_capture/sd_capture)) %>%
  pull(d_z)

Feature_low.es

#' 
#' ## Development of suppression/capture effect across blocks
#' 
## ----Block Analysis Data Tidying-----------------------------------------
saccadeData_means_byparticipant_byblock <- 
  saccadedata %>%
  mutate(sub = as.factor(sub), newblock = ceiling(as.numeric(block)/2)) %>%
  group_by(sub, condition, dType, newblock) %>%
  summarise(saccade_to_target = mean(saccade_to_target, na.rm = T),
            saccade_to_singleton = mean(saccade_to_singleton, na.rm = T),
            saccade_to_nonsingleton = mean(saccade_to_nonsingleton, na.rm = T)/mean(num_nonsingletons, na.rm = T),
            num_nonsingletons = mean(num_nonsingletons, na.rm = T),
            capture = saccade_to_singleton - saccade_to_nonsingleton) %>%
  filter(dType != "Absent")

capturemeans_byblock <- summarySEwithin2(data = saccadeData_means_byparticipant_byblock, measurevar = "capture", betweenvars = "condition", withinvars = c("dType", "newblock"), idvar = "sub", na.rm = T) %>%
  select(-ends_with("Normed"))

#' 
## ----Block Analysis Graph, echo=FALSE, fig.height=10, fig.width=5, message=FALSE, warning=FALSE----
capturePlot_byblock_singleton <- 
  capturemeans_byblock %>%
  filter(condition == "Singleton Search") %>%
  ggplot(aes(x = newblock, y = capture, group = dType, shape = dType, colour = dType)) +
  #geom_rect(fill = "#D3D3D3", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, alpha = .3, inherit.aes = F) +
  geom_hline(yintercept = 0, colour = "black", size = .5, linetype = "dotted") +
  annotate(geom = "text", label = "Capture", x = 4.7, y = .15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "text", label = "Suppression", x = 4.7, y = -.15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "segment", x = 4.5, xend = 4.5, y = -.03, yend = -.27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  annotate(geom = "segment", x = 4.5, xend = 4.5, y = .03, yend = .27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  geom_line(aes(linetype = dType), size = .5) +
  geom_errorbar(aes(ymin = capture - se, ymax = capture + se), width = .1, colour = "black") +
  geom_point(aes(shape = dType, fill = dType), size = 3, colour = "black") +
  scale_shape_manual(name = "Distractor Type", guide = guide_legend(), values = c(21,22)) +
  scale_fill_manual(name = "Distractor Type", values = vmacCols, guide = guide_legend()) +
  scale_colour_manual(name = "Distractor Type", values = vmacCols, guide = guide_legend()) +
  scale_y_continuous(expand = c(0,0), limits = c(-.3,.301), breaks = seq(-.3, .3, .1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete() +
  coord_cartesian(xlim = c(1,4),
                  clip = 'off') +
  scale_linetype_manual(name = "Distractor Type", values = c("solid", "longdash"), guide = guide_legend()) +
  ylab("Capture Score") + xlab("Epoch (96 trials)") + labs(tag = "A", title = 'Singleton Search') +
  my_theme

capturePlot_byblock_feature <- 
  capturemeans_byblock %>%
  filter(condition == "Feature Search") %>%
  ggplot(aes(x = newblock, y = capture, group = dType, shape = dType, colour = dType)) +
  geom_hline(yintercept = 0, colour = "black", size = .5, linetype = "dotted") +
  annotate(geom = "text", label = "Capture", x = 4.7, y = .15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "text", label = "Suppression", x = 4.7, y = -.15, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "segment", x = 4.5, xend = 4.5, y = -.03, yend = -.27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  annotate(geom = "segment", x = 4.5, xend = 4.5, y = .03, yend = .27, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  geom_line(aes(linetype = dType), size = .5) +
  geom_errorbar(aes(ymin = capture - se, ymax = capture + se), width = .1, colour = "black") +
  geom_point(aes(shape = dType, fill = dType), size = 3, colour = "black") +
  scale_shape_manual(name = "Distractor Type", guide = guide_legend(), values = c(21,22)) +
  scale_fill_manual(name = "Distractor Type", values = vmacCols, guide = guide_legend()) +
  scale_colour_manual(name = "Distractor Type", values = vmacCols, guide = guide_legend()) +
  scale_y_continuous(expand = c(0,0), limits = c(-.3,.301), breaks = seq(-.3, .3, .1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete() +
  coord_cartesian(xlim = c(1,4),
                  clip = 'off') +
  scale_linetype_manual(name = "Distractor Type", values = c("solid", "longdash"), guide = guide_legend()) +
  ylab("Capture Score") + xlab("Epoch (96 trials)") + labs(tag = "B", title = 'Feature Search') +
  my_theme

capturePlot_byblock_vert <- (capturePlot_byblock_singleton + plot_spacer() + plot_layout(widths = c(1,.1))) / legend_saccade / (capturePlot_byblock_feature + plot_spacer() + plot_layout(widths = c(1,.1))) + plot_layout(heights = c(1,.1,1))

capturePlot_byblock_vert

save_plot(here("analysis", "figures","capture_plot_by_block.png"), capturePlot_byblock_vert, ncol = 1, nrow = 2, type = "cairo", base_aspect_ratio = 1.1, dpi = 300)



#' 
## ----Block Analysis Statistics-------------------------------------------
capture_byblock.ANOVA <- aov_car(capture ~ newblock*dType*condition + Error(sub/newblock*dType), data = saccadeData_means_byparticipant_byblock, anova_table = list(es="pes"))

knitr::kable(nice(capture_byblock.ANOVA))

ref_all <- lsmeans(capture_byblock.ANOVA, ~newblock | dType)

capture_byblock.polys <- summary(contrast(ref_all, method = "poly")) %>%
  mutate(t.ratio = abs(t.ratio),
         df = round(df, digits = 0))

knitr::kable(capture_byblock.polys)

#' 
#' ## Vincentized Analysis
#' 
#' This analysis looks at the percentage of trials captured/suppressed by each distractor as a function of saccade latency.
#' 
## ----Vincentized Analysis - Data Tidying---------------------------------
quartiles_cutoffs <- saccadedata %>%
  group_by(sub, dType, condition) %>%
  summarise(q1 = quantile(latency, probs = .25, na.rm = T), q2 = quantile(latency, probs = .5, na.rm = T), q3 = quantile(latency, probs = .75, na.rm = T), q4 = quantile(latency, probs = 1, na.rm = T))

quartiles_cutoffs_figures <- saccadedata %>% 
  group_by(sub, dType, condition) %>%
  summarise(qs = list(enframe(quantile(latency, probs=seq(.25,.75,.25), na.rm = T)))) %>% 
  unnest

saccadedata <- full_join(saccadedata, quartiles_cutoffs, by = c("sub","dType", "condition"))

saccadedata <- saccadedata %>% 
  mutate(quartile = case_when(latency <= q1 ~ 1,
                              latency <= q2 ~ 2,
                              latency <= q3 ~ 3,
                              latency <= q4 ~ 4))

saccade_latency_quartiles_means_byparticipant <- saccadedata %>%
  ungroup() %>%
  drop_na(quartile) %>% 
  group_by(sub, condition, dType, quartile) %>%
  summarise(mean_latency = mean(latency, na.rm = T))

saccade_latency_quartiles_means_bycondition <- summarySEwithin2(saccade_latency_quartiles_means_byparticipant, measurevar = "mean_latency", betweenvars = "condition", withinvars = c("dType", "quartile"), idvar = "sub") %>%
  select(-ends_with("Normed"))

saccade_direction_quartiles_means_byparticipant <- saccadedata %>%
  ungroup() %>%
  drop_na(quartile) %>% 
  group_by(sub, condition, dType, quartile) %>%
  summarise(saccade_to_target = mean(saccade_to_target, na.rm = T),
            saccade_to_singleton = mean(saccade_to_singleton, na.rm = T),
            saccade_to_nonsingleton = mean(saccade_to_nonsingleton, na.rm = T)/mean(num_nonsingletons, na.rm = T),
            num_nonsingletons = mean(num_nonsingletons, na.rm = T),
            capture = saccade_to_singleton - saccade_to_nonsingleton) %>%
  filter(dType != "Absent")

saccade_direction_quartiles_means_bycondition <- summarySEwithin2(saccade_direction_quartiles_means_byparticipant, measurevar = "capture", betweenvars = "condition", withinvars = c("dType", "quartile"), idvar = "sub") %>%
  select(-ends_with("Normed")) %>%
  left_join(., saccade_latency_quartiles_means_bycondition, by = c("dType", "condition", "quartile"), suffix = c(".prop", ".latency"))

#' 
## ----Vincentized Analysis - Graph, echo=FALSE, warning=FALSE, fig.height=10, fig.width=5, message=FALSE----

vincentized_annotations <- c("***"," "," "," ","**"," "," "," ","***"," "," "," ","***"," "," ","")

vincentizedPlot_singleton <- saccade_direction_quartiles_means_bycondition %>%
  mutate(annotations = vincentized_annotations) %>%
  filter(condition == "Singleton Search") %>%
  ggplot(aes(x = mean_latency, y = capture, fill = dType, linetype = dType, colour = dType)) +
  annotate(geom = "text", label = "Capture", x = 370, y = .3, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "text", label = "Suppression", x = 370, y = -.2, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "segment", x = 365, xend = 365, y = -.03, yend = -.37, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  annotate(geom = "segment", x = 365, xend = 365, y = .03, yend = .57, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  geom_linerange(aes(ymin = capture-se.prop, ymax = capture + se.prop), colour = "black", linetype = "solid", size = .5) +
  geom_errorbarh(aes(xmin = mean_latency-se.latency, xmax = mean_latency+se.latency, height = 0), linetype = "solid", size = .5, colour = "black") +
  geom_line(size = .5) +
  geom_point(aes(shape = dType, fill = dType), size = 3, colour = "black") +
  geom_hline(yintercept = 0, colour = "black", size = .5, linetype = "dotted") +
  geom_text(aes(label = annotations), nudge_y = .07, colour = "black") +
  scale_fill_manual(name = "Trial Type", values = vmacCols, guide = guide_legend(), labels = c("High-reward", "Low-reward")) +
  scale_colour_manual(name = "Trial Type", values = vmacCols, guide = guide_legend(), labels = c("High-reward", "Low-reward")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(-.4,.621)) +
  scale_x_continuous(expand = c(0,0), limits = c(165, 380), breaks = seq(125,375,50)) +
  scale_shape_manual(name = "Trial Type", guide = guide_legend(), values = c(21,22), labels = c("High-reward", "Low-reward")) +
  scale_linetype_manual(name = "Trial Type", values = c("solid", "longdash"), guide = guide_legend(), labels = c("High-reward", "Low-reward")) +
  coord_cartesian(xlim = c(165,360),
                  clip = 'off') +
  ylab("Capture Score") + xlab("Saccade latency (ms)") + labs(tag = "A", title = 'Singleton Search') +
  my_theme + theme(axis.ticks.x = element_line(colour = "black", size = .5))

vincentizedPlot_feature <- 
  saccade_direction_quartiles_means_bycondition %>%
  mutate(annotations = vincentized_annotations) %>%
  filter(condition == "Feature Search") %>%
  mutate(nudge_y = c(rep(.07, 7), -.07), nudge_x = c(0,0,0,-5,0,0,0,5)) %>%
  ggplot(aes(x = mean_latency, y = capture, fill = dType, linetype = dType, colour = dType)) +
  annotate(geom = "text", label = "Capture", x = 370, y = .3, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "text", label = "Suppression", x = 370, y = -.2, angle = 90, vjust = 1, hjust = .5, family = font_rc) +
  annotate(geom = "segment", x = 365, xend = 365, y = -.03, yend = -.37, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  annotate(geom = "segment", x = 365, xend = 365, y = .03, yend = .57, arrow = arrow(type = 'closed', length = unit(6, 'pt'))) +
  geom_linerange(aes(ymin = capture-se.prop, ymax = capture + se.prop), colour = "black", linetype = "solid", size = .5) +
  geom_errorbarh(aes(xmin = mean_latency-se.latency, xmax = mean_latency+se.latency, height = 0), linetype = "solid", size = .5, colour = "black") +
  geom_line(size = .5) +
  geom_point(aes(shape = dType, fill = dType), size = 3, colour = "black") +
  geom_hline(yintercept = 0, colour = "black", size = .5, linetype = "dotted") +
  geom_text(aes(label = annotations, y = capture + nudge_y, x = mean_latency), colour = "black") +
  scale_fill_manual(name = "Trial Type", values = vmacCols, guide = guide_legend()) +
  scale_colour_manual(name = "Trial Type", values = vmacCols, guide = guide_legend()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(-.4,.621)) +
  scale_x_continuous(expand = c(0,0), limits = c(165, 380), breaks = seq(125,375,50)) +
  scale_linetype_manual(name = "Trial Type", values = c("solid", "longdash"), guide = guide_legend()) +
  scale_shape_manual(name = "Trial Type", guide = guide_legend(), values = c(21,22)) +
  coord_cartesian(xlim = c(165,360),
                  clip = 'off') +
  ylab("Capture Score") + xlab("Saccade latency (ms)") + labs(tag = "B", title = 'Feature Search') +
  my_theme + theme(axis.ticks.x = element_line(colour = "black", size = .5))

legend_vincentizedPlot <- get_legend(vincentizedPlot_singleton + theme(legend.position = "bottom",
                                                                       legend.title = element_text(size = 12),
                                                                       legend.text = element_text(size = 12),
                                                                       legend.key.width = unit(24, 'pt')))

vincentizedPlot_ms <- vincentizedPlot_singleton + plot_spacer() + legend_saccade + plot_spacer() + vincentizedPlot_feature + plot_spacer() + plot_layout(nrow = 3, ncol = 2, heights = c(1,.2,1), widths = c(1,.05))

save_plot(here("analysis","figures","vincentized_plot.png"), vincentizedPlot_ms, ncol = 1, nrow = 2, type = "cairo", base_aspect_ratio = 1.1, dpi = 300)

vincentizedPlot_ms

#' 
## ----Vincentized Analysis - Statistics, results="markup"-----------------
vincentized_singleton_high_q1.ttest <- saccade_direction_quartiles_means_byparticipant%>%
  ungroup() %>%
  filter(condition == "Singleton Search", dType == "High", quartile == 1) %>%
  select(capture) %>%
  pull() %>%
  t.test()

vincentized_singleton_high_q1.ttest

vincentized_singleton_high_q1.es <- saccade_direction_quartiles_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "High", quartile == 1) %>%
  select(capture) %>%
  summarise(mean_capture = mean(capture), sd_capture = sd(capture), d_z = abs(mean_capture/sd_capture)) %>%
  pull(d_z)

vincentized_singleton_high_q1.es

vincentized_singleton_low_q1.ttest <- saccade_direction_quartiles_means_byparticipant%>%
  ungroup() %>%
  filter(condition == "Singleton Search", dType == "Low", quartile == 1) %>%
  select(capture) %>%
  pull() %>%
  t.test()

vincentized_singleton_low_q1.ttest

vincentized_singleton_low_q1.es <- saccade_direction_quartiles_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Singleton Search") %>%
  filter(dType == "Low", quartile == 1) %>%
  select(capture) %>%
  summarise(mean_capture = mean(capture), sd_capture = sd(capture), d_z = abs(mean_capture/sd_capture)) %>%
  pull(d_z)

vincentized_singleton_low_q1.es

vincentized_feature_high_q1.ttest <- saccade_direction_quartiles_means_byparticipant%>%
  ungroup() %>%
  filter(condition == "Feature Search", dType == "High", quartile == 1) %>%
  select(capture) %>%
  pull() %>%
  t.test()

vincentized_feature_high_q1.ttest

vincentized_feature_high_q1.es <- saccade_direction_quartiles_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "High", quartile == 1) %>%
  select(capture) %>%
  summarise(mean_capture = mean(capture), sd_capture = sd(capture), d_z = abs(mean_capture/sd_capture)) %>%
  pull(d_z)

vincentized_feature_high_q1.es

vincentized_feature_low_q1.ttest <- saccade_direction_quartiles_means_byparticipant%>%
  ungroup() %>%
  filter(condition == "Feature Search", dType == "Low", quartile == 1) %>%
  select(capture) %>%
  pull() %>%
  t.test()

vincentized_feature_low_q1.ttest

vincentized_feature_low_q1.es <- saccade_direction_quartiles_means_byparticipant %>%
  ungroup() %>%
  filter(condition == "Feature Search") %>%
  filter(dType == "Low", quartile == 1) %>%
  select(capture) %>%
  summarise(mean_capture = mean(capture), sd_capture = sd(capture), d_z = abs(mean_capture/sd_capture)) %>%
  pull(d_z)

vincentized_feature_low_q1.es

#' 
#' 
