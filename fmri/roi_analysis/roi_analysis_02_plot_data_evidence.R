## Plot evidence-wise fMRI parameter estimates

#packages
library(dplyr)
library(lme4)
library(jtools)
library(ggplot2)
library(ggpubr)
library(lmerTest)

#set seed (for reproducible effects)
set.seed(1000)

#theme
th <- theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(face = 'bold', colour = 'black', size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white',color = 'white'),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(colour = "black", size = 16),
        axis.title.y = element_text(colour = "black", size = 16),
        axis.text=element_text(size = 16),
        axis.text.x = element_text(colour = "black", size = 16),
        axis.text.y = element_text(colour = "black", size = 16))

#open data
data <- read.csv(file="./subject_evidence_mean_zstat.csv",
                 header=TRUE,
                 sep = ",",
                 na.strings=c("","NA"))

data <- within(data, {
  subjectID <- factor(subjectID)
  scenario <- factor(scenario)
  })

#Evidence fMRI Significant Cluster Parameter Estimates

#Case Strength
fmri_cs <- lmer(zstat ~ weight_sum + (1 | subjectID), data = data)
#summary
summ(fmri_cs, scale = TRUE, confint = TRUE, digits = 4)

fmri_cs_plot <- effect_plot(fmri_cs, pred = weight_sum, interval = TRUE, main.title = '',
            x.label = "Case Strength Weight Sum", y.label = "Mean Evidence P.E.") + th


#Word Count - fMRI
fmri_wc <- lmerTest::lmer(zstat ~ wc_sum + (1 | subjectID), data = data)
#summary
summ(fmri_wc, scale = TRUE, confint = TRUE, digits = 4)
anova(fmri_wc)

fmri_wc_plot <- effect_plot(fmri_wc, pred = wc_sum, interval = TRUE, main.title = '',
                            x.label = "Sum Evidence Text Word Count",
                            y.label = "Mean fMRI parameter estimate") + th +
  theme(plot.margin=unit(c(25.5, 25.5, 5.5, 25.5),"points"))


#Reading Level - fMRI
fmri_fk_grade <- lmerTest::lmer(zstat ~ fk_grade_avg + (1 | subjectID), data = data)
#summary
summ(fmri_fk_grade, scale = TRUE, confint = TRUE, digits = 4)
anova(fmri_fk_grade)

fmri_fk_grade_plot <- effect_plot(fmri_fk_grade, pred = fk_grade_avg, interval = TRUE, main.title = '',
                            x.label = "Sum Evidence Text Reading Grade Level",
                            y.label = "Mean fMRI parameter estimate") + th +
  theme(plot.margin=unit(c(25.5, 25.5, 5.5, 25.5),"points"))


panels <- ggarrange(fmri_wc_plot,fmri_fk_grade_plot, ncol = 1, nrow=2,
                    labels=c('A','B'),font.label = list(size = 20), align='v',widths = c(1,1))
ggsave('plots/supp_fig_8.pdf', plot=panels,
       width=8, height=8, units='in', useDingbats=FALSE)


#################################################################################

#Word Count - Behavior (Case Strength)

#drop duplicates (just keep one of each unique word count sum/evidence combo)
#data_wc_cs <- distinct(data, wc_sum, .keep_all = TRUE)
# distinct
data_wc_cs <- data %>% 
  distinct(scenario, physical, witness, history, .keep_all = T)

#cor.test
cor.test(data_wc_cs$wc_sum,data_wc_cs$weight_sum)
#lm
behavior_wc_cs <- lm(weight_sum~wc_sum,data=data_wc_cs)
#behavior_wc_cs_lme <- lmer(weight_sum ~ wc_sum + (1 | scenario), data = data_wc_cs)
summ(behavior_wc_cs, scale = TRUE, confint = TRUE, digits = 4)
#plot
behav_cs_plot <- effect_plot(behavior_wc_cs, pred = wc_sum, plot.points = TRUE, interval = TRUE, main.title = '',
                            x.label = "Evidence Word Count Sum", y.label = "Case Strength Weight Sum") + th


#Reading-Grade Level - Behavior
#cor.test
cor.test(data_wc_cs$fk_grade_sum,data_wc_cs$weight_sum)
#lm
behavior_fk_grade_cs <- lm(weight_sum~fk_grade_sum,data=data_wc_cs)
#behavior_wc_cs_lme <- lmer(weight_sum ~ wc_sum + (1 | scenario), data = data_wc_cs)
summ(behavior_fk_grade_cs, scale = TRUE, confint = TRUE, digits = 4)
#plot
behav_fk_grade_plot <- effect_plot(behavior_fk_grade_cs, pred = fk_grade_sum, plot.points = TRUE, interval = TRUE, main.title = '',
                             x.label = "Evidence Reading Grade Level Sum", y.label = "Case Strength Weight Sum") + th

#Word Count - fMRI w/covariate
fmri_wc2 <- lmer(zstat ~ wc_sum + weight_sum + (1 | subjectID), data = data)
#summary
summ(fmri_wc2, scale = TRUE, confint = TRUE, digits = 4)

fmri_wc2_plot <- effect_plot(fmri_wc2, pred = weight_sum, interval = TRUE, main.title = '',
                            x.label = "Case Strength Weight Sum", y.label = "Mean Evidence P.E.") + th



##############################################################################

#what if we drop all instances where there was no evidence (no physical, no witness, no history)
data_evidence_yes <- data_wc_cs %>%
  filter(physical != "noPhys") %>%
  filter(witness != "noWitness") %>%
  filter(history != "noPrior")

#cor.test
cor.test(data_evidence_yes$wc_sum,data_evidence_yes$weight_sum)
#lm
behavior_wc_cs_ev_yes <- lm(weight_sum~wc_sum,data=data_evidence_yes)
#behavior_wc_cs_lme <- lmer(weight_sum ~ wc_sum + (1 | scenario), data = data_wc_cs)

summ(behavior_wc_cs_ev_yes, scale = TRUE, confint = TRUE, digits = 4)

effect_plot(behavior_wc_cs_ev_yes, pred = wc_sum, plot.points = TRUE, interval = TRUE, main.title = '',
            x.label = "Evidence Word Count Sum", y.label = "Case Strength Weight Sum") + th

data_fmri_evidence_yes <- data %>%
  filter(physical != "noPhys") %>%
  filter(witness != "noWitness") %>%
  filter(history != "noPrior")
# #Case Strength - evidence presence

fmri_cs_ev_yes <- lmer(zstat ~ weight_sum + (1 | subjectID), data = data_fmri_evidence_yes)
#summary
summ(fmri_cs_ev_yes, scale = TRUE, confint = TRUE, digits = 4)

fmri_cs_ev_yes_plot <- effect_plot(fmri_cs_ev_yes, pred = weight_sum, interval = TRUE, main.title = '',
                            x.label = "Case Strength Weight Sum", y.label = "Mean Evidence P.E.") + th

# #Word Count - fMRI
fmri_wc_ev_yes <- lmer(zstat ~ wc_sum + (1 | subjectID), data = data_fmri_evidence_yes)
#summary
summ(fmri_wc_ev_yes, scale = TRUE, confint = TRUE, digits = 4)
