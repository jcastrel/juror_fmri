## Plot scenario-wise fMRI parameter estimates

#packages
library(dplyr)
library(lme4)
library(jtools)
library(ggplot2)
library(ggpubr)
library(lmerTest)
library(magick)
library(gridBase)


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
data <- read.csv(file="./subject_scenario_mean_zstat.csv",
                 header=TRUE,
                 sep = ",",
                 na.strings=c("","NA"))

data <- within(data, {
  victim_type <- factor(victim_type,
                        levels = 0:2,
                        labels = c('Victimless',
                                   'Loss of Property',
                                   'Injury or Loss of Life'))
  subjectID <- factor(subjectID)
  })

#Scenario fMRI Significant Cluster Parameter Estimates

#PC1
fmri_pc1 <- lmer(zstat ~ PC1 + (1 | subjectID), data = data)
#summary
summ(fmri_pc1, scale = TRUE, confint = TRUE, digits = 4)

fmri_pc1_plot <- effect_plot(fmri_pc1, pred = PC1, interval = TRUE, main.title = '',
            x.label = "PC1", y.label = "Mean Scenario P.E.") + th

#PC2
fmri_pc2 <- lmer(zstat ~ PC2 + (1 | subjectID), data = data)
#summary
summ(fmri_pc2, scale = TRUE, confint = TRUE, digits = 4)

fmri_pc2_plot <- effect_plot(fmri_pc2, pred = PC2, interval = TRUE, main.title = '',
                             x.label = "PC2", y.label = "Mean Scenario P.E.") + th


#Victim Type - fMRI
fmri_victim_type <- lmerTest::lmer(zstat ~ factor(victim_type) + (1 | subjectID), data = data)
#summary
summ(fmri_victim_type, scale = TRUE, confint = TRUE, digits = 4)
anova(fmri_victim_type)

#grab means and s.e.m
victim_type_data <- data %>%
  group_by(victim_type) %>%
  summarize(victim_type_mean_pe = mean(zstat, na.rm = TRUE),
            victim_type_sd_pe = sd(zstat),
            victim_type_count_pe = n(),
            victim_type_sem_pe = victim_type_sd_pe / sqrt(victim_type_count_pe))

#plot raw data
fmri_victim_type_plot <- ggplot(victim_type_data, aes(x=victim_type, y=victim_type_mean_pe)) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(ymin=victim_type_mean_pe-victim_type_sem_pe,
                      ymax=victim_type_mean_pe+victim_type_sem_pe),size=.75) +
  theme(text=element_text(family="Helvetica")) + th +
  labs(y = "Mean fMRI parameter estimate", x = "Crime Type") +
  theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5),"points"))


#Category/seriousness - fMRI
fmri_seriousness <- lmerTest::lmer(zstat ~ factor(category) + (1 | subjectID), data = data)
#summary
summ(fmri_seriousness, scale = TRUE, confint = TRUE, digits = 4)
anova(fmri_seriousness)

#grab means and s.e.m
category_data <- data %>%
  group_by(category) %>%
  summarize(category_mean_pe = mean(zstat, na.rm = TRUE),
            category_sd_pe = sd(zstat),
            category_count_pe = n(),
            category_sem_pe = category_sd_pe / sqrt(category_count_pe))

#plot raw data
fmri_category_type_plot <- ggplot(category_data, aes(x=category, y=category_mean_pe)) +
  geom_hline(yintercept=0, color = "grey") +
  geom_pointrange(aes(ymin=category_mean_pe-category_sem_pe,
                      ymax=category_mean_pe+category_sem_pe),size=0.75) + th +
  labs(y = "Mean fMRI parameter estimate", x = "Crime Type") +
  theme(plot.margin=unit(c(25.5, 25.5, 5.5, 25.5),"points"))


#Victim Type - Behavior
#grab behavior data
behavior <- read.csv(file="../../behavior/data/scenario_classification.csv",
                 header=TRUE,
                 sep = ",",
                 na.strings=c("","NA"))

behavior <- within(behavior, {
  victim_type <- factor(JRL,
                        levels = 0:2,
                        labels = c('Victimless',
                                   'Loss of Property',
                                   'Injury or Loss of Life'))
  category <- factor(category)
})

#grab pca data
pca <- read.csv(file="../../behavior/pca_loadings-fmri.csv",
                     header=TRUE,
                     sep = ",",
                     na.strings=c("","NA"))
pca <- within(pca, {
  scenario <- factor(X)
})

#merge dataframes
behavior_data <- merge(behavior, pca, by='scenario')

victim_type_behavior <- behavior_data %>%
  group_by(victim_type) %>%
  summarize(victim_type_mean_PC1 = mean(PC1, na.rm = TRUE),
            victim_type_sd_PC1 = sd(PC1),
            victim_type_count_PC1 = n(),
            victim_type_sem_PC1 = victim_type_sd_PC1 / sqrt(victim_type_count_PC1),
            
            victim_type_mean_PC2 = mean(PC2, na.rm = TRUE),
            victim_type_sd_PC2 = sd(PC2),
            victim_type_count_PC2 = n(),
            victim_type_sem_PC2 = victim_type_sd_PC2 / sqrt(victim_type_count_PC2))

#PC1
victim_type_behavior_PC1 <- lm(PC1 ~ factor(victim_type), data = behavior_data)
summary(victim_type_behavior_PC1)
summ(victim_type_behavior_PC1, scale = TRUE, confint = TRUE, digits = 4)

anova(victim_type_behavior_PC1)

behavior_victim_type_plot_PC1 <- ggplot(victim_type_behavior, aes(x=victim_type, y=victim_type_mean_PC1)) +
  geom_hline(yintercept=0, color = "grey") +
  geom_pointrange(aes(ymin=victim_type_mean_PC1-victim_type_sem_PC1,
                      ymax=victim_type_mean_PC1+victim_type_sem_PC1),size=0.75) + th +
  labs(y = "Mean PC1 Loading", x = "Crime Type") +
  theme(plot.margin=unit(c(25.5, 25.5, 5.5, 25.5),"points"))
################################################################
# social maps for panel
social_maps <- image_read_pdf('../decoding_analysis/social_maps/social_topic_maps.pdf') #8 x 5 fig
social_maps_trimmed <- image_trim(social_maps)
panel_A <- ggplot() +
  annotation_custom(grid::rasterGrob(social_maps_trimmed,interpolate=TRUE)) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold")) + 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=0)) +
  theme(panel.background = element_rect(colour = "white", fill=NA, size=0))
theme(axis.line=element_blank())+
  theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5),"points"))


################################################################
#merge plots
combo_plot <- ggarrange(panel_A,
                        ggarrange(behavior_victim_type_plot_PC1, 
                                  ggarrange(fmri_victim_type_plot,
                                            labels=c('C'),font.label = list(size = 20), align = 'v'),
                                  heights = c(1,1), nrow = 2,labels=c("B"), font.label = list(size = 20)),
                        ncol = 2,labels = "A", font.label = list(size = 20), widths=c(1,1))

ggsave('plots/main_fig_4.pdf', plot=combo_plot,
       width=16, height=8, units='in', useDingbats=FALSE)

#################################################################

#PC2
victim_type_behavior_PC2 <- lm(PC2 ~ factor(victim_type),
                                data = behavior_data)
summary(victim_type_behavior_PC2)
anova(victim_type_behavior_stat)

ggplot(victim_type_behavior, aes(x=victim_type, y=victim_type_mean_PC2)) +
  geom_hline(yintercept=0, color = "grey", size=0.75) +
  geom_pointrange(aes(ymin=victim_type_mean_PC2-victim_type_sem_PC2,
                      ymax=victim_type_mean_PC2+victim_type_sem_PC2),size=0.75) + th +
  labs(y = "Mean PC2 Loading", x = "Crime Type")



#Category/seriousness - Behavior
category_behavior <- data %>%
  group_by(category) %>%
  summarize(category_mean_PC1 = mean(PC1, na.rm = TRUE),
            category_sd_PC1 = sd(PC1),
            category_count_PC1 = n(),
            category_sem_PC1 = category_sd_PC1 / sqrt(category_count_PC1),
            
            category_mean_PC2 = mean(PC2, na.rm = TRUE),
            category_sd_PC2 = sd(PC2),
            category_count_PC2 = n(),
            category_sem_PC2 = category_sd_PC2 / sqrt(category_count_PC2))

#PC1
#PC1
category_type_behavior_PC1 <- lm(PC1 ~ factor(category), data = behavior_data)
summary(category_type_behavior_PC1)
summ(category_type_behavior_PC1, scale = TRUE, confint = TRUE, digits = 4)

anova(category_type_behavior_PC1)

behavior_category_type_plot_PC1 <- ggplot(category_behavior, aes(x=category, y=category_mean_PC1)) +
  geom_hline(yintercept=0, color = "grey") +
  geom_pointrange(aes(ymin=category_mean_PC1-category_sem_PC1,
                      ymax=category_mean_PC1+category_sem_PC1),size=0.75) + th +
  labs(y = "Mean PC1 Loading", x = "Crime Type") +
  theme(plot.margin=unit(c(25.5, 25.5, 5.5, 25.5),"points"))

################################################################
#merge plots
panels <- ggarrange(behavior_category_type_plot_PC1,fmri_category_type_plot, ncol = 1, nrow=2,
                    labels=c('A','B'),font.label = list(size = 20), align='v',widths = c(1,1))
ggsave('plots/supp_fig_9.pdf', plot=panels,
       width=8, height=8, units='in', useDingbats=FALSE)

#################################################################


#PC2
ggplot(category_behavior, aes(x=category, y=category_mean_PC2)) +
  geom_hline(yintercept=0, color = "grey") +
  geom_pointrange(aes(ymin=category_mean_PC2-category_sem_PC2,
                      ymax=category_mean_PC2+category_sem_PC2),size=0.75) + th +
  labs(y = "Mean PC2 Loading", x = "Crime Type")




