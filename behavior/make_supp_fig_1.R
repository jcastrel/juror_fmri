# supplementary figure 1 from the paper
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)
library(broom)

source('ggplot_setup.R')

load('data/stan_postprocess_2v_t.rdata')
effects <- effects %>% filter(group =='mri'| group=='mturk')
dat <- dat %>% filter(group=='mri'| group=='mturk')

############### Panel A: Baseline effects by scenario ##################################
# get scenario effects
se <- effects %>% filter(variable == 'gamma', evidence == 'baseline', outcome == 'rating') %>% 
  select(scenario, mean, group) %>%
  mutate(outcome='rating')

sc_ranked <- as.factor(c(27, 6, 12, 29, 13, 14, 1, 24, 2, 22, 25,
                         3, 8, 9, 4, 18, 33, 15, 7, 19, 28, 32, 5, 11, 26, 17,
                         20, 30, 31, 21, 10, 16, 23))

crime_type <- rep('state', 33)
crime_type[c(1, 14, 28)] <- 'federal'
crime_type <- as.factor(crime_type)

seriousness <- data.frame(seriousness=as.factor(c(1:33)), 
                          scenario=sc_ranked,
                          crime_type=crime_type)

se2_tmp  <- effects %>% filter(variable == 'gamma',
                               evidence == 'baseline',
                               outcome == 'rating') %>% 
  select(scenario, mean, X2.5., X50., X97.5., group) %>%
  mutate(outcome='rating')

se2 <- merge(se2_tmp, seriousness) %>% filter(outcome=='rating') %>% 
  group_by(scenario, seriousness, crime_type)

panel_A <- ggplot(data=se2) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=seriousness, y=mean, ymin=X2.5., ymax=X97.5., color=group), size=1.,
                  position=position_dodge(width = 0.75)) + 
  group_color_scale +
  scale_x_discrete(name='Scenario (rank-ordered by severity)',
                   breaks=c(1:33),
                   labels=seriousness$scenario) +
  coord_cartesian(ylim=c(-10,50)) +
  #labs(title="A") +
  ylab("Case Strength (points)") +
  xlab("Scenario Effects") +
  th + 
  theme(legend.position=c(0.9, 0.8),
        axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  ) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"))


############### Panel B: Effect sizes for case strength ##################################
# get evidence effects
fe <- effects %>% filter(outcome=='rating',variable == 'mu', evidence != 'baseline') %>%
  select(mean, evidence, X2.5., X97.5., group) %>%
  mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                            "physicalNon-DNA", 
                                            "witnessYes Witness", 
                                            "historyRelated", 
                                            "historyUnrelated")))

panel_B <- ggplot(data=fe) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5., color=group), size=1.,
                  position=position_dodge(width = 0.75)) + 
  evidence_x_axis +
  group_color_scale +
  coord_cartesian(ylim=c(-10,50)) +
  #labs(title="B") +
  ylab("Case Strength (points)") +
  xlab("Evidence Effects") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(legend.position=c(0.9, 0.8),
        axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  ) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"))




############### Combine into a single figure ##################################

combo_plot <- ggarrange(panel_A,panel_B,
                        nrow = 2,labels = c("A","B"), font.label = list(size = 20))

ggsave('figs/supp_fig_1.pdf', plot=combo_plot, width=9, height=9, units='in', useDingbats=FALSE)
