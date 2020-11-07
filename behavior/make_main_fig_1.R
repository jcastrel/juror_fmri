# make figure showing: (1.) task schematic, (2.) evidence & scenario effects plot, (3.)correlation plot, & (4.) PCA plot
library(tidyverse)
library(grid)
library(gridExtra)
library(rstan)
library(magick)
library(gtable)
library(gridBase)
#library(factoextra)
#library(ggpubr)

source('ggplot_setup.R')
load('data/stan_postprocess_2v_t.rdata')
effects <- effects %>% filter(group =='mri')
dat <- dat %>% filter(group=='mri')

####################
#PANEL A
# task mockup from svg
task_mock <- image_read_pdf('figs/task_mockup.pdf') #8 x 5 fig
task_mock_trimmed <- image_trim(task_mock)
panel_A <- ggplot() +
  #labs(title="A",size= 9 / .pt) +
  annotation_custom(rasterGrob(task_mock_trimmed,
                               interpolate=TRUE)) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold")) + 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=0)) +
  theme(panel.background = element_rect(colour = "white", fill=NA, size=0))
theme(axis.line=element_blank())+
  theme(plot.margin=unit(c(25.5, 5.5, 5.5, 25.5),"points"))

ggsave('figs/main_fig_1_A.pdf', plot=panel_A, width=1.375, height=2.75, units='in')

############### Panel B1: Effect sizes for case strength ##################################
# get evidence effects
fe <- effects %>% filter(outcome=='rating',variable == 'mu', evidence != 'baseline') %>%
  select(mean, evidence, X2.5., X97.5.) %>%
  mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                            "physicalNon-DNA", 
                                            "witnessYes Witness", 
                                            "historyRelated", 
                                            "historyUnrelated")))

plt_b1 <- ggplot(data=fe) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5.), size=.75,
                  position=position_dodge(width = 0.75)) + 
  evidence_x_axis +
  group_color_scale +
  coord_cartesian(ylim=c(-10,50)) +
  ggtitle("Evidence Effects") +
  ylab("Case Strength (points)") +
  xlab(NULL) +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  geom_vline(xintercept=5.5, colour='grey') +
  th + 
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="plain")) +
  theme(plot.margin=unit(c(25.5, 5.5, 0, 25.5),"points")) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

panel_B1 <- ggplot_gtable(ggplot_build(plt_b1))
panel_B1$layout$clip[panel_B1$layout$name == "panel"] <- "off"
grid::grid.draw(panel_B1)


ggsave('figs/main_fig_1_B1.pdf', plot=panel_B1,
       width=10, height=5, units='in', useDingbats=FALSE)

############### Panel B2: Crime effects ############################
# get scenario effects
se <- effects %>% filter(variable == 'gamma', evidence == 'baseline', outcome=='rating') %>% 
  select(scenario, mean, X2.5., X97.5.) %>%
  mutate(outcome='rating',
         scenario = factor(scenario))

#quick ordering
se$rank <- rank(se$mean)

plt_b2 <- ggplot(data=se) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=reorder(scenario, rank), y=mean, ymin=X2.5., ymax=X97.5.), size=.75,
                  position=position_dodge(width = 0.75)) +
  expand_limits(x= c(-1, length(levels(se$rank)) + 2))+
  group_color_scale +
  coord_cartesian(ylim=c(-10,50)) +
  ggtitle("Scenario Effects") +
  ylab("Case Strength (points)") +
  xlab("Crime Type") +
  th + 
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="plain")) +
  theme(plot.margin=unit(c(25.5, 25.5, 0, -10),"points")) +
  theme(
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()) +
  geom_segment(aes(x = 1, y = -5, xend=32, yend=-5),
               arrow = arrow(length = unit(0.5, "cm"),ends = 'both')) +
  annotate("text", x = 6, y = -9, label = "victimless", size=rel(5))+
  annotate("text", x = 29, y = -9, label = "homicide", size=rel(5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = 15))


panel_B2 <- ggplot_gtable(ggplot_build(plt_b2))
panel_B2$layout$clip[panel_B2$layout$name == "panel"] <- "off"
grid::grid.draw(panel_B2)


ggsave('figs/main_fig_1_B2.pdf', plot=panel_B2,
       width=10, height=5, units='in', useDingbats=FALSE)

#####################
#combine 1B1 and 1B2

panel_B <- ggarrange(plt_b1,plt_b2,
                          ncol = 2,
                          font.label = list(size = 20),
                          align='h',
                          widths = c(1.5,1))

ggsave('figs/main_fig_1_B.pdf', plot=panel_B,
       width=12, height=5, units='in', useDingbats=FALSE)

############### Panels C & D: PCA  ##################################
#fmri sample
#read data
fmri_data <- read.csv(file="data/scenario_effects_fmri_sample.csv",
                      header=TRUE, sep = ",",
                      na.strings=c("","NA"))

#correlation plot
plt_c <- ggplot(data = fmri_data,
            mapping = aes(x = case_strength_mean,y = punishment_mean)) +
  geom_point(color='black',size=2) +
  geom_smooth(method='lm', formula= y~x, colour = "black") +
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 85)) +
  xlab('Case Strength') +
  ylab('Punishment')

panel_C <- plt_c + th +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"))+
  theme(plot.margin=unit(c(5, 5.5, 25.5, 25.5),"points"))


#pca & plot
fmri_data.pca <- prcomp(fmri_data[,c(2:3)],
                        center = TRUE,
                        scale. = TRUE)
#summary
summary(fmri_data.pca)
#loadings
fmri_data.pca$rotation
#scores
fmri_data.pca$x

pct_var_exp<- get_eig(fmri_data.pca)
pct_var_exp <- cbind(PC = rownames(pct_var_exp), pct_var_exp)
rownames(pct_var_exp) <- 1:nrow(pct_var_exp)
pct_var_exp$PC_name[pct_var_exp$PC == "Dim.1"] <- "1"
pct_var_exp$PC_name[pct_var_exp$PC == "Dim.2"] <- "2"
pct_var_exp$PC<-pct_var_exp$PC_name

h <- ggplot(pct_var_exp, aes(x = PC, y = variance.percent)) +
  geom_point(stat = "identity", size=5)

# h <- fviz_eig(fmri_data.pca, geom="line", addlabels = FALSE, ylim = c(0, 100),
#               linecolor = c('black'),
#               xlab=c('PC')) +
#   labs(title=" ")

panel_D <- h + th + ylab('Variance Explained (%) ') +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"),
        plot.background = element_rect(fill = 'white',color='white'))+
  theme(plot.margin=unit(c(5.5, 25.5, 5.5, 25.5),"points"))



############### Combine into a single figure ##################################
# make a list of panels
combo_plot <- ggarrange(panel_A,
                        ggarrange(panel_B, 
                                  ggarrange(panel_C,panel_D, widths=c(1,1),ncol=2, labels=c('C','D'), font.label = list(size = 20), align = 'hv'),
                                  heights = c(2,1.25), nrow = 2,labels=c("B"), font.label = list(size = 20)),
                        ncol = 2,labels = "A", font.label = list(size = 20), widths=c(1,2))

#combo_plot<-combo_plot+theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5), "points"))

ggsave('figs/main_fig_1.pdf', plot=combo_plot,
       width=14, height=8,
       units='in', useDingbats=FALSE)