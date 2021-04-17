# supplementary figure showing correlation between case strength & punishment
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)
library(factoextra)
library(ggarrange)

source('ggplot_setup.R')
load('data/stan_postprocess_2v_t.rdata')
#effects <- effects %>% filter(group =='mri')
#dat <- dat %>% filter(group |'mri')


############### Panel A: Punishment and case strength effect correlations ##################################
load('data/stan_postprocess_2v_t.rdata')

effects <- effects %>% filter(group =='mri' | group == 'mturk')

panel_A <- ggplot(data=(effects %>% filter(grepl('Omega', variable), evidence=='baseline'))) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                  position=position_dodge(width = 0.5)) + 
  xlab('') + ylab('\nCase Strength /\nPunishmnet Correlation') +
  group_color_scale +
  evidence_plus_baseline_x_axis +
  #labs(title="A", size=rel(3)) +
  th + 
  theme(
    legend.position=c(0.8, 0.8)
  ) + theme(text=element_text(family="Helvetica"),
            plot.title = element_text(family="Helvetica",face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

############### Panels B & C: PCA  ##################################
#run PCA on weights for case strength and punishment
#fmri sample
#read data
fmri_data <- read.csv(file="data/scenario_effects_fmri_sample.csv",
                      header=TRUE, sep = ",",
                      na.strings=c("","NA"))

# #correlation plot
# plt_b <- ggplot(data = fmri_data,
#             mapping = aes(x = case_strength_mean,y = punishment_mean)) +
#   geom_point(color='black',size=2) +
#   geom_smooth(method='lm', formula= y~x, colour = "black") +
#   scale_x_continuous(limits = c(0, 25)) +
#   scale_y_continuous(limits = c(0, 85)) +
#   xlab('Case Strength') +
#   ylab('Punishment')
# 
# panel_B <- plt_b + th +
#   theme(text=element_text(family="Helvetica"),
#         plot.title = element_text(family="Helvetica",face="bold"))+
#   theme(plot.margin=unit(c(5, 5.5, 25.5, 25.5),"points"))

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

panel_B <- h + th + ylab('Variance Explained (%) ') +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"),
        plot.background = element_rect(fill = 'white',color='white'))+
  theme(plot.margin=unit(c(5.5, 25.5, 5.5, 25.5),"points")) +
  scale_y_continuous(limits = c(0, 100))

####################

#run PCA on weights for case strength and punishment
#mturk sample
#read data
mturk_data <- read.csv(file="data/scenario_effects_mturk_sample.csv",
                       header=TRUE, sep = ",",
                       na.strings=c("","NA"))
#pca & plot
mturk_data.pca <- prcomp(mturk_data[,c(2:3)],
                         center = TRUE,
                         scale. = TRUE)

#summary
summary(mturk_data.pca)
#loadings
mturk_data.pca$rotation
#scores
mturk_data.pca$x

pct_var_exp<- get_eig(mturk_data.pca)
pct_var_exp <- cbind(PC = rownames(pct_var_exp), pct_var_exp)
rownames(pct_var_exp) <- 1:nrow(pct_var_exp)
pct_var_exp$PC_name[pct_var_exp$PC == "Dim.1"] <- "1"
pct_var_exp$PC_name[pct_var_exp$PC == "Dim.2"] <- "2"
pct_var_exp$PC<-pct_var_exp$PC_name

j <- ggplot(pct_var_exp, aes(x = PC, y = variance.percent)) +
  geom_point(stat = "identity", size=5)

panel_C <- j + th + ylab('Variance Explained (%) ') +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"),
        plot.background = element_rect(fill = 'white',color='white'))+
  theme(plot.margin=unit(c(5.5, 25.5, 5.5, 25.5),"points")) +
  scale_y_continuous(limits = c(0, 100))


############### Combine into a single figure ##################################

combo_plot <- ggarrange(panel_A,panel_B, panel_C,
                        ncol = 3, widths = c(1,1,1),align = 'hv',
                        labels = c("A","B","C"), font.label = list(size = 20))


# save to disk
ggsave('figs/supp_fig_3.pdf', plot=combo_plot, width=13, height=6, units='in', useDingbats=FALSE)
