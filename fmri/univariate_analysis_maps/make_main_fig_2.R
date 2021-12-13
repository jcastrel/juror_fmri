# make figure 2
library(tidyverse)
library(grid)
library(gridExtra)
library(magick)
library(gtable)
library(gridBase)
library(ggpubr)

source('ggplot_setup.R')


############### Panel A: fMRI Maps ############################

fmri_maps <- image_read_pdf('figs/Fig_2a.pdf')
fmri_maps_trimmed <- image_trim(fmri_maps)
panel_A <- ggplot() +
  annotation_custom(rasterGrob(fmri_maps_trimmed,interpolate=TRUE)) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"),
        panel.border = element_rect(colour = "white", fill=NA, size=0),
        panel.background = element_rect(colour = "white", fill=NA, size=0),
        axis.line=element_blank(),
        plot.margin=unit(c(25.5, 25.5, 0, 25.5),"points"))

# ############### Panel B: Neurosynth Histogram ############################
# ############### Panel B: Neurosynth Regression Illustration ############################
#
# illustration <- image_read_pdf('figs/Fig_2b.pdf')
# illustration_trimmed <- image_trim(illustration)
#
# # topic_histogram <- image_read_pdf('../decoding_analysis/plots/topic_histograms/Fig_2b.pdf')
# # topic_histogram_trimmed <- image_trim(topic_histogram)
# panel_B <- ggplot() +
#   annotation_custom(rasterGrob(illustration_trimmed,interpolate=TRUE)) +
#   theme(text=element_text(family="Helvetica"),
#         plot.title = element_text(family="Helvetica",face="bold"),
#         panel.border = element_rect(colour = "white", fill=NA, size=0),
#         panel.background = element_rect(colour = "white", fill=NA, size=0),
#         axis.line=element_blank(),
#         plot.margin=unit(c(0, 25.5, 25.5, 25.5),"points"))
#
#
# ############### Combine into a single figure ##################################
# # make a list of panels
# combo_plot <- ggarrange(panel_A, panel_B, labels=c('A','B'),font.label = list(size = 20),
#                                   align = 'v', widths = c(1,1), ncol = 1, nrow = 2, heights = c(1,1))

#combo_plot<-combo_plot+theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5), "points"))

ggsave('figs/main_fig_2.pdf', plot=panel_A,
       width=12, height=6,
       units='in', useDingbats=FALSE)
