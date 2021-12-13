# make figure 2
library(tidyverse)
library(grid)
library(gridExtra)
library(magick)
library(gtable)
library(gridBase)
library(ggpubr)

source('ggplot_setup.R')


############### Panel A: Neurosynth regression illustration ############################

illustration <- image_read_pdf('neurosynth_illustration/Fig_4a.pdf')
illustration_trimmed <- image_trim(illustration)
panel_A <- ggplot() +
  annotation_custom(rasterGrob(illustration_trimmed,interpolate=TRUE)) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"),
        panel.border = element_rect(colour = "white", fill=NA, size=0),
        panel.background = element_rect(colour = "white", fill=NA, size=0),
        axis.line=element_blank(),
        plot.margin=unit(c(25.5, 25.5, 25.5, 25.5),"points"))

############### Panel B: Dendrogram ############################

dendrogram <- image_read_pdf('dendrograms/Fig_4b.pdf')
dendrogram_trimmed <- image_trim(dendrogram)

panel_B <- ggplot() +
  annotation_custom(rasterGrob(dendrogram_trimmed,interpolate=TRUE)) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"),
        panel.border = element_rect(colour = "white", fill=NA, size=0),
        panel.background = element_rect(colour = "white", fill=NA, size=0),
        axis.line=element_blank(),
        plot.margin=unit(c(0, 25.5, 25.5, 25.5),"points"))


############### Combine into a single figure ##################################
# make a list of panels
combo_plot <- ggarrange(panel_A, panel_B, labels=c('A','B'),font.label = list(size = 20),
                                  align = 'v', widths = c(1,1), ncol = 1, nrow = 2, heights = c(1,1))

#combo_plot<-combo_plot+theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5), "points"))

ggsave('main_fig_4.pdf', plot=combo_plot,
       width=12, height=14,
       units='in', useDingbats=FALSE)
