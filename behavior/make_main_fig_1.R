# make figure showing: (1.) task schematic, (2.) evidence & scenario effects plot, (3.)correlation plot, & (4.) PCA plot
library(tidyverse)
library(grid)
library(gridExtra)
library(rstan)
library(magick)
library(gtable)
library(gridBase)
library(ggpubr)
library(rjson)
library(ggrepel)

source('ggplot_setup.R')
load('data/stan_postprocess_2v_t.rdata')
effects <- effects %>% filter(group =='mri')
dat <- dat %>% filter(group=='mri')

####################
#PANEL A
# task mockup from svg
task_mock <- image_read_pdf('figs/task_mockup2.pdf') #8 x 5 fig
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

############### Panel B: Example scenario cs accumulation ##################################

#get the scenario with the maximum mean rating (scenario 14)
example_scenario <- effects %>% filter(variable == 'gamma', evidence == 'baseline', outcome=='rating') %>% 
  select(scenario, mean, X2.5., X97.5.) %>%
  mutate(outcome='rating',
         scenario = factor(scenario))
example_scenario <- example_scenario[example_scenario$mean == max(example_scenario$mean),]

#get evidence pieces (previous related, witness, DNA)
example_evidence <- effects %>% filter(outcome=='rating',variable == 'mu', evidence != 'baseline') %>%
  select(mean, evidence, X2.5., X97.5.) %>%
  mutate(evidence=factor(evidence, levels=c("physicalDNA",
                                            "witnessYes Witness", 
                                            "historyRelated")))
example_evidence <- example_evidence %>%
  drop_na()


#get scenario and evidence text
json_file <- 'data/scenarios.json'
json_data <- fromJSON(file=json_file)
scnum <- example_scenario$scenario[[1]]
sc_text <- json_data[[scnum]]$vars$base$Base
ev1_text <- json_data[[scnum]][["vars"]][["Physical Evidence"]][["DNA"]]
ev2_text <- json_data[[scnum]][["vars"]][["Witness"]][["isWitness"]]
ev3_text <- json_data[[scnum]][["vars"]][["Criminal History"]][["relatedPrior"]]

#organize example
event <- c(1:4)
event_name <- c('Scenario', 'Evidence 1', 'Evidence 2', 'Evidence 3')
text <- c(sc_text, ev1_text, ev2_text, ev3_text)
mean <- c(example_scenario$mean, example_evidence$mean)
lb <- c(example_scenario$X2.5., example_evidence$X2.5.)
ub <- c(example_scenario$X97.5., example_evidence$X97.5.)

example <- data.frame(event_name, event, mean, lb, ub, text)
example$text_wrapped = str_wrap(example$text, width = 40)

example <- within(example, cs_sum <- cumsum(mean))
example <- within(example, cs_sum_lb <- cumsum(lb))
example <- within(example, cs_sum_ub <- cumsum(ub))


spline_int_mean <- as.data.frame(spline(example$event, example$cs_sum))
spline_int_lb <- as.data.frame(spline(example$event, example$cs_sum_lb))
spline_int_ub <- as.data.frame(spline(example$event, example$cs_sum_ub))

spline_int_df <- data.frame(spline_int_mean$x,
                            spline_int_mean$y,
                            spline_int_lb$y,
                            spline_int_ub$y)

colnames(spline_int_df) <- c("x", "y", "lb","ub")

#wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

plt_b <- ggplot(example) + geom_hline(yintercept=0, colour='grey') +
  geom_point(aes(x = event, y = cs_sum), size = 4, shape=17) +
  geom_line(data = spline_int_mean, aes(x = x, y = y)) +
  coord_cartesian(ylim=c(-10,100), xlim = c(0, 5)) +
  ylab("Case Strength (points)") + th +
  xlab(NULL) +
  theme(plot.margin=unit(c(25.5, 25.5, 0.5, 25.5),"points")) +
  #theme(axis.text.x = element_text(angle = 45,hjust = 1, )) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Helvetica")) +
  scale_x_continuous(breaks = 0:5,
                     labels = c(" ","Scenario",
                                "Evidence 1","Evidence 2",
                                "Evidence 3"," ")) +
  theme(axis.text.y = element_blank()) +
  geom_text_repel(data = example, aes(event, cs_sum, label = text_wrapped),
                  nudge_x=c(-3, 0, 0, 0), nudge_y=c(45,-25,20,-35), size=rel(4),
                  point.padding = 5.0)
  #annotate(geom="text", x = example$event, y = example$cs_sum,
  #         label= example$text_wrapped, size = rel(4), hjust=c(0,0,0,0))

panel_B <- ggplot_gtable(ggplot_build(plt_b))

ggsave('figs/main_fig_1_B.pdf', plot=panel_B,
       width=10, height=5, units='in', useDingbats=FALSE)

############### Panel C1: Crime effects ############################
# get scenario effects
se <- effects %>% filter(variable == 'gamma', evidence == 'baseline', outcome=='rating') %>% 
  select(scenario, mean, X2.5., X97.5.) %>%
  mutate(outcome='rating',
         scenario = factor(scenario))

#quick ordering
se$rank <- rank(se$mean)

#add an indicator variable column to highlight our example in panel B
se <- se %>%
  mutate(highlight = ifelse(scenario == scnum, "yes", "no"))

plt_c1 <- ggplot(data=se) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(colour = "black", fill = highlight, shape = highlight, x=reorder(scenario, rank),
                      y=mean, ymin=X2.5., ymax=X97.5.), size=.75,
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
  theme(plot.margin=unit(c(25.5, 5.5, 0, 25.5),"points")) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="plain")) +
  geom_segment(aes(x = 1, y = -5, xend=32, yend=-5),
               arrow = arrow(length = unit(0.5, "cm"),ends = 'both')) +
  annotate("text", x = 6, y = -9, label = "victimless", size=rel(5))+
  annotate("text", x = 29, y = -9, label = "homicide", size=rel(5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(vjust = 15)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values=c("white", "black"), aesthetics="fill") + 
  scale_colour_manual(values=c("black", "red"), aesthetics="color")


panel_C1 <- ggplot_gtable(ggplot_build(plt_c1))
panel_C1$layout$clip[panel_C1$layout$name == "panel"] <- "off"
grid::grid.draw(panel_C1)

############### Panel C2: Effect sizes for case strength ##################################
# get evidence effects
fe <- effects %>% filter(outcome=='rating',variable == 'mu', evidence != 'baseline') %>%
  select(mean, evidence, X2.5., X97.5.) %>%
  mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                            "physicalNon-DNA", 
                                            "witnessYes Witness", 
                                            "historyRelated", 
                                            "historyUnrelated")))

#add an indicator variable column to highlight our example in panel B
condition <- fe$evidence %in% example_evidence$evidence
fe$highlight[condition] <- "yes"

fe <- fe %>%
  mutate(highlight = replace_na(highlight, "no"))

#plot
plt_c2 <- ggplot(data=fe) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(colour = "black", fill = highlight, shape = highlight, x=evidence, y=mean, ymin=X2.5., ymax=X97.5.), size=.75,
                  position=position_dodge(width = 0.75)) + 
  evidence_x_axis +
  group_color_scale +
  coord_cartesian(ylim=c(-10,50)) +
  ggtitle("Evidence Effects") +
  ylab("Case Strength (points)") +
  xlab(NULL) +
  geom_vline(xintercept=0.5, colour='grey') +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank())  +
  theme(plot.margin=unit(c(25.5, 25.5, 0, -10),"points")) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Helvetica")) +
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values=c("white", "black"), aesthetics="fill") + 
  scale_colour_manual(values=c("black", "red"), aesthetics="color")

panel_C2 <- ggplot_gtable(ggplot_build(plt_c2))
panel_C2$layout$clip[panel_C2$layout$name == "panel"] <- "off"
grid::grid.draw(panel_C2)


#####################
#combine 1C1 and 1C2

panel_C <- ggarrange(plt_c1,plt_c2,
                     ncol = 2,
                     font.label = list(size = 20),
                     align='h',
                     widths = c(1,1.5))

ggsave('figs/main_fig_1_C.pdf', plot=panel_C,
       width=12, height=5, units='in', useDingbats=FALSE)


############### Combine into a single figure ##################################
# make a list of panels
combo_plot <- ggarrange(panel_A,
                        ggarrange(panel_B, 
                                  ggarrange(panel_C,
                                            labels=c('C'),font.label = list(size = 20), align = 'hv'),
                                  heights = c(1.25,2), nrow = 2,labels=c("B"), font.label = list(size = 20)),
                        ncol = 2,labels = "A", font.label = list(size = 20), widths=c(1,2))

#combo_plot<-combo_plot+theme(plot.margin=unit(c(25.5, 25.5, 25.5, 25.5), "points"))

ggsave('figs/main_fig_1.pdf', plot=combo_plot,
       width=14, height=8,
       units='in', useDingbats=FALSE)
