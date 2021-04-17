# supplementary figure 2 from paper
library(tidyverse)
library(grid)
library(gridExtra)
library(rstan)
library(magick)
library(gtable)
library(gridBase)
library(factoextra)
library(ggpubr)

source('ggplot_setup.R')
load('data/stan_postprocess_2v_t.rdata')
effects <- effects %>% filter(group =='mri')
dat <- dat %>% filter(group=='mri')


####################
#PANEL B
# calculate mean rating for session and evidence combinations
sc_means <- dat %>% group_by(scenario, physical, history, witness) %>% 
  summarise(rating=mean(rating)) %>%
  arrange(scenario, physical, witness, history) %>%
  ungroup()

# get unique variable code for each evidence combination
ev_codes <- sc_means %>% select(physical, witness, history) %>% 
  distinct(physical, witness, history) %>%
  mutate(code=as.factor(row_number()))

# add code column to scenario means
sc_means <- sc_means %>% merge(ev_codes) %>% arrange(code, scenario)
code_means <- sc_means %>% group_by(physical, witness, history) %>% 
  summarise(rating=mean(rating)) %>%
  ungroup() %>%
  mutate_at(c("history"), funs(substr(as.character(.), 1, 1))) %>%
  mutate_at(c("witness"), funs(sapply(strsplit(as.character(.), " "), `[`, 1)))

# calculate model prediction
# get mean values of coefficients across scenarios
betas <- effects %>% filter(variable == 'gamma') %>% 
  select(scenario, evidence, mean) %>%
  group_by(evidence) %>%
  summarise(effect=mean(mean)) %>%
  arrange(evidence)

# generate model matrix for predictions         
Xmat <- model.matrix(form, ev_codes)
colnames(Xmat)[1] <- 'baseline'
Xmat <- Xmat[, sort(colnames(Xmat), index.return=TRUE)$ix]
preds <- data.frame(code=ev_codes$code, pred=Xmat %*% betas$effect)

plt_2 <- ggplot(sc_means) +
  geom_boxplot(aes(code, rating), outlier.size=0, outlier.stroke=0) +
  geom_jitter(aes(code, rating), width=0.2, alpha=0.5, color=color_genpop) +
  geom_point(data=preds, aes(x=code, y=pred), color="red", shape=5, size = 5, stroke=1) +
  ylab('Mean case strength (points)') +
  coord_cartesian(ylim = c(-0, 100), expand = TRUE) +
  #labs(title="B") +
  annotate(geom = "text", x = seq_len(18), y = -8, 
           label = code_means$history, size = 4) +
  annotate(geom = "text", x = -0, 
           y = -8, label = "Prior conviction", size = 4, hjust=1) +
  annotate(geom = "text", x = seq(2, 18, 3), 
           y = -15, label = code_means$witness[seq(2, 18, 3)], size = 4) +
  annotate(geom = "text", x = -0, 
           y = -15, label = "Witness", size = 4, hjust=1) +
  annotate(geom = "text", x = seq(3.5, 18, 6), 
           y = -22, label = code_means$physical[seq(3, 18, 6)], size = 4) +
  annotate(geom = "text", x = -0, 
           y = -22, label = "Physical evidence", size = 4, hjust=1) +
  th +
  theme(
    plot.margin=unit(c(25.5, 25.5, 60, 60), "points"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  theme(text=element_text(family="Helvetica"),
        plot.title = element_text(family="Helvetica",face="bold"))

evidence_plot <- ggplot_gtable(ggplot_build(plt_2))
evidence_plot$layout$clip[evidence_plot$layout$name == "panel"] <- "off"
grid::grid.draw(evidence_plot)


ggsave('figs/supp_fig_X.pdf', plot=evidence_plot,
       width=8, height=6, units='in', useDingbats=FALSE)