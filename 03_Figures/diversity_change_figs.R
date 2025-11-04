library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggeffects)
library(sjPlot)
library(marginaleffects)

# basic boxplots without models --------

## all results together ---------
#reorder groups
phylo_df$community <- factor(phylo_df$community,
                              levels  = c("sagebrush",
                                          "spruce-fir",
                                          "upland-herb",
                                          "alpine"))
phylo_df$metric <- factor(phylo_df$metric,
                             levels  = c("PD",
                                         "MPD",
                                         "MNTD"))

ggplot(phylo_df, aes(x=community, y = SES, color = year)) +
  geom_violin()+
  theme_classic(base_size = 22) +
  labs(y = "SES", x = "Community type")+
  facet_grid(.~metric)+
  scale_color_manual(values = c("#56c667","#33638d"))

ggsave("Figures/boxplots_bymetric.pdf",width = 15, height = 7)
ggsave("Figures/boxplots_bymetric.jpeg",width = 18, height = 7, dpi = 600)

## paired boxplot ----
phylo_paired_df$community <- factor(phylo_paired_df$community,
                             levels  = c("sagebrush",
                                         "spruce-fir",
                                         "upland-herb",
                                         "alpine"))
phylo_paired_df$metric <- factor(phylo_paired_df$metric,
                          levels  = c("PD",
                                      "MPD",
                                      "MNTD"))

ggplot(phylo_paired_df, aes(x=community, y = SES_change)) +
  geom_boxplot()+
  theme_classic(base_size = 22) +
  labs(y = Delta ~ "SES", x = "Community type")+
  facet_grid(.~metric)


## WITHOUT lumped genera --------

phylo_df_NL$community <- factor(phylo_df_NL$community,
                             levels  = c("sagebrush",
                                         "spruce-fir",
                                         "upland-herb",
                                         "alpine"))
phylo_df_NL$metric <- factor(phylo_df_NL$metric,
                          levels  = c("PD",
                                      "MPD",
                                      "MNTD"))

ggplot(phylo_df_NL, aes(x=community, y = SES, color = year)) +
  geom_boxplot()+
  theme_classic(base_size = 22) +
  labs(y = "SES", x = "Community")+
  facet_grid(.~metric)+
  scale_color_manual(values = c("#56c667","#33638d"))

ggsave("Figures/boxplots_bymetric_nolumped.jpeg",width = 18, height = 7, dpi = 600)

# linear model figures --------------------

## including lumped --------

all_pred_fig <- ggplot(all_pred) +
  geom_pointrange(mapping = aes(x = community, y= estimate, 
                                ymin = conf.low,
                                ymax = conf.high, 
                                colour = Metric), 
                  position = position_dodge(width = 0.2),
                  size = 0.8,
                  linewidth = 1.3)+
  theme(axis.text.x = element_text(colour="black"))+
  theme_classic(base_size = 22)+
  ylab(Delta ~ "SES")+
  xlab("Community Type")+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed", linewidth = 1.3)+
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "black"))

plot(all_pred_fig)
ggsave("Figures/LM_withlumped.png", width = 12, height = 7, dpi = 600)

## without lumped ------

all_pred_fig_NL <- ggplot(all_pred_NL) +
  geom_pointrange(mapping = aes(x = community, y= estimate, 
                                ymin = conf.low,
                                ymax = conf.high, 
                                colour = metric), 
                  position = position_dodge(width = 0.2),
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 22)+
  ylab(Delta ~ "SES")+
  xlab("Community Type")+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed", linewidth = 1.3)+
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "black"))


plot(all_pred_fig_NL)
ggsave("Figures/LM_NOlumped.png", width = 12, height = 7, dpi = 600)

## combine with patchwork ------
all_change_fig <- all_pred_fig + all_pred_fig_NL +
  plot_annotation(tag_levels = 'A')+
  plot_layout(axis_titles = "collect", guides = "collect")

plot(all_change_fig)
ggsave("Figures/LM_allchange.PDF", width = 15, height = 7)

# multiple regression with change ses ~ site elevation by community type -------

## PD ------------------------------

# reorder groups
pd_paired$community <- factor(pd_paired$community,
                            levels  = c("sagebrush",
                                        "spruce-fir",
                                        "upland-herb",
                                        "alpine"))

# figure 
pd_lm_fig <- ggplot(pd_paired,aes(y=SES_change,x=Elevation_m,color=community))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  theme_bw(base_size = 22)+
  scale_color_manual(values=c("yellow","#8FD744FF","#287C8EFF","#440154FF"))+
  labs(y = expression(Delta ~ "PD SES"), x = "Elevation (m)")

plot(pd_lm_fig)

## MPD ----------
# reorder groups
mpd_paired$community <- factor(mpd_paired$community,
                              levels  = c("sagebrush",
                                          "spruce-fir",
                                          "upland-herb",
                                          "alpine"))
# figure 
mpd_lm_fig <- ggplot(mpd_paired,aes(y=SES_change,x=Elevation_m,color=community))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  theme_bw(base_size = 22)+
  scale_color_manual(values=c("yellow","#8FD744FF","#287C8EFF","#440154FF"))+
  labs(y = expression(Delta ~ "MPD SES"), x = "Elevation (m)")

plot(mpd_lm_fig)


## MNTD --------

# reorder groups
mntd_paired$community <- factor(mntd_paired$community,
                               levels  = c("sagebrush",
                                           "spruce-fir",
                                           "upland-herb",
                                           "alpine"))
# figure
mntd_lm_fig <- ggplot(mntd_paired,aes(y=SES_change,x=Elevation_m,color=community))+
  geom_point()+
  stat_smooth(method="lm",se=TRUE)+
  theme_bw(base_size = 22)+
  scale_color_manual(values=c("yellow","#8FD744FF","#287C8EFF","#440154FF"))+
  labs(y = expression(Delta ~ "MNTD SES"), x = "Elevation (m)")
plot(mntd_lm_fig)


## combine regressions with patchwork --------
all_regression_fig <- pd_lm_fig + mpd_lm_fig + mntd_lm_fig +
  plot_annotation(tag_levels = 'A')+
  plot_layout(axis_titles = "collect", guides = "collect")

plot(all_regression_fig)
ggsave("Figures/regression_fig.PDF", height = 7, width = 20)



