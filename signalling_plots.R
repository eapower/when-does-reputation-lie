library(data.table)
library(ggplot2)
library(truncnorm)
library(latticeExtra)
library(cowplot)

#### Figure 1

ba <- read.csv("rep_change.csv", header=TRUE, as.is=TRUE)

## in degree is our proxy for social capital, to align with the model. 
## 42 is max in degree in the village, 39 is max among big vow-takers; 0 is min in village, 2 is min among big vow-takers
ba$s <- (ba$in_degree - min(ba$in_degree, na.rm = TRUE))/( max(ba$in_degree, na.rm = TRUE) -  min(ba$in_degree, na.rm = TRUE))
ba$s_jitter <- ave(as.numeric(ba$s), ba$s, 
                   FUN = function(x) x + rnorm(length(x), sd = .01))

pdf("repchange.pdf", 8,4)
ggplot(ba, aes(x = (s_jitter), y = pbefore, colour = SCBC)) + 
  geom_point(aes(shape = SCBC, colour = SCBC)) + 
  labs(x = "Social Capital", y = "(Change in) Reputation") + 
  scale_y_continuous(limits = c(-0.001, 0.13)) + 
  geom_segment(aes(y = pbefore, yend = pbefore + pchange, x = s_jitter, xend = s_jitter), lineend = "round", size = 1.5, alpha = 0.5) + 
  scale_color_brewer(palette = "Set2") + 
  theme( panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background=element_rect(fill = "white"),
         panel.background = element_rect(fill = 'white'),
         axis.line=element_line(color = "black"),
         legend.position="none"
    )
dev.off()



#### Analytical plots 
anal1 = fread('analresults_cueTrue_sigTrue_fdFalse_0.8_0.6_0.2_0.4.csv',sep=',',header=TRUE)

anal2 = fread('analresults_cueFalse_sigTrue_fdTrue_0.8_0.6_0.2_0.4.csv',sep=',',header=TRUE)

anal3 = fread('analresults_cueTrue_sigTrue_fdTrue_0.8_0.6_0.2_0.4.csv',sep=',',header=TRUE)

## Mechanism 1
pdf("analytical_mech1.pdf", 8,4)
ggplot(anal1, aes(x=s)) + 
  geom_line(aes(y=pL), colour = "#ca0020", linetype = 2, size = 1.25, alpha = 0.7) + 
  geom_line(aes(y=pH), colour = "#ca0020", linetype = 3, size = 1.25, alpha = 0.7) + 
  geom_line(aes(y = rep_boost_success), colour = "#0571b0", linetype = 4, size = 1.25, alpha = 0.7) + 
  geom_line(aes(y = rep_boost_failure), colour = "#0571b0", linetype = 5, size = 1.25, alpha = 0.7) + 
  scale_y_continuous(
    name = "Probability of signalling",
    sec.axis = sec_axis(~., name="Change in reputation after signalling"),
    breaks = c(-0.5,0,.5,1), 
    limits = c(-.4, 1.2)
  ) + 
  scale_x_continuous(
    name = "Social prominence/capital", 
    breaks = c(0,.2,.4,.6,.8,1), 
    limits = c(0,1.001)
  ) +
  annotate("text", x = 0.2, y = 1.15, label = "hybrid equilibrium") + 
  annotate("text", x = 0.7, y = 1.15, label = "pooling equilibrium") + 
  annotate("text", x = 0.1, y = 0.9, label = "high quality", colour = "#ca0020", size = 3) + 
  annotate("text", x = 0.3, y = 0.5, label = "low quality", colour = "#ca0020", size = 3) + 
  annotate("text", x = 0.5, y = 0.25, label = "after success", colour = "#0571b0", size = 3) + 
  annotate("text", x = 0.85, y = 0, label = "after failure", colour = "#0571b0", size = 3) + 
  geom_vline(xintercept = 0.4, size = 1.25, colour = "grey") + 
  theme(
    axis.title.y = element_text(color = "#ca0020"),
    axis.title.y.right = element_text(color = "#0571b0"),
    # get rid of panel grids
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "white"),
    panel.background = element_rect(fill = 'white'),
    # Change legend 
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(color = "gray", fill = "white"),
    legend.title = element_text(color = "black"),
    legend.text = element_text(color = "black"), 
    axis.title.x = element_text(colour = "black"),
    strip.background =element_rect(fill="gray"), 
    axis.text=element_text(color="black"), 
    axis.ticks=element_line(color = "black"),
    axis.line=element_line(color = "black")
  )
dev.off()


pdf("analytical_mech1_strat.pdf", 8,4)
ggplot(anal1, aes(x=s)) + 
  geom_line(aes(y=pL), colour = "#ca0020", linetype = 2, size = 1.25, alpha = 0.7) + 
  geom_line(aes(y=pH), colour = "#ca0020", linetype = 3, size = 1.25, alpha = 0.7) + 
  scale_y_continuous(
    name = "Probability of signalling",
    breaks = c(0,.5,1), 
    limits = c(0, 1.2)
  ) + 
  scale_x_continuous(
    name = "Social prominence/capital", 
    breaks = c(0,.2,.4,.6,.8,1), 
    limits = c(0,1.001)
  ) +
  annotate("text", x = 0.2, y = 1.15, label = "hybrid equilibrium") + 
  annotate("text", x = 0.7, y = 1.15, label = "pooling equilibrium") + 
  annotate("text", x = 0.1, y = 0.9, label = "high quality", colour = "#ca0020", size = 3) + 
  annotate("text", x = 0.3, y = 0.5, label = "low quality", colour = "#ca0020", size = 3) + 
  geom_vline(xintercept = 0.4, size = 1.25, colour = "grey") + 
  theme_classic()
dev.off()



## Mechanism 2
pdf("analytical_mech2.pdf", 8,4)
ggplot(anal2, aes(x=s)) + 
  geom_line(aes(y=pL), colour = "#ca0020", linetype = 2, size = 1.25, alpha = 0.7) + 
  geom_line(aes(y=pH), colour = "#ca0020", linetype = 3, size = 1.25, alpha = 0.7) + 
  scale_y_continuous(
    name = "Probability of signalling",
    breaks = c(0,.5,1), 
    limits = c(0, 1.2)
  ) + 
  scale_x_continuous(
    name = "Social prominence/capital", 
    breaks = c(0,.2,.4,.6,.8,1), 
    limits = c(0,1.001)
  ) +
  annotate("text", x = 0.16, y = 1.15, label = "pooling eq") + 
  annotate("text", x = 0.45, y = 1.15, label = "separating eq") + 
  annotate("text", x = 0.73, y = 1.15, label = "hybrid eq") + 
  annotate("text", x = 0.975, y = 1.15, label = "pooling eq") + 
  annotate("text", x = 0.42, y = 0.9, label = "high quality", colour = "#ca0020", size = 3) + 
  annotate("text", x = 0.8, y = 0.5, label = "low quality", colour = "#ca0020", size = 3) + 
  geom_vline(xintercept = 0.35, size = 1.1, colour = "grey", alpha = 0.7) + 
  geom_vline(xintercept = 0.55, size = 1.1, colour = "grey", alpha = 0.7) + 
  geom_vline(xintercept = 0.9, size = 1.1, colour = "grey", alpha = 0.7) + 
  theme_classic()
dev.off()

## Mechanism 1 and 2
pdf("analytical_mech1&2.pdf", 8,4)
ggplot(anal3, aes(x=s)) + 
  geom_line(aes(y=pL), colour = "#ca0020", linetype = 2, size = 1.2, alpha = 0.7) + 
  geom_line(aes(y=pH), colour = "#ca0020", linetype = 3, size = 1.25, alpha = 0.7) + 
  scale_y_continuous(
    name = "Probability of signalling",
    breaks = c(0,.5,1), 
    limits = c(0, 1.2)
  ) + 
  scale_x_continuous(
    name = "Social prominence/capital", 
    breaks = c(0,.2,.4,.6,.8,1), 
    limits = c(0,1.001)
  ) +
  annotate("text", x = 0.16, y = 1.15, label = "pooling eq") + 
  annotate("text", x = 0.45, y = 1.15, label = "separating eq") + 
  annotate("text", x = 0.65, y = 1.15, label = "hybrid eq") + 
  annotate("text", x = 0.85, y = 1.15, label = "pooling eq") + 
  annotate("text", x = 0.42, y = 0.9, label = "high quality", colour = "#ca0020", size = 3) + 
  annotate("text", x = 0.68, y = 0.25, label = "low quality", colour = "#ca0020", size = 3) + 
  geom_vline(xintercept = 0.35, size = 1.1, colour = "grey", alpha = 0.7) + 
  geom_vline(xintercept = 0.55, size = 1.1, colour = "grey", alpha = 0.7) + 
  geom_vline(xintercept = 0.75, size = 1.1, colour = "grey", alpha = 0.7) + 
  theme_classic()
dev.off()






########################### ABM plots  ###########################
traj_df <- function(df,num_individuals){
  i1 = sample(unlist(unique(df[type=='q=0, init S_i=low','id'])),num_individuals)
  i2 = sample(unlist(unique(df[type=='q=0, init S_i=high','id'])),num_individuals)
  i3 = sample(unlist(unique(df[type=='q=1, init S_i=low','id'])),num_individuals)
  i4 = sample(unlist(unique(df[type=='q=1, init S_i=high','id'])),num_individuals)
  individuals <- df[id %in% unlist(c(i1,i2,i3,i4)),]
  return(individuals)
}
### Main text figure
d1 = fread('resultsdf_cueTrue_sigFalse_fdFalse.csv',sep=',',header=TRUE)
d1[,model:='(a) Cue Only']
d2 = fread('resultsdf_cueTrue_sigTrue_fdFalse.csv',sep=',',header=TRUE)
d2[,model:="(b) Mechanism 1: Altered Prior"]
d3 = fread('resultsdf_cueFalse_sigTrue_fdTrue.csv',sep=',',header=TRUE)
d3[,model:="(c) Mechanism 2: Altered Payoff"]
d4 = fread('resultsdf_cueTrue_sigTrue_fdTrue.csv',sep=',',header=TRUE)
d4[,model:="(d) Mechanisms 1 & 2"]
d = rbindlist(list(d1,d2,d3,d4),use.names=TRUE)
d[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                       ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
d$type = factor(d$type,levels=c('q=0, init S_i=high',
                                'q=0, init S_i=low',
                                'q=1, init S_i=high',
                                'q=1, init S_i=low'))

individuals <- traj_df(d,15)


my_colors <- RColorBrewer::brewer.pal(8, "Paired")[c(6, 7, 2, 4)]

## plot just to get legend
myhist <- ggplot(d, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,22)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="bottom") + scale_fill_manual(values = my_colors) + scale_color_manual(values = my_colors)

legend <- cowplot::get_legend(myhist)

pdf("histograms_4models_d098_v2.pdf", 10,6)
myhist <- ggplot(d, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,22)) +facet_wrap(~model) + xlab('social prominence/capital')  + scale_fill_manual(values = my_colors) + scale_color_manual(values = my_colors) + 
  theme( panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background=element_rect(fill = "white"),
         panel.background = element_rect(fill = 'white'),
         axis.line=element_line(color = "black"),
         legend.position="none"
  )
cowplot::plot_grid(myhist, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()


pdf("trajectories_4models_d098_v2.pdf", 10, 6)
plot <- ggplot(individuals,aes(x=time,y=support, color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model) + ylab('social prominence/capital') + scale_color_manual(values = my_colors) +
  theme( panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         plot.background=element_rect(fill = "white"),
         panel.background = element_rect(fill = 'white'),
         axis.line=element_line(color = "black"),
         legend.position="none"
  )
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

########## SI: Plot pairwise interactions

pdf("histograms_Pairwise_4models_d098.pdf", 10, 6)
plot <- ggplot(d, aes(x=bilateral_interaction, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 2,
                 position="identity") + coord_cartesian(ylim=c(0,0.46)) +facet_wrap(~model) + xlab('pairwise interactions')  +  theme(legend.position="none") + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()


pdf("trajectories_Pairwise_4models_d098.pdf", 10, 6)
plot <- ggplot(individuals,aes(x=time,y=bilateral_interaction,color=type, group = id)) + geom_line(alpha=.2) + facet_wrap(~model)+  theme(legend.position="none") + ylab('pairwise interactions') + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()


######### SI: Plots with 2 way learning in the pairwise interactions
d1b = fread('resultsdf_cueTrue_sigFalse_fdFalse_2ways.csv',sep=',',header=TRUE)
d1b[,model:='(a) Cue Only']
d2b = fread('resultsdf_cueTrue_sigTrue_fdFalse_2ways.csv',sep=',',header=TRUE)
d2b[,model:="(b) Mechanism 1: Altered Prior"]
d3b = fread('resultsdf_cueFalse_sigTrue_fdTrue_2ways.csv',sep=',',header=TRUE)
d3b[,model:="(c) Mechanism 2: Altered Payoff"]
d4b = fread('resultsdf_cueTrue_sigTrue_fdTrue_2ways.csv',sep=',',header=TRUE)
d4b[,model:="(d) Mechanisms 1 & 2"]
db = rbindlist(list(d1b,d2b,d3b,d4b),use.names=TRUE)
db[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                       ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
db$type = factor(db$type,levels=c('q=0, init S_i=high',
                                'q=0, init S_i=low',
                                'q=1, init S_i=high',
                                'q=1, init S_i=low'))
individualsb <- traj_df(db,15)

pdf("histograms_PI2ways_4models_d098.pdf", 10, 6)
plot <- ggplot(db, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,20)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="none") + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

pdf("trajectories_PI2ways_4models_d098.pdf", 10, 6)
plot <- ggplot(individualsb,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="none") + ylab('social prominence/capital') + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

##### SI: plots alternative scenarios

d1c = fread('resultsdf_cueFalse_sigFalse_fdFalse.csv',sep=',',header=TRUE)
d1c[,model:='Private learning only']
d2c = fread('resultsdf_cueFalse_sigTrue_fdFalse_pooling.csv',sep=',',header=TRUE)
d2c[,model:='Signal, no soc prom/cap (pooling eq)']
d3c = fread('resultsdf_cueFalse_sigTrue_fdFalse_semipooling.csv',sep=',',header=TRUE)
d3c[,model:='Signal, no soc prom/cap (hybrid eq)']
d4c = fread('resultsdf_cueTrue_sigFalse_fdFalse_noprivatelearning.csv',sep=',',header=TRUE)
d4c[,model:="Cue only (no private learning)"]
d5c = fread('resultsdf_cueTrue_sigTrue_fdFalse_noprivatelearning.csv',sep=',',header=TRUE)
d5c[,model:="Mech 1: Alt Prior (no private learning)"]
d6c = fread('resultsdf_cueFalse_sigTrue_fdTrue_noprivatelearning.csv',sep=',',header=TRUE)
d6c[,model:="Mech 2: Alt Payoff (no private learning)"]
d7c = fread('resultsdf_cueTrue_sigTrue_fdTrue_noprivatelearning.csv',sep=',',header=TRUE)
d7c[,model:="Mech 1 & 2 (no private learning)"]
dc = rbindlist(list(d1c,d2c,d3c,d4c,d5c,d6c,d7c),use.names=TRUE)
dc[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                 ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                        ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
dc$type = factor(dc$type,levels=c('q=0, init S_i=high',
                                  'q=0, init S_i=low',
                                  'q=1, init S_i=high',
                                  'q=1, init S_i=low'))
dc$model = factor(dc$model,levels=c('Private learning only','Signal, no soc prom/cap (pooling eq)','Signal, no soc prom/cap (hybrid eq)',"Cue only (no private learning)","Mech 1: Alt Prior (no private learning)","Mech 2: Alt Payoff (no private learning)","Mech 1 & 2 (no private learning)"))
individualsc <- traj_df(dc,15)

pdf("histograms_7altmodels_d098.pdf", 10, 6)
plot <- ggplot(dc, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,22)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="none") + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

pdf("trajectories_7altmodels_d098.pdf", 10, 6)
plot <- ggplot(individualsc,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="none") + ylab('social prominence/capital') + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()


##### SI: perfect memory
d1d = fread('resultsdf_cueTrue_sigFalse_fdFalse_d1.csv',sep=',',header=TRUE)
d1d[,model:='(a) Cue Only']
d2d = fread('resultsdf_cueTrue_sigTrue_fdFalse_d1.csv',sep=',',header=TRUE)
d2d[,model:="(b) Mechanism 1: Altered Prior"]
d3d = fread('resultsdf_cueFalse_sigTrue_fdTrue_d1.csv',sep=',',header=TRUE)
d3d[,model:="(c) Mechanism 2: Altered Payoff"]
d4d = fread('resultsdf_cueTrue_sigTrue_fdTrue_d1.csv',sep=',',header=TRUE)
d4d[,model:="(d) Mechanisms 1 & 2"]
dd = rbindlist(list(d1d,d2d,d3d,d4d),use.names=TRUE)
dd[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                 ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                        ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
dd$type = factor(dd$type,levels=c('q=0, init S_i=high',
                                  'q=0, init S_i=low',
                                  'q=1, init S_i=high',
                                  'q=1, init S_i=low'))
individualsd <- traj_df(dd,15)

pdf("histograms_4altmodels_d1.pdf", 10, 6)
plot <- ggplot(dd, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,16)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="none") + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

pdf("trajectories_4altmodels_d1.pdf", 10, 6)
plot <- ggplot(individualsd,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="none") + ylab('social prominence/capital') + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

##### SI: success/failure with equal probability
d1d = fread('resultsdf_cueTrue_sigTrue_fdFalse_equalprobFS.csv',sep=',',header=TRUE)
d1d[,model:="(a) Mechanism 1: Altered Prior"]
d2d = fread('resultsdf_cueFalse_sigTrue_fdTrue_equalprobFS.csv',sep=',',header=TRUE)
d2d[,model:="(b) Mechanism 2: Altered Payoff"]
d3d = fread('resultsdf_cueTrue_sigTrue_fdTrue_equalprobFS.csv',sep=',',header=TRUE)
d3d[,model:="(c) Mechanisms 1 & 2"]

dd = rbindlist(list(d1d,d2d,d3d),use.names=TRUE)
dd[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                 ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                        ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
dd$type = factor(dd$type,levels=c('q=0, init S_i=high',
                                  'q=0, init S_i=low',
                                  'q=1, init S_i=high',
                                  'q=1, init S_i=low'))
individualsd <- traj_df(dd,15)

pdf("histograms_3altmodels_equalprobFS.pdf", 10, 6)
plot <- ggplot(dd, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,20)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="none") + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()

pdf("trajectories_3altmodels_equalprobFS.pdf", 10, 6)
plot <- ggplot(individualsd,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="none") + ylab('social prominence/capital') + scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors)
cowplot::plot_grid(plot, legend, ncol = 1, rel_heights = c(1, .1))
dev.off()