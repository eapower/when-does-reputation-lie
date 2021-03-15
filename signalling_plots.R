library(data.table)
library(ggplot2)
library(truncnorm)
library(latticeExtra)

#### Figure 1

ba <- read.csv("big_fest_particip.csv", header=TRUE, as.is=TRUE)

ba$pchange<-(ba$AllRep.Redo/408)-(ba$AllRep.Orig/2880)

## 42 is max in village, 39 is max in big vow-takers; 0 is min in village, 2 is min in big vow-takers

ba$s <- (ba$KSup3.In.Degree - min(ba$KSup3.In.Degree, na.rm = TRUE))/( max(ba$KSup3.In.Degree, na.rm = TRUE) -  min(ba$KSup3.In.Degree, na.rm = TRUE))
ba$s_jitter <- ave(as.numeric(ba$s), ba$s, 
                   FUN = function(x) x + rnorm(length(x), sd = .01))


ggplot(ba, aes(x=(s_jitter), y=AllRep.Orig/2880, colour=SCBC)) + geom_point(aes(shape=ba$SCBC, colour=SCBC)) + labs(x = "Social Capital",y = "(Change in) Reputation") + scale_y_continuous(limits=c(-0.001,0.13)) + geom_segment(aes(y=AllRep.Orig/2880,yend=AllRep.Orig/2880+pchange,x=s_jitter,xend=s_jitter),lineend = "round",size=1.5, alpha = 0.5) + scale_color_brewer(palette="Set2") + theme(legend.position="none")




#### Analytical plots 
anal1 = fread('analresults_cueTrue_sigTrue_fdFalse_0.8_0.6_0.2_0.4.csv',sep=',',header=TRUE)
strategies =  melt(anal1[,.(s,pL,pH)],id.vars=c('s'),measure.vars=c('pL','pH'),variable.name='quality',value.name = 'probability of signalling')
rep_boost = melt(anal1[,.(s,rep_boost_success,rep_boost_failure)],id.vars=c('s'),measure.vars=c('rep_boost_success','rep_boost_failure'),variable.name='outcome',value.name = 'change in reputation after signalling')
obj1 <- xyplot(`probability of signalling`~s, data=strategies, groups = quality,ylim = c(-.3,1.3),pch=c(3,4), col='black',xlab='social prominence/capital',
               panel = function(...) {
                 panel.text(c(0.1,.2,0.15,.7),c(.95,.7,1.2,1.2),c('high quality','low quality','semi-separating equilibrium','pooling equilibrium'),col='black')
                 panel.xyplot(...)
                 })
obj2 <- xyplot(`change in reputation after signalling`~s,data = rep_boost, groups = outcome, ylim=c(-.3,1.3),col='red',pch=c(1,2),xlab='social prominence/capital',
               panel = function(...) {
                 panel.text(c(0.65,.7),c(.3,-.15),c('after success','after failure'),col='red')
                 panel.xyplot(...)
               })
doubleYScale(obj1, obj2, add.ylab2 = TRUE)+ layer(panel.abline(v = .4,lwd=8,col=alpha('grey',.5)))+layer(panel.abline(h=1.1,lwd=1,col='black')) + layer(panel.abline(h=0,lwd=1,col='grey')) 
update(trellis.last.object(),
       par.settings = simpleTheme(col = c("black", "red")))
                 
anal2 = fread('analresults_cueFalse_sigTrue_fdTrue_0.8_0.6_0.2_0.4.csv',sep=',',header=TRUE)
strategies =  melt(anal2[,.(s,pL,pH)],id.vars=c('s'),measure.vars=c('pL','pH'),variable.name='quality',value.name = 'probability of signalling')
rep_boost = melt(anal2[,.(s,rep_boost_success,rep_boost_failure)],id.vars=c('s'),measure.vars=c('rep_boost_success','rep_boost_failure'),variable.name='outcome',value.name = 'reputation after signalling')
xyplot(`probability of signalling`~s, data=strategies, groups = quality,ylim = c(-.1,1.3),pch=c(3,4), col='black',xlab='social prominence/capital',
               panel = function(...) {
                 panel.text(c(0.15,.45,.72,.96),c(1.2,1.2,1.2,1.2),c('pooling eq',paste(strwrap('separating eq',width=.2), collapse="\n"),'semi-separating eq',paste(strwrap('pooling eq',width=.1), collapse="\n")),col='black')
                 panel.xyplot(...)
               })+ layer(panel.abline(v = .35,lwd=8,col=alpha('grey',.5)))+ layer(panel.abline(v = .55,lwd=8,col=alpha('grey',.5))) + layer(panel.abline(v = .9,lwd=8,col=alpha('grey',.5)))+layer(panel.abline(h=1.1,lwd=1,col='black'))

anal3 = fread('analresults_cueTrue_sigTrue_fdTrue_0.8_0.6_0.2_0.4.csv',sep=',',header=TRUE)
strategies =  melt(anal3[,.(s,pL,pH)],id.vars=c('s'),measure.vars=c('pL','pH'),variable.name='quality',value.name = 'probability of signalling')
rep_boost = melt(anal3[,.(s,rep_boost_success,rep_boost_failure)],id.vars=c('s'),measure.vars=c('rep_boost_success','rep_boost_failure'),variable.name='outcome',value.name = 'reputation after signalling')
xyplot(`probability of signalling`~s, data=strategies, groups = quality,ylim = c(-.1,1.3),pch=c(3,4), col='black',xlab='social prominence/capital',
       panel = function(...) {
         panel.text(c(0.15,.45,.65,.88),c(1.2,1.2,1.2,1.2),c('pooling eq',paste(strwrap('separating eq',width=.2), collapse="\n"),paste(strwrap('semi-separating eq',width=.15),collapse='\n'),'pooling eq'),col='black')
         panel.xyplot(...)
       })+ layer(panel.abline(v = .35,lwd=8,col=alpha('grey',.5)))+ layer(panel.abline(v = .55,lwd=8,col=alpha('grey',.5))) + layer(panel.abline(v = .75,lwd=8,col=alpha('grey',.5)))+layer(panel.abline(h=1.1,lwd=1,col='black'))


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
d2[,model:="(b) Mechanism 1"]
d3 = fread('resultsdf_cueFalse_sigTrue_fdTrue.csv',sep=',',header=TRUE)
d3[,model:="(c) Mechanism 2"]
d4 = fread('resultsdf_cueTrue_sigTrue_fdTrue.csv',sep=',',header=TRUE)
d4[,model:="(d) Mechanisms 1 & 2"]
d = rbindlist(list(d1,d2,d3,d4),use.names=TRUE)
d[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                       ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
d$type = factor(d$type,levels=c('q=1, init S_i=high','q=0, init S_i=low','q=0, init S_i=high','q=1, init S_i=low'))
individuals <- traj_df(d,15)

pdf("histograms_4models_d098.pdf", 10,6)
ggplot(d, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,22)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="bottom") 
dev.off()

pdf("trajectories_4models_d098.pdf", 10,6)
ggplot(individuals,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="bottom") + ylab('social prominence/capital')
dev.off()

########## SI: Plot pairwise interactions

pdf("histograms_Pairwise_4models_d098.pdf", 10,6)
ggplot(d, aes(x=bilateral_interaction, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 2,
                 position="identity") + coord_cartesian(ylim=c(0,0.46)) +facet_wrap(~model) + xlab('pairwise interactions')  +  theme(legend.position="bottom") 
dev.off()

pdf("trajectories_Pairwise_4models_d098.pdf", 10,6)
ggplot(individuals,aes(x=time,y=bilateral_interaction,color=type, group = id)) + geom_line(alpha=.2) + facet_wrap(~model)+  theme(legend.position="bottom") + ylab('pairwise interactions')
dev.off()

######### SI: Plots with 2 way learning in the pairwise interactions
d1b = fread('resultsdf_cueTrue_sigFalse_fdFalse_2ways.csv',sep=',',header=TRUE)
d1b[,model:='(a) Cue Only']
d2b = fread('resultsdf_cueTrue_sigTrue_fdFalse_2ways.csv',sep=',',header=TRUE)
d2b[,model:="(b) Mechanism 1"]
d3b = fread('resultsdf_cueFalse_sigTrue_fdTrue_2ways.csv',sep=',',header=TRUE)
d3b[,model:="(c) Mechanism 2"]
d4b = fread('resultsdf_cueTrue_sigTrue_fdTrue_2ways.csv',sep=',',header=TRUE)
d4b[,model:="(d) Mechanisms 1 & 2"]
db = rbindlist(list(d1b,d2b,d3b,d4b),use.names=TRUE)
db[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                       ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
db$type = factor(db$type,levels=c('q=1, init S_i=high','q=0, init S_i=low','q=0, init S_i=high','q=1, init S_i=low'))
individualsb <- traj_df(db,15)

pdf("histograms_PI2ways_4models_d098.pdf", 10,6)
ggplot(db, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,20)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="bottom") 
dev.off()

pdf("trajectories_PI2ways_4models_d098.pdf", 10,6)
ggplot(individualsb,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="bottom") + ylab('social prominence/capital')
dev.off()

##### SI: plots alternative scenarios
d1c = fread('resultsdf_cueFalse_sigFalse_fdFalse.csv',sep=',',header=TRUE)
d1c[,model:='Private learning only']
d2c = fread('resultsdf_cueTrue_sigFalse_fdFalse_noprivatelearning.csv',sep=',',header=TRUE)
d2c[,model:="Cue only (no private learning)"]
d3c = fread('resultsdf_cueTrue_sigTrue_fdFalse_noprivatelearning.csv',sep=',',header=TRUE)
d3c[,model:="Mech 1 (no private learning)"]
d4c = fread('resultsdf_cueFalse_sigTrue_fdTrue_noprivatelearning.csv',sep=',',header=TRUE)
d4c[,model:="Mech 2 (no private learning)"]
d5c = fread('resultsdf_cueTrue_sigTrue_fdTrue_noprivatelearning.csv',sep=',',header=TRUE)
d5c[,model:="Mech 1 & 2 (no private learning)"]
dc = rbindlist(list(d1c,d2c,d3c,d4c,d5c),use.names=TRUE)
dc[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                 ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                        ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
dc$type = factor(dc$type,levels=c('q=1, init S_i=high','q=0, init S_i=low','q=0, init S_i=high','q=1, init S_i=low'))
dc$model = factor(dc$model,levels=c('Private learning only',"Cue only (no private learning)","Mech 1 (no private learning)","Mech 2 (no private learning)","Mech 1 & 2 (no private learning)"))
individualsc <- traj_df(dc,15)

pdf("histograms_4altmodels_d098.pdf", 10,6)
ggplot(dc, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,22)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="bottom") 
dev.off()

pdf("trajectories_4altmodels_d098.pdf", 10,6)
ggplot(individualsc,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="bottom") + ylab('social prominence/capital')
dev.off()


##### SI: perfect memory
d1d = fread('resultsdf_cueTrue_sigFalse_fdFalse_d1.csv',sep=',',header=TRUE)
d1d[,model:='(a) Cue Only']
d2d = fread('resultsdf_cueTrue_sigTrue_fdFalse_d1.csv',sep=',',header=TRUE)
d2d[,model:="(b) Mechanism 1"]
d3d = fread('resultsdf_cueFalse_sigTrue_fdTrue_d1.csv',sep=',',header=TRUE)
d3d[,model:="(c) Mechanism 2"]
d4d = fread('resultsdf_cueTrue_sigTrue_fdTrue_d1.csv',sep=',',header=TRUE)
d4d[,model:="(d) Mechanisms 1 & 2"]
dd = rbindlist(list(d1d,d2d,d3d,d4d),use.names=TRUE)
dd[,type:=ifelse(qual==0 & early_rep==0,'q=0, init S_i=low',
                 ifelse(qual==0 & early_rep==1, 'q=0, init S_i=high',
                        ifelse(qual==1 & early_rep==0,'q=1, init S_i=low','q=1, init S_i=high')))]
dd$type = factor(dd$type,levels=c('q=1, init S_i=high','q=0, init S_i=low','q=0, init S_i=high','q=1, init S_i=low'))
individualsd <- traj_df(dd,15)

pdf("histograms_4altmodels_d1.pdf", 10,6)
ggplot(dd, aes(x=support, color=type, fill=type)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, binwidth = 0.04,
                 position="identity") + coord_cartesian(ylim=c(0,16)) +facet_wrap(~model) + xlab('social prominence/capital')  +  theme(legend.position="bottom") 
dev.off()

pdf("trajectories_4altmodels_d1.pdf", 10,6)
ggplot(individualsd,aes(x=time,y=support,color=type, group = id)) + geom_line(alpha=.4) + facet_wrap(~model)+  theme(legend.position="bottom") + ylab('social prominence/capital')
dev.off()
