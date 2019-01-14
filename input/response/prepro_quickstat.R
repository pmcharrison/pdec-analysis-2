####load libraries ######
#http://www.personality-project.org/r/r.anova.html
library("ggsci")
library("lme4")
library('emmeans')
library("gdata") 
library("Hmisc")
library(languageR)
library (tidyr)
library(lmerTest)
library(plyr) # needed to access . function
library(ggplot2)
library(reshape2)
library(lsr) # for eta squared calculation
library(ez)
library(schoRsch)  #for formatted output of anova
library(pastecs) #for descriptives
library(car)  #for lmer
library(effects) #for lmer
library(lsmeans)#for lmer
library(readxl)
source("diagnostic_fcns.r")
library(lattice)
library(foreign)
library(multcomp)
library(lsmeans)
library(phia)
library(stats)
library(reshape2)
library(data.table)
library(plotrix)
library(HH)
library(effects)
library(grid)
library(data.table)
library(xtable)
library(gridExtra)
######
#####load data #######
ls()
rm(list=ls())
dir = "insert/directory/"

setwd(dir)

dat=read.table(file="all.csv",header=T, sep="\t")  # read data
dat$cyc <- " "
dat$cyc[dat$speed==1]= "TEN"
dat$cyc[dat$speed==2]= "TEN"
dat$cyc[dat$speed==3]= "TEN"
dat$cyc[dat$speed==4]= "TWE"
dat$cyc[dat$speed==5]= "TWE"
dat$cyc[dat$speed==6]= "TWE"
dat$cond[dat$condi==1]= "RAND"
dat$cond[dat$condi==2]= "RANDREG"
dat$cond[dat$condi==3]= "CONT"
dat$cond[dat$condi==4]= "STEP"
dat$tempo[dat$speed==1]= "25ms"
dat$tempo[dat$speed==2]= "50ms"
dat$tempo[dat$speed==3]= "75ms"
dat$tempo[dat$speed==4]= "25ms"
dat$tempo[dat$speed==5]= "50ms"
dat$tempo[dat$speed==6]= "75ms"

dat$subj<-as.factor(dat$subj)
dat$block<-as.integer(dat$block)
dat$cond<-as.factor(dat$cond)
dat$trialN<-as.integer(dat$trialN)
dat$tempo<-as.factor(dat$tempo)
dat$cyc<-as.factor(dat$cyc)
dat$condi<-as.factor(dat$condi)


colnames(dat)[colnames(dat)=="response"] <- "error"
dat$response[dat$error==2]= "CorRej"
dat$response[dat$error==1]= "Hit"
dat$response[dat$error==0]= "Miss"
dat$response[dat$error==-1]= "FA"
dat$response[dat$error==-2]= "TooEarly"
dat$response<-as.factor(dat$response)


# subj=c(1, 2, 3,4,5, 6,7, 8, 9,10, 11,12, 13)
# dat=dat[dat$subj%in%subj,]
unique(dat$subj)
dat<-droplevels(dat[dat$subj!=7 ,])
#dat<-droplevels(dat[dat$subj!=9 ,])

###preprocess RT and Errors 
d1<-droplevels(dat[dat$correct==1 ,])
d<-droplevels(d1[d1$condi==2 | d1$condi==4 ,])   # subselect only correct trials and trials with response

#agg<- with(d[d$cond=='STEP',], aggregate(cbind(RTs) ~ block+subj, FUN="median"))  #get mean of STEP condition per block per subject
agg<- with(d[d$cond=='STEP' & d$correct==1,], aggregate(cbind(RTs) ~ block+subj, FUN="mean"))  #get mean of STEP condition per block per subject

names(agg)[3]<-paste("bsl") 
d<-merge(agg, d, by=c('subj', 'block'), all=T)
d$RTadj <- ifelse(d$cond=="RANDREG", d$RTs-d$bsl, d$RTs)  #baseline the RTs with the mean step of their corresponding block
summary(d)


##only to check subj 7
#stat.desc(d$bsl[ d$block=="3"])
#stat.desc(d$bsl[d$subj=="7" & d$block=="3"])

fast= d[d$tempo=="25ms",]    #transform RT in efftones for fast and slow tempo
fast$tone= round(fast$RTadj/0.025, 0)
slow= d[d$tempo=="75ms",]
slow$tone= round(slow$RTadj/0.075, 0)
med= d[d$tempo=="50ms",]
med$tone= round(med$RTadj/0.050, 0)
d<-rbind(fast, slow, med)
########
d.or = d
d <- ddply(d, .(cyc,tempo, condi), transform, sd2pos = mean(RTadj)+2*sd(RTadj))  # compute SD neg and pos for each cond across all subjects
d <- ddply(d, .(cyc,tempo, condi), transform, sd2neg = mean(RTadj)-2*sd(RTadj))  
d <- ddply(d, .(cyc,tempo, condi), transform, above = RTadj>=sd2neg & RTadj<=sd2pos )  
d$above[d$above=="TRUE"]="<2SD"  #attribute logical if within 2SD from mean per each cond and block
d$above[d$above=="FALSE"]="All"
a=ggplot(d[d$cond=="RANDREG",], aes(RTadj, fill=above, color=above))+ facet_grid(~cyc*tempo)+
  geom_histogram(breaks=seq(-0.8,5, by=0.25))+theme_bw()+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.title = element_text(size=10),
        legend.text = element_text(size = 10))+ xlab("Baselined RT (s)")+ggtitle("")+
  scale_color_manual(values=c("black", "black"))+
  scale_fill_manual(values=c("orange", "grey"))
  #ggtitle("RTs over all trials all subjects")
drt=d
pdf(file=paste0(dir, "PLOT_RT_hist.pdf"), 15,10)
grid.arrange(a, ncol=1)
dev.off()

d = d.or
d <- ddply(d, .(cyc,tempo, condi), transform, sd2pos = mean(tone)+2*sd(tone))  # compute SD neg and pos for each cond across all subjects
d <- ddply(d, .(cyc,tempo, condi), transform, sd2neg = mean(tone)-2*sd(tone))  
d <- ddply(d, .(cyc,tempo, condi), transform, above = tone>=sd2neg & tone<=sd2pos )  
d$above[d$above=="TRUE"]="<2SD"  #attribute logical if within 2SD from mean per each cond and block
d$above[d$above=="FALSE"]="All"
a=ggplot(d[d$cond=="RANDREG",], aes(tone, fill=above, color=above))+ facet_grid(~cyc*tempo)+
  geom_histogram(breaks=seq(-20,70, by=4))+theme_bw()+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.title = element_text(size=10),
        legend.text = element_text(size = 10))+ xlab("Tones")+ggtitle("")+
  scale_color_manual(values=c("black", "black"))+
  scale_fill_manual(values=c("orange", "grey"))
#ggtitle("RTs over all trials all subjects")
pdf(file=paste0(dir, "PLOT_RT_hist_intones.pdf"), 15,10)
grid.arrange(a, ncol=1)
dev.off()

d<-droplevels(d[d$above=="<2SD",])  #discard RT that deviate by more than 2SD from the mean of each condition
d=d[complete.cases(d), ]
summary(d)
f=subset(d, select= -c(correct, error, sd2pos,sd2neg,above )) #dropping columns

write.table(f, file=paste0(dir, "exp_stm.txt"), row.names=T, col.names=T, sep="\t") 


####END of preprocessing

v<- ddply(d, "subj:cyc:tempo:cond", transform, Variability=sd(tone, na.rm = T))
v2<- with(v, aggregate(cbind(Variability) ~ subj+cyc+tempo+cond, FUN="mean"))

means <- aggregate(Variability ~ cyc+tempo+cond, v2, mean)
means$Variability=round(means$Variability, 2)
b=ggplot(v2, aes(cyc, Variability,  group=interaction(cyc,tempo), fill=tempo, color=tempo, label=subj))+facet_wrap(~cond)+
  geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1, position = position_dodge(width = 0.8))+
  geom_boxplot(alpha=0.8, position = position_dodge(width = 0.8), outlier.shape = NA)+theme_bw()+
  geom_point(position = position_jitterdodge(), fill="white", shape=21, size=2)+
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size=12))+
   stat_summary(fun.y=mean, colour="darkgrey", geom="point", 
                shape=18, size=4,show.legend = FALSE, position = position_dodge(width = 0.8)) + 
  geom_hline(yintercept=0, lty=2)+ylab("Variability (tones)")+xlab("Alphabet size")+
  ggtitle("variability in tones")+
  scale_fill_jco()+
  scale_color_jco()+
  #guides(fill = guide_legend(title="Predictability", title.theme = element_text(size=12, angle=0)))+
  theme(legend.text = element_text(size = 12,angle = 0), 
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 0))+ 
  geom_text(data = means, aes(label = Variability, y =Variability + 0.1),position = position_dodge(width = 0.8), color="black")

pdf(file=paste0(dir, "PLOT_RT_Variability_intones2.pdf"), 10, 10)
grid.arrange( b, ncol=1)
dev.off()



#### BoxPlot RTs #######
#subtract RT and tones fist cycle
ten= d[d$cyc=="TEN",]                                
ten$effRT= round(ten$RTadj-0.5, 4)
ten$efftone= round(ten$tone-10, 0)

twe= d[d$cyc=="TWE",]
twe$effRT= round(twe$RTadj-1, 4)
twe$efftone= round(twe$tone-20, 0)
d<-rbind(ten,twe)

ag= aggregate(cbind(RTadj, tone, RTs, dprime, effRT, efftone) ~ cyc+tempo+cond+subj, d, mean)  #TABLE TO PLOT for RT (s)

means <- aggregate(RTadj ~ cyc+tempo, ag[ag$cond=="STEP",], mean)
means$RTadj=round(means$RTadj, 2)
p00=ggplot(ag[ag$cond=="STEP",], aes(cyc, RTadj,  group=interaction(cyc,tempo), fill=tempo, color=tempo, label=subj))+
  geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1, position = position_dodge(width = 0.8))+
  geom_boxplot(alpha=0.8, position = position_dodge(width = 0.8), outlier.shape = NA)+theme_bw()+
  geom_point(position = position_jitterdodge(), fill="white", shape=21, size=2)+
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size=12))+
  stat_summary(fun.y=mean, colour="darkgrey", geom="point", 
               shape=18, size=4,show.legend = FALSE, position = position_dodge(width = 0.8)) + 
 # geom_hline(yintercept=0, lty=2)+ylab("RTs")+xlab("Alphabet size")+
  ylab("RT (s)")+xlab("Alphabet size")+ggtitle("STEP condition (sec)")+
  scale_fill_jco()+
  scale_color_jco()+
  scale_y_continuous(breaks=seq(0,1,0.05))+
  theme(legend.text = element_text(size = 12,angle = 0), 
        axis.title.y = element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12, angle = 0))+ 
  geom_text(data = means, aes(label = RTadj, y = RTadj + 0.01),position = position_dodge(width = 0.8), color="black")

pdf(file=paste0(dir, "PLOT_STEPvar.pdf"), 15,7)
grid.arrange(p00, p1, ncol=2)
dev.off()

means <- aggregate(efftone ~ cyc+tempo, ag[ag$cond=="RANDREG",], mean)
means$efftone=round(means$efftone, 1)
p2=ggplot(ag[ag$cond=="RANDREG",], aes(cyc, efftone,  group=interaction(cyc,tempo), fill=tempo, color=tempo, label=subj))+
  geom_text(aes(label=subj), vjust = -0.3, hjust = 0.1, position = position_dodge(width = 0.8))+
  geom_boxplot(alpha=0.8, position = position_dodge(width = 0.8), outlier.shape = NA)+theme_bw()+
  geom_point(position = position_jitterdodge(), fill="white", shape=21, size=2)+
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size=12))+
  stat_summary(fun.y=mean, colour="darkgrey", geom="point", 
               shape=18, size=4,show.legend = FALSE, position = position_dodge(width = 0.8)) + 
  geom_hline(yintercept=0, lty=2)+ylab("Tones")+xlab("Alphabet size")+
  scale_fill_jco()+
  scale_color_jco()+
  scale_y_continuous(breaks=seq(0,42,4))+theme(legend.position='none')+
  ggtitle("RANDREG condition (tones)")+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.title = element_text(size=8),
        legend.text = element_text(size = 8))+
  geom_text(data = means, aes(label = efftone, y = efftone + 5),position = position_dodge(width = 0.8), color="black")
pdf(file=paste0(dir,"PLOT_RT_trans.pdf"), 7,7)
grid.arrange(p2, ncol=1)
dev.off()
########

####ggplot bar plot RTs  ########
b11a=ggplot(ag[ag$cond=="RANDREG",], aes(cyc, efftone, group=tempo, fill=tempo, color=tempo))+theme_bw()+
  stat_summary(fun.y=mean, geom="bar", alpha=0.8, color= "white", position = position_dodge(width = 0.9))+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0.1, alpha=1, position = position_dodge(width = 0.9))+
  scale_fill_jco()+
  scale_color_jco()+
  xlab("Alphabet size") + ylab("Response times (N tones)")+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=16),
        axis.title=element_text(size=16),legend.title = element_text(size=16),
        legend.text = element_text(size = 16))
pdf(file=paste0(dir,"Fig1_RT_barplot.pdf"), 8,6)
grid.arrange(b11a, ncol=1)
dev.off()

#######


####dprime #######

dprime= droplevels(ag[ag$cond=="RANDREG",])
p3=ggplot(dprime, aes(cyc, dprime, group=tempo, fill=tempo, color=tempo))+theme_bw()+
  stat_summary(fun.y=mean, geom="bar", alpha=0.8, color= "black",  position = position_dodge(width = 0.9))+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0.1, alpha=0.6, position = position_dodge(width = 0.9))+
  scale_fill_jco()+
  scale_color_jco()+
  xlab("Alphabet size") + ylab("d'")+
  scale_y_continuous(breaks=seq(0,5,0.5))+
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size=12))+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.title = element_text(size=12),
        legend.text = element_text(size = 12))+ ggtitle("d'")

p3a=ggplot(dprime, aes(cyc, dprime, group=tempo, fill=tempo, color=tempo))+theme_bw()+
  stat_summary(fun.y=mean, geom="bar", alpha=0.8, color= "white", position = position_dodge(width = 0.9))+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0.1, alpha=1, position = position_dodge(width = 0.9))+
  scale_color_manual(values=c("coral1", "lightblue", "red"))+
  scale_fill_manual(values=c("coral1", "lightblue", "red"))+
  coord_cartesian(ylim=c(0,4.5))+
  xlab("Alphabet size") + ylab("d'")+
  theme(axis.line = element_line(colour = "black"), axis.text=element_text(size=16),
        axis.title=element_text(size=16),legend.title = element_text(size=16),
        legend.text = element_text(size = 16))

pdf(file="Fig2_dprime.pdf", 8,6)
grid.arrange(p3a, ncol=1)
dev.off()

###########


#######ANALYSIS#####
# contrasts(d$cyc) <- contr.sum
# contrasts(d$tempo) <-contr.sum
d$block=as.integer(d$block)
d$trialN=as.integer(d$trialN)
head(d)
st<-droplevels(d[d$cond=="RANDREG",])

#mixed model ######
m1 = lmer(tone~cyc+tempo+(cyc:tempo)+block+(1+cyc:tempo|subj) + (1|setID), data = st, REML = TRUE, lmerControl(optCtrl = list(maxfun = 2e5))) 
Anova(m1, type=3) #
xtable(coef(summary(m1)))

ef <- effect("cyc:tempo", m1) #put in the same order as in the model
f <- as.data.frame(ef)
p1=ggplot(f, aes(tempo, fit, group=cyc, color=cyc)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se),width=0.4) + theme_bw(base_size=12)+
  ggtitle("model estimates")

emmeans(m1,  pairwise ~ cyc|tempo, adjust='bonferroni', type="response", lmer.df = "satterthwaite")


##ACCURACY
m4 = glmer(correct~cyc+tempo+cyc:tempo+block+(1+cyc:tempo|subj) + (1|setID), data = dat, family="binomial") 
Anova(m4, type=3) # 
xtable(coef(summary(m2)))

dprime= with(st, aggregate(cbind(dprime)~subj+tempo+cyc, FUN="mean"))
m2 = lm(dprime~cyc*tempo, data = dprime) 
Anova(m2, type=3) # pred*Trial, sliking*Trial
xtable(coef(summary(m2)))
#######
