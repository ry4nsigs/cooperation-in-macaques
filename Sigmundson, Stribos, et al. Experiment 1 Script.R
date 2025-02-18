#importing data and creating data frame
#set working drive then substitute .csv with complete data file. Prepared .csv file used in analyses available upon request.
AllData<-read.csv("Macaques.csv",header=TRUE)
sessions<-AllData[,"Session"]
wait<-AllData[,"Waiting"]
loop<-AllData[,"Loop"]
success<-AllData[,"Success"]
pull<-AllData[,"Pull"]
popularity<-AllData[,"PresenceNo"]
subject<-AllData[,"Subject"]
trial<-AllData[,"Trial"]
Data<-data.frame(Success=success,Ind=subject,Pull=pull,Trial=trial,Sessions=sessions,Wait=wait,LoopPresence=loop,PresNo=popularity)

#running binomial GLMs to test main hypotheses
library(lme4)
waitGLM<-glmer(Wait~scale(Sessions)+(1|Ind/Pull),data=Data,family=binomial)
successGLM<-glmer(Success~scale(Sessions)+(1|Ind/Pull),data=Data,family=binomial)
loopPresGLM<-glmer(LoopPresence~scale(Sessions)+(1|Ind/Pull),data=Data,family=binomial)
summary(waitGLM)
summary(successGLM)
summary(loopPresGLM)

#comparing GLM results to null models
waitNullGLM<-glmer(Wait~(1|Ind/Pull),data=Data,family=binomial)
successNullGLM<-glmer(Success~(1|Ind/Pull),data=Data,family=binomial)
loopPresNullGLM<-glmer(LoopPresence~(1|Ind/Pull),data=Data,family=binomial)
anova(waitGLM,waitNullGLM)
anova(successGLM,successNullGLM)
anova(loopPresGLM,loopPresNullGLM)

#correcting "attempts in presence of partner" for presence likelihood increase
loopPresGLM<-glmer(LoopPresence~scale(Sessions)+PresNo+(1|Ind/Pull),data=Data,family=binomial)
loopPresNullGLM<-glmer(LoopPresence~PresNo+(1|Ind/Pull),data=Data,family=binomial)
summary(loopPresGLM)
anova(loopPresGLM,loopPresNullGLM)

#creating pivot tables
library(dplyr)
successBySession<-Data %>% group_by(Sessions) %>% summarize(SuccessProportion=sum(Success)/sum(Pull/Pull))
attemptsInLoopPresence<-Data %>% group_by(Sessions) %>% summarize(PresenceAttempts=sum(LoopPresence)/sum(Pull/Pull))
waitsBySession<-Data %>% group_by(Sessions) %>% summarize(WaitInstances=sum(Wait))
waitsByIndividual<-Data %>% group_by(Ind) %>% summarize(WaitInstances=sum(Wait))

#plotting pivot tables
library(ggplot2)
ggplot(successBySession,aes(Sessions,SuccessProportion))+geom_jitter()+geom_smooth(method=lm,color="deepskyblue4")+labs(x="Session",y="Proportion of Successful Pulls")+coord_cartesian(ylim=c(0,1.0))+scale_x_continuous(breaks=seq(10,125,by=25))+theme_classic()
ggplot(attemptsInLoopPresence,aes(Sessions,PresenceAttempts*-1+1))+geom_jitter()+geom_smooth(method=lm,color="deepskyblue4")+theme_classic()+labs(x="Session",y="Proportion of Attempts without Partner at Loop")+scale_x_continuous(breaks=seq(10,125,by=25))
ggplot(waitsBySession,aes(Sessions,WaitInstances))+geom_col()+geom_smooth(method=lm,color="deepskyblue4")+theme_classic()+labs(x="Session",y="Instances of Waiting")+coord_cartesian(ylim=c(0,5))+scale_x_continuous(breaks=seq(10,125,by=25))
ggplot(waitsByIndividual,aes(Ind,WaitInstances))+geom_bar(stat="identity",color="black")+theme_classic()+labs(x="Individual",y="Total Instances of Waiting")

#running cohen's k tests
library(irr)
#substitute .csv file with all pull data from both observers side-by-side.
PullData<-read.csv("PullAgreement.csv",header=TRUE)
pull<-PullData[,7:8]
attempt<-kappa2(pull[,c(1,2)], "unweighted")

#substitute .csv file excluding cells with non-matching pulls. Prepared file can be provided upon request.
AllData<-read.csv("CleanedAgreement.csv",header=TRUE)
data<-AllData[,5:18]
wait<-kappa2(data[,c(5,6)], "unweighted")
present<-kappa2(data[c(7,8)], "unweighted")
partner<-kappa2(data[c(9,10)], "unweighted")