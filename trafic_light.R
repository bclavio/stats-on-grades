library(ggplot2)
library(rpart)
library(rpart.plot)

MedDataBSc <- read.csv("Y:/analysis_data/dropOut/data/MedDataBSc2012-2016.csv", stringsAsFactors=FALSE, encoding = 'ANSI')

MedDataBSc <- MedDataBSc[MedDataBSc$startaar%in%c(2013,2014),]
MedDataBSc <- MedDataBSc[MedDataBSc$statussn!='orlov',]
MedDataBSc$dropout <- ifelse(MedDataBSc$statussn=='afbrudt','dropout','active/finished')

merit <- read.csv('Y:/analysis_data/dropOut/data_2017cohortCPHAAL/2008-18_merit_MED.csv', stringsAsFactors = F, encoding = 'UTF-8')
names(merit)[1] <- 'person_id'

ID <- read.csv('Y:/analysis_data/dropOut/data_2017cohortCPHAAL/2008-18_ram_op_MED.csv', stringsAsFactors = F, encoding = 'UTF-8')
ID <- ID[,c(3,40)]
ID <- ID[!duplicated(ID),]

test <- merge(MedDataBSc,ID, by='studienr',all.x = T)

merit <- merit[merit$person_id%in%test$PERSON_ID,]

test$X2_T_nom_insem <- test$X2_T_nom - test$X1_T_nom
test$X3_T_nom_insem <- test$X3_T_nom - test$X2_T_nom
test$X4_T_nom_insem <- test$X4_T_nom - test$X3_T_nom
test$X5_T_nom_insem <- test$X5_T_nom - test$X4_T_nom
test$X6_T_nom_insem <- test$X6_T_nom - test$X5_T_nom

test$X2_NT_nom_insem <- test$X2_NT_nom - test$X1_NT_nom
test$X3_NT_nom_insem <- test$X3_NT_nom - test$X2_NT_nom
test$X4_NT_nom_insem <- test$X4_NT_nom - test$X3_NT_nom
test$X5_NT_nom_insem <- test$X5_NT_nom - test$X4_NT_nom
test$X6_NT_nom_insem <- test$X6_NT_nom - test$X5_NT_nom

test$X2_El_nom_insem <- test$X2_El_nom - test$X1_El_nom
test$X3_El_nom_insem <- test$X3_El_nom - test$X2_El_nom
test$X4_El_nom_insem <- test$X4_El_nom - test$X3_El_nom
test$X5_El_nom_insem <- test$X5_El_nom - test$X4_El_nom
test$X6_El_nom_insem <- test$X6_El_nom - test$X5_El_nom

test$X2_Pr_nom_insem <- test$X2_Pr_nom - test$X1_Pr_nom
test$X3_Pr_nom_insem <- test$X3_Pr_nom - test$X2_Pr_nom
test$X4_Pr_nom_insem <- test$X4_Pr_nom - test$X3_Pr_nom
test$X5_Pr_nom_insem <- test$X5_Pr_nom - test$X4_Pr_nom
test$X6_Pr_nom_insem <- test$X6_Pr_nom - test$X5_Pr_nom

names(table(merit$aktivitet_navn))

test$X3_NT_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='A/V produktion']] <- test$X3_NT_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='A/V produktion']]-5
test$X1_NT_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Animation og grafisk design']] <- test$X1_NT_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Animation og grafisk design']]-5
test$X1_NT_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Audio-Visuel Sketching']] <- test$X1_NT_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Audio-Visuel Sketching']]-5
test$X1_T_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Grundlæggende programmering']] <- test$X1_T_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Grundlæggende programmering']]-5
test$X2_NT_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Interaktionsdesign']] <- test$X2_NT_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Interaktionsdesign']]-5
test$X1_Pr_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Kreativ leg - teknologisk udfo']] <- test$X1_Pr_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Kreativ leg - teknologisk udfo']]-5
test$X2_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Matematik til multimedie-appli']] <- test$X2_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Matematik til multimedie-appli']]-5
test$X4_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Objektorienteret Software Engi']] <- test$X4_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Objektorienteret Software Engi']]-5
test$X1_NT_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Problembaseret læring i videns']] <- test$X1_NT_nom[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Problembaseret læring i videns']]-5
test$X3_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Proceduremæssig programmering']] <- test$X3_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Proceduremæssig programmering']]-5
test$X2_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Programmering af interaktive s']] <- test$X2_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Programmering af interaktive s']]-5
test$X3_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Programmering af komplekse sof']] <- test$X3_T_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Programmering af komplekse sof']]-5
test$X5_NT_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Screen Media']] <- test$X5_NT_nom_insem[test$PERSON_ID%in%merit$person_id[merit$aktivitet_navn=='Screen Media']]-5

test$X2_T_nom <- test$X1_T_nom+test$X2_T_nom_insem
test$X3_T_nom <- test$X2_T_nom+test$X3_T_nom_insem
test$X4_T_nom <- test$X3_T_nom+test$X4_T_nom_insem
test$X5_T_nom <- test$X4_T_nom+test$X5_T_nom_insem
test$X6_T_nom <- test$X5_T_nom+test$X6_T_nom_insem

test$X2_NT_nom <- test$X1_NT_nom+test$X2_NT_nom_insem
test$X3_NT_nom <- test$X2_NT_nom+test$X3_NT_nom_insem
test$X4_NT_nom <- test$X3_NT_nom+test$X4_NT_nom_insem
test$X5_NT_nom <- test$X4_NT_nom+test$X5_NT_nom_insem
test$X6_NT_nom <- test$X5_NT_nom+test$X6_NT_nom_insem

test$X2_El_nom <- test$X1_El_nom+test$X2_El_nom_insem
test$X3_El_nom <- test$X2_El_nom+test$X3_El_nom_insem
test$X4_El_nom <- test$X3_El_nom+test$X4_El_nom_insem
test$X5_El_nom <- test$X4_El_nom+test$X5_El_nom_insem
test$X6_El_nom <- test$X5_El_nom+test$X6_El_nom_insem

test$X2_Pr_nom <- test$X1_Pr_nom+test$X2_Pr_nom_insem
test$X3_Pr_nom <- test$X2_Pr_nom+test$X3_Pr_nom_insem
test$X4_Pr_nom <- test$X3_Pr_nom+test$X4_Pr_nom_insem
test$X5_Pr_nom <- test$X4_Pr_nom+test$X5_Pr_nom_insem
test$X6_Pr_nom <- test$X5_Pr_nom+test$X6_Pr_nom_insem

MedDataBSc <- test[,-c(193:2013)]

MedDataBSc$X1_all_atpIn <- rowSums(MedDataBSc[,c('X1_T_atpIn','X1_NT_atpIn','X1_Pr_atpIn','X1_El_atpIn')],na.rm = T)
MedDataBSc$X2_all_atpIn <- rowSums(MedDataBSc[,c('X2_T_atpIn','X2_NT_atpIn','X2_Pr_atpIn','X2_El_atpIn')],na.rm = T)
MedDataBSc$X3_all_atpIn <- rowSums(MedDataBSc[,c('X3_T_atpIn','X3_NT_atpIn','X3_Pr_atpIn','X3_El_atpIn')],na.rm = T)
MedDataBSc$X4_all_atpIn <- rowSums(MedDataBSc[,c('X4_T_atpIn','X4_NT_atpIn','X4_Pr_atpIn','X4_El_atpIn')],na.rm = T)
MedDataBSc$X5_all_atpIn <- rowSums(MedDataBSc[,c('X5_T_atpIn','X5_NT_atpIn','X5_Pr_atpIn','X5_El_atpIn')],na.rm = T)


MedDataBSc$X1_all_atnBy <- rowSums(MedDataBSc[,c('X1_T_atnBy','X1_NT_atnBy','X1_Pr_atnBy','X1_El_atnBy')],na.rm = T)
MedDataBSc$X2_all_atnBy <- rowSums(MedDataBSc[,c('X2_T_atnBy','X2_NT_atnBy','X2_Pr_atnBy','X2_El_atnBy')],na.rm = T)
MedDataBSc$X3_all_atnBy <- rowSums(MedDataBSc[,c('X3_T_atnBy','X3_NT_atnBy','X3_Pr_atnBy','X3_El_atnBy')],na.rm = T)
MedDataBSc$X4_all_atnBy <- rowSums(MedDataBSc[,c('X4_T_atnBy','X4_NT_atnBy','X4_Pr_atnBy','X4_El_atnBy')],na.rm = T)
MedDataBSc$X5_all_atnBy <- rowSums(MedDataBSc[,c('X5_T_atnBy','X5_NT_atnBy','X5_Pr_atnBy','X5_El_atnBy')],na.rm = T)

MedDataBSc$X1_all_nom <- rowSums(MedDataBSc[,c('X1_T_nom','X1_NT_nom','X1_Pr_nom','X1_El_nom')],na.rm = T)
MedDataBSc$X2_all_nom <- rowSums(MedDataBSc[,c('X2_T_nom','X2_NT_nom','X2_Pr_nom','X2_El_nom')],na.rm = T)
MedDataBSc$X3_all_nom <- rowSums(MedDataBSc[,c('X3_T_nom','X3_NT_nom','X3_Pr_nom','X3_El_nom')],na.rm = T)
MedDataBSc$X4_all_nom <- rowSums(MedDataBSc[,c('X4_T_nom','X4_NT_nom','X4_Pr_nom','X4_El_nom')],na.rm = T)
MedDataBSc$X5_all_nom <- rowSums(MedDataBSc[,c('X5_T_nom','X5_NT_nom','X5_Pr_nom','X5_El_nom')],na.rm = T)

MedDataBSc$X1_all_missing <- MedDataBSc$X1_all_nom-MedDataBSc$X1_all_atnBy
MedDataBSc$X2_all_missing <- MedDataBSc$X2_all_nom-MedDataBSc$X2_all_atnBy
MedDataBSc$X3_all_missing <- MedDataBSc$X3_all_nom-MedDataBSc$X3_all_atnBy
MedDataBSc$X4_all_missing <- MedDataBSc$X4_all_nom-MedDataBSc$X4_all_atnBy
MedDataBSc$X5_all_missing <- MedDataBSc$X5_all_nom-MedDataBSc$X5_all_atnBy

MedDataBSc$X1_T_missing <- MedDataBSc$X1_T_nom-MedDataBSc$X1_T_atnBy
MedDataBSc$X2_T_missing <- MedDataBSc$X2_T_nom-MedDataBSc$X2_T_atnBy
MedDataBSc$X3_T_missing <- MedDataBSc$X3_T_nom-MedDataBSc$X3_T_atnBy
MedDataBSc$X4_T_missing <- MedDataBSc$X4_T_nom-MedDataBSc$X4_T_atnBy
MedDataBSc$X5_T_missing <- MedDataBSc$X5_T_nom-MedDataBSc$X5_T_atnBy

MedDataBSc$X1_NT_missing <- MedDataBSc$X1_NT_nom-MedDataBSc$X1_NT_atnBy
MedDataBSc$X2_NT_missing <- MedDataBSc$X2_NT_nom-MedDataBSc$X2_NT_atnBy
MedDataBSc$X3_NT_missing <- MedDataBSc$X3_NT_nom-MedDataBSc$X3_NT_atnBy
MedDataBSc$X4_NT_missing <- MedDataBSc$X4_NT_nom-MedDataBSc$X4_NT_atnBy
MedDataBSc$X5_NT_missing <- MedDataBSc$X5_NT_nom-MedDataBSc$X5_NT_atnBy

MedDataBSc$X1_Pr_missing <- MedDataBSc$X1_Pr_nom-MedDataBSc$X1_Pr_atnBy
MedDataBSc$X2_Pr_missing <- MedDataBSc$X2_Pr_nom-MedDataBSc$X2_Pr_atnBy
MedDataBSc$X3_Pr_missing <- MedDataBSc$X3_Pr_nom-MedDataBSc$X3_Pr_atnBy
MedDataBSc$X4_Pr_missing <- MedDataBSc$X4_Pr_nom-MedDataBSc$X4_Pr_atnBy
MedDataBSc$X5_Pr_missing <- MedDataBSc$X5_Pr_nom-MedDataBSc$X5_Pr_atnBy

MedDataBSc$X1_TF <- cut(MedDataBSc$X1_all_missing,breaks = c(-1,5,15,30),labels = c('green','yellow','red'))
MedDataBSc$X2_TF <- cut(MedDataBSc$X2_all_missing,breaks = c(-1,5,15,60),labels = c('green','yellow','red'))
MedDataBSc$X3_TF <- cut(MedDataBSc$X3_all_missing,breaks = c(-1,5,15,90),labels = c('green','yellow','red'))
MedDataBSc$X4_TF <- cut(MedDataBSc$X4_all_missing,breaks = c(-1,5,15,120),labels = c('green','yellow','red'))
MedDataBSc$X5_TF <- cut(MedDataBSc$X5_all_missing,breaks = c(-1,5,15,150),labels = c('green','yellow','red'))

MedDataBSc$EndSemester[is.na(MedDataBSc$EndSemester)] <- 7

MedDataBSc.1 <- MedDataBSc[MedDataBSc$EndSemester>1,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X1_T_missing','X1_NT_missing','X1_Pr_missing','X1_all_missing','X1_TF')]
names(MedDataBSc.1)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.1$semester <- 1
MedDataBSc.2 <- MedDataBSc[MedDataBSc$EndSemester>2,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X2_T_missing','X2_NT_missing','X2_Pr_missing','X2_all_missing','X2_TF')]
names(MedDataBSc.2)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.2$semester <- 2
MedDataBSc.3 <- MedDataBSc[MedDataBSc$EndSemester>3,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X3_T_missing','X3_NT_missing','X3_Pr_missing','X3_all_missing','X3_TF')]
names(MedDataBSc.3)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.3$semester <- 3
MedDataBSc.4 <- MedDataBSc[MedDataBSc$EndSemester>4,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X4_T_missing','X4_NT_missing','X4_Pr_missing','X4_all_missing','X4_TF')]
names(MedDataBSc.4)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.4$semester <- 4
MedDataBSc.5 <- MedDataBSc[MedDataBSc$EndSemester>5,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X5_T_missing','X5_NT_missing','X5_Pr_missing','X5_all_missing','X5_TF')]
names(MedDataBSc.5)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.5$semester <- 5

MedDataBSc.allsem <- rbind(MedDataBSc.1,MedDataBSc.2,MedDataBSc.3,MedDataBSc.4,MedDataBSc.5)
MedDataBSc.allsem$startaar <- factor(MedDataBSc.allsem$startaar)
MedDataBSc.allsem$semester <- factor(MedDataBSc.allsem$semester)

#including only students who attempted something on previous semester
MedDataBSc.allactive.1 <- MedDataBSc[MedDataBSc$EndSemester>1 & MedDataBSc$X1_all_atpIn>0,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X1_T_missing','X1_NT_missing','X1_Pr_missing','X1_all_missing','X1_TF')]
names(MedDataBSc.allactive.1)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.allactive.1$semester <- 1
MedDataBSc.allactive.2 <- MedDataBSc[MedDataBSc$EndSemester>2& MedDataBSc$X2_all_atpIn>0,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X2_T_missing','X2_NT_missing','X2_Pr_missing','X2_all_missing','X2_TF')]
names(MedDataBSc.allactive.2)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.allactive.2$semester <- 2
MedDataBSc.allactive.3 <- MedDataBSc[MedDataBSc$EndSemester>3& MedDataBSc$X3_all_atpIn>0,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X3_T_missing','X3_NT_missing','X3_Pr_missing','X3_all_missing','X3_TF')]
names(MedDataBSc.allactive.3)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.allactive.3$semester <- 3
MedDataBSc.allactive.4 <- MedDataBSc[MedDataBSc$EndSemester>4& MedDataBSc$X4_all_atpIn>0,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X4_T_missing','X4_NT_missing','X4_Pr_missing','X4_all_missing','X4_TF')]
names(MedDataBSc.allactive.4)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.allactive.4$semester <- 4
MedDataBSc.allactive.5 <- MedDataBSc[MedDataBSc$EndSemester>5& MedDataBSc$X5_all_atpIn>0,c('fradatosn','slutdatosn','statussn','EndSemester','startaar','Campus','dropout','X5_T_missing','X5_NT_missing','X5_Pr_missing','X5_all_missing','X5_TF')]
names(MedDataBSc.allactive.5)[8:12] <- c('T_missing','NT_missing','Pr_missing','all_missing','TF')
MedDataBSc.allactive.5$semester <- 5

MedDataBSc.allactive.allsem <- rbind(MedDataBSc.allactive.1,MedDataBSc.allactive.2,MedDataBSc.allactive.3,MedDataBSc.allactive.4,MedDataBSc.allactive.5)
MedDataBSc.allactive.allsem$startaar <- factor(MedDataBSc.allactive.allsem$startaar)
MedDataBSc.allactive.allsem$semester <- factor(MedDataBSc.allactive.allsem$semester)

#TF and dropout
#all students who hasn't dropped out
ggplot(data=MedDataBSc.allsem,aes(x=TF,fill=dropout))+
  geom_bar()+
  facet_wrap(~semester,labeller = as_labeller(c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')),scales = 'free_x')+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Count')+
  xlab('Trafic light category')+
  theme(legend.position = c(0.85,0.25))

ggplot(data=MedDataBSc.allsem,aes(x=TF,fill=dropout))+
  geom_bar(position = 'fill')+
  facet_wrap(~semester,labeller = as_labeller(c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')),scales = 'free_x')+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Proportion')+
  xlab('Trafic light category')+
  theme(legend.position = c(0.85,0.25))  

ggplot(data=MedDataBSc.allsem,aes(x=TF,fill=dropout))+
  geom_bar()+
  facet_grid(startaar~semester,labeller =labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')),scales = 'free_x' )+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Count')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allsem,aes(x=TF,fill=dropout))+
  geom_bar(position = 'fill')+
  facet_grid(startaar~semester,labeller = labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')),scales = 'free_x')+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Proportion')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allsem,aes(x=TF,fill=dropout))+
  geom_bar()+
  facet_grid(Campus~semester,labeller =labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Count')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allsem,aes(x=TF,fill=dropout))+
  geom_bar(position = 'fill')+
  facet_grid(Campus~semester,labeller =labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Count')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allsem,aes(x=dropout,fill=TF))+
  geom_bar(position = 'fill')+
  facet_wrap(~semester,labeller = as_labeller(c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')),scales = 'free_x')+
  scale_fill_manual(values = c('forestgreen','gold2','darkred'),name='Trafic light')+
  ylab('Proportion')+
  xlab('Status')+
  theme(legend.position = c(0.8,0.25))


ggplot(data=MedDataBSc.allsem,aes(x=semester,y=all_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS')+
  xlab('Semester')

ggplot(data=MedDataBSc.allsem,aes(x=semester,y=T_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS in technical courses')+
  xlab('Semester')

ggplot(data=MedDataBSc.allsem,aes(x=semester,y=NT_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS in non-technical courses')+
  xlab('Semester')

ggplot(data=MedDataBSc.allsem,aes(x=semester,y=Pr_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS in projects')+
  xlab('Semester')

#For students who actually attempted ECTS as well

ggplot(data=MedDataBSc.allactive.allsem,aes(x=TF,fill=dropout))+
  geom_bar()+
  facet_wrap(~semester,labeller = as_labeller(c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Count')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=TF,fill=dropout))+
  geom_bar(position = 'fill')+
  facet_wrap(~semester,labeller = as_labeller(c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Proportion')+
  xlab('Trafic light category')  

ggplot(data=MedDataBSc.allactive.allsem,aes(x=TF,fill=dropout))+
  geom_bar()+
  facet_wrap(semester~startaar,labeller =labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')) )+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Count')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=TF,fill=dropout))+
  geom_bar(position = 'fill')+
  facet_wrap(~semester+startaar,labeller = labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','darkred'),name='Status')+
  ylab('Proportion')+
  xlab('Trafic light category')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=dropout,fill=TF))+
  geom_bar(position = 'fill')+
  facet_wrap(~semester,labeller = as_labeller(c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','gold2','darkred'),name='Trafic light')+
  ylab('Proportion')+
  xlab('Status')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=dropout,fill=TF))+
  geom_bar(position = 'fill')+
  facet_wrap(~semester+startaar,labeller = labeller(semester=c('1'='After 1. semester','2'='After 2. semester','3'='After 3. semester','4'='After 4. semester','5'='After 5. semester')))+
  scale_fill_manual(values = c('forestgreen','gold2','darkred'),name='Trafic light')+
  ylab('Proportion')+
  xlab('Status')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=semester,y=all_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS')+
  xlab('Semester')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=semester,y=T_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS in technical courses')+
  xlab('Semester')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=semester,y=NT_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS in non-technical courses')+
  xlab('Semester')

ggplot(data=MedDataBSc.allactive.allsem,aes(x=semester,y=Pr_missing,col=dropout))+
  geom_boxplot(position = 'dodge',outlier.shape = NA)+
  geom_count(alpha=0.5,position = position_dodge(0.75))+
  scale_colour_manual(values = c('forestgreen','darkred'),name='Status')+
  scale_size_continuous(name='Count')+
  ylab('Missing ECTS in projects')+
  xlab('Semester')


#Classification trees to determine cutpoint
tree.all.1 <- rpart(dropout~all_missing,data = MedDataBSc.1)
rpart.plot(tree.all.1,main='After 1. semester')
plotcp(tree.all.1)

tree.all.2 <- rpart(dropout~all_missing,data = MedDataBSc.2)
rpart.plot(tree.all.2)
plotcp(tree.all.2)
tree.all.2 <- rpart(dropout~all_missing,data = MedDataBSc.2,cp=0.084)
rpart.plot(tree.all.2,main='After 2. semester')

tree.all.3 <- rpart(dropout~all_missing,data = MedDataBSc.3)
rpart.plot(tree.all.3,main='After 3. semester')
plotcp(tree.all.3)

tree.all.4 <- rpart(dropout~all_missing,data = MedDataBSc.4)
rpart.plot(tree.all.4,main='After 4. semester')
plotcp(tree.all.4)

tree.all.5 <- rpart(dropout~all_missing,data = MedDataBSc.5)
rpart.plot(tree.all.5)
plotcp(tree.all.5)
#use empty tree

tree.category.1 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.1)
rpart.plot(tree.category.1)
tree.category.1$variable.importance
plotcp(tree.category.1)
tree.category.1 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.1,cp=0.12)
rpart.plot(tree.category.1,main='After 1. semester')

tree.category.2 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.2)
rpart.plot(tree.category.2)
tree.category.2$variable.importance
plotcp(tree.category.2)
tree.category.2 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.2,cp=0.084)
rpart.plot(tree.category.2,main='After 2. semester')

tree.category.3 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.3)
rpart.plot(tree.category.3)
tree.category.3$variable.importance
plotcp(tree.category.3)
tree.category.3 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.3,cp=0.091)
rpart.plot(tree.category.3,main='After 3. semester')

tree.category.4 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.4)
rpart.plot(tree.category.4)
tree.category.4$variable.importance
plotcp(tree.category.4)
tree.category.4 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.4,cp=0.17)
rpart.plot(tree.category.4,main='After 4. semester')

tree.category.5 <- rpart(dropout~T_missing+NT_missing+Pr_missing,data = MedDataBSc.5)
rpart.plot(tree.category.5)
tree.category.5$variable.importance
plotcp(tree.category.5)
#empty tree


CVcart <- function(tree,data, k=10){
  accuracy <- rep(0,k)
  FP <- rep(0,k)
  FN <- rep(0,k)
  idx <- sample(1:k,nrow(data),replace = TRUE)
  for (i in 1:k){
    train <- data[idx!=i,]
    test <- data[idx==i,]
    formula <- formula(tree)
    cp <- tree$cptable[nrow(tree$cptable),1]
    fittrain <- rpart(formula, data = train, cp=cp)
    pred <- predict(fittrain,test,type='class')
    accuracy[i] <- mean(pred==test$dropout)
    tab <- table(factor(pred, levels = c('active/finished','dropout')),test$dropout)
    if(sum(tab[,1])==0){FP[i] <- 0}
    else{FP[i] <- tab[2,1]/sum(tab[,1])}
    if(sum(tab[,2])==0){FN[i] <- 0}
    else{FN[i] <- tab[1,2]/sum(tab[,2])}
  }
  return(list('accuracy'=accuracy,'FP'=FP,'FN'=FN))
}

CV.all.1 <- CVcart(tree.all.1,MedDataBSc.1)
mean(CV.all.1$accuracy)
mean(CV.all.1$FP)
mean(CV.all.1$FN)

CV.category.1 <- CVcart(tree.category.1,MedDataBSc.1)
mean(CV.category.1$accuracy)
mean(CV.category.1$FP)
mean(CV.category.1$FN)

CV.all.2 <- CVcart(tree.all.2,MedDataBSc.2)
mean(CV.all.2$accuracy)
mean(CV.all.2$FP)
mean(CV.all.2$FN)

CV.category.2 <- CVcart(tree.category.2,MedDataBSc.2)
mean(CV.category.2$accuracy)
mean(CV.category.2$FP)
mean(CV.category.2$FN)

CV.all.3 <- CVcart(tree.all.3,MedDataBSc.3)
mean(CV.all.3$accuracy)
mean(CV.all.3$FP)
mean(CV.all.3$FN)

CV.category.3 <- CVcart(tree.category.3,MedDataBSc.3)
mean(CV.category.3$accuracy)
mean(CV.category.3$FP)
mean(CV.category.3$FN)

CV.all.4 <- CVcart(tree.all.4,MedDataBSc.4)
mean(CV.all.4$accuracy)
mean(CV.all.4$FP)
mean(CV.all.4$FN)

CV.category.4 <- CVcart(tree.category.4,MedDataBSc.4)
mean(CV.category.4$accuracy)
mean(CV.category.4$FP)
mean(CV.category.4$FN)
