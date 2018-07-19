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


