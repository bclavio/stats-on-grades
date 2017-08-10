

missingRows<- dfAAUGradesWODistEnrol[!dfAAUGradesWODistEnrol$rowID %in% dfAAUMarriedGrades$rowID ,]
missingActivities<-data.frame(missingRows[!missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ]$aktivitetText)

missingRows1<-data.frame(missingRows[missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ])
missingRows1<-missingRows1[!is.na(missingRows1$slutdatosn), ]
missingRows1<-missingRows1[!missingRows1$statussn=="afbrudt", ]

reexams<-dfAAUGradesWODistEnrol[c(dfAAUGradesWODistEnrol$Forsoeg.nr.>1 | dfAAUGradesWODistEnrol$Sidste.Fors.=="Nej") , ]
#retained<-dfAAUGradesWODistEnrol[c(dfAAUGradesWODistEnrol$fradatosn > | dfAAUGradesWODistEnrol$Sidste.Fors.=="Nej") , ]

dfAAUGradesWODistEnrol1$fradatosn<-dfAAUGradesWODistEnrol1$fradatosn[dfAAUGradesWODistEnrol1$Sidste.Fors.=="Nej"]
#dfAAUGradesWODistEnrol$fradatosn[ifelse(dfAAUGradesWODistEnrol$Sidste.Fors.=="Ja" & dfAAUGradesWODistEnrol$Forsoeg.nr.>1, 1,0)

