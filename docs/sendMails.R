
library(sendmailR)
library(RDCOMClient)
i<-1
#personalized_info <- personalized_info[88:190,]


# for (i in 1:nrow(personalized_info)){
# attachmentPath <- gsub(" ", "", paste('C:/Users/BiancaClavio/Documents/stats-on-grades/docs/handouts/SSPanalysis_', 87+i,'.pdf'))
# name <- personalized_info$name[i]
# coordinator <- ifelse(personalized_info$Campus[i] == 'AAL', "Hendrik (hk@create.aau.dk)", "Jon (jpe@create.aau.dk)")
# mailBody <- paste("Unfortunately, I had sent you the previous mail prematurely, and you received an individual report of another student by mistake.     
# I had simply forgotten to change a value in my mailing list for loop. Please, find your personal report attached here.
# 
# 
# Dear",name, "
# 
# In September, you participated in the Study Verification Test (SSP), in which you answered a series of questions on Moodle. We collected this information to better understand your hopes, expectations, and worries about student life at AAU in general and about Medialogy in particular. We use the information to improve your study environment and to reach out and provide individual support to those of you who seek or need it to adjust to university life and master your chosen programme.
# We have analysed your responses in your respective cohort; more specifically within the first semester students in Aalborg and Copenhagen in 2017. One major outcome of this effort is the attached student report. It provides personalized feedback on important factors for finishing a university degree as well as specific recommendations and links to AAU resources that can be of help.
# Currently only the semester coordinator, supervisors, and the study board have access to this information. But you are welcome to share your student reports with peers, study counsellors, teachers, or others - should you desire to do so. Depending on individual results and needs we might approach you again in the future.
# 
# Please contact",coordinator,"via email and add Bianca in CC (bcch@create.aau.dk) if you have any questions.
# 
# Best regards,
# Jon, Bianca, and Hendrik
# 
# ")
# ## init com api
# OutApp <- COMCreate("Outlook.Application")
# ## create an email
# outMail = OutApp$CreateItem(0)
# ## configure  email parameter
# outMail[["To"]] = as.character(personalized_info$email[i])
# outMail[["Cc"]] = ifelse(personalized_info$Campus[i] == 'AAL', "hk@create.aau.dk", "jpe@create.aau.dk")
# outMail[["subject"]] = ifelse(personalized_info$Campus[i] == 'AAL', "SSP-AAL’17: Individual student feedback", "SSP-CPH’17: Individual student feedback")
# outMail[["body"]] = mailBody
# outMail[["Attachments"]]$Add(attachmentPath)
# outMail$Send()
# }
