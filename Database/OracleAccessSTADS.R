library(RJDBC)
#the script assumes that you have a config.R file in your libloc folder that holds the STADSuser and STADSpass strings. 
#STADSuser<-"AAU$HK" 
#STADSpass<-"somePaSSWORD" #not shown here
source(paste(libloc,"//config.R",sep='')) #ON WINDOWS use "\\config.R"
jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="/Applications/oracle/ojdbc6.jar")
jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora-stads-stdb.srv.aau.dk:1521/STAPSB", STADSuser, STADSpass)
#table1=dbReadTable(jdbcConnection,'vueudv.aau_stamdata_hdk') #not working?
#table2=dbGetQuery(jdbcConnection,'select * from table1 where name=\'string\'')
tabletest<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stamdata_hdk')
#listOfTables<-dbListTables(jdbcConnection)
#listOfTables<-as.data.frame(listOfTables)
stamdata_hdk<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stamdata_hdk')

dbReadTable(jdbcConnection,'vueudv.beg_uddramme')
beg_uddelement<-dbReadTable(jdbcConnection,"vueudv.beg_uddelement")

beg_uddelement<-dbGetQuery(jdbcConnection,'select * from vueudv.beg_uddelement')


