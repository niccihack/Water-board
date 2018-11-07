
#Trash Data Dive Data prep

library(dplyr)
library(readr)

#####Schools#####
Copy_of_rptmonthlyposting_1 -> schools
names(schools) = schools[1,]
schools = schools[-1,]
schools = tidyr::separate(schools, SchoolAddress, c("address","zip"), "CA")
schools = mutate(schools, ID = paste(SchoolName, zip, sep = ''))

Copy_of_CDESchoolDirectoryExport -> addresses
addresses = mutate(addresses, ID = paste(School, `Street Zip`))

mod = left_join(addresses, schools, by = 'ID')[,1:18]
write.csv(mod, file = "CA-schools.csv")

Disposal_ADC <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 1)[,1:15]
write.csv(Disposal_ADC, file = "Disposal_ADC.csv")
Disposal_AIC <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 2)[,1:15]
write.csv(Disposal_AIC, file = "Disposal_AIC.csv")
Disposal_ADC_jurisdiction <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 3)[,1:15]
write.csv(Disposal_ADC_jurisdiction, file = "Disposal_ADC_jurisdiction.csv")
Disposal_AIC_jurisdiction <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 4)[,1:15]
write.csv(Disposal_AIC_jurisdiction, file = "Disposal_AIC_jurisdiction.csv")
Jurisdiction_Transformation <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 5)[,1:5]
write.csv(Jurisdiction_Transformation, file = "Jurisdiction_Transformation.csv")
Disposal_Exports <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 6)[,1:6]
Other_beneficial_reuse <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 7)[,1:14]
write.csv(Other_beneficial_reuse, file = "Other_beneficial_reuse.csv")
Disposal_Biomass <- readxl::read_excel("~/Trash/Disposal Data.xlsx", sheet = 8)[,1:3]
write.csv(Disposal_Biomass, file = "Disposal_Biomass.csv")
Station_Tons <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 9)[,1:6]
write.csv(Station_Tons, file = "Station_Tons.csv")
Trash_Transfers <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 10)[,1:7]
write.csv(Trash_Transfers, file = "Trash_Transfers.csv")
Station_Allocation <- read_excel("~/Trash/Disposal Data.xlsx", sheet = 11)[,1:11]
write.csv(Station_Allocation, file = "Station_Allocation.csv")

####Questions####
trashers = read_csv("C:/Users/NHack/Downloads/Questions for trash experts (Responses) - Form Responses 1.csv")
names(trashers) = c('time', 'name', 'gender', 'job', 'company', 'role','expertise','data', 'upload', 'participation')
trashers = trashers[,c(2:8,10)]

nerds = read_csv("C:/Users/NHack/Downloads/Questions for Data Scientists (Responses) - Form Responses 1.csv")
names(nerds) = c('time','name','gender','job','company','programming','statistics','wrangling','viz_com','db_manag','software','GIS','ML','intuition','pub_policy','big_data','participation','trash')
merged = dplyr::bind_rows(trashers, nerds[,2:18]) 
merged = tidyr::separate(merged, name, c("First_Name","Last_Name"), " ")

eventbrite <- read_csv("C:/Users/NHack/Downloads/report-2018-11-05T1644.csv")
names(eventbrite) = c('order','First_Name','Last_Name','email','tic_type','group','gender','role','role_other','expertise','data','upload','participation','rate','programming','statistics','ML','wrangling','viz_com','db_manag','software','intuition','pub_policy','big_data','GIS','trash','job','company')
eventbrite = eventbrite[,c(2:5,27,28,7:26)]
questions = eventbrite %>% dplyr::filter(!is.na(job))
check = eventbrite %>% dplyr::filter(is.na(job))
final = dplyr::union(eventbrite, merged)
#need to replace NAs in eventbrite with answers from merged by name
#use dplyr::coalesce or dplyr::case_when() 

final = dplyr::bind_rows(merged, eventbrite)
trash_final = dplyr::filter(final, tic_type == 'Participant - Trash Expert')
dups = final[duplicated(final$Last_Name),]
#need to merge question responses and tix type
