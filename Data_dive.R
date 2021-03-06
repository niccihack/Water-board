
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
eventbrite <- read_csv("C:/Users/NHack/Downloads/report-2018-11-15T1043.csv")
names(eventbrite) = c('order','First_Name','Last_Name','email','tic_type','gender','role','role_other','expertise','data','upload','participation','rate','programming','statistics','ML','wrangling','viz_com','db_manag','software','intuition','pub_policy','big_data','GIS','trash','job','company')
eventbrite = eventbrite[,c(2:5,26,27,6:25)]

#Trash Experts#
trashers = read_csv("C:/Users/NHack/Downloads/Questions for trash experts (Responses) - Form Responses 1.csv")
names(trashers) = c('time', 'name', 'gender', 'job', 'company', 'roles','expertise','data', 'upload', 'participation')
trashers = trashers %>% 
  dplyr::select(c(2:10)) %>%
  tidyr::separate(name, c("First_Name","Last_Name"), " ", extra = 'merge')

eventbrite_trash = eventbrite %>% 
  dplyr::filter(tic_type == 'Participant - Trash Expert') %>% 
  dplyr::filter(!is.na(company) | !is.na(gender)) %>% 
  dplyr::select(1:13)
clean_eb_trash = eventbrite_trash %>% 
  dplyr::mutate(roles = ifelse(role == 'Other', role_other, role)) %>%
  dplyr::select(1:7,14,10:13)

merged_trash = dplyr::bind_rows(trashers, clean_eb_trash)
#Replace NAs in email and tic_type with eventbrite info
final_trash = left_join(dplyr::select(merged_trash, 1:10), eventbrite[1:4], by = c('First_Name','Last_Name'))
final_trash = final_trash[!duplicated(final_trash),]
write.csv(final_trash, file = "DataDive_TrashExperts_questions20181108.csv")


#Data Scientist#
nerds = read_csv("C:/Users/NHack/Downloads/Questions for Data Scientists (Responses) - Form Responses 1.csv")
names(nerds) = c('time','name','gender','job','company','programming','statistics','wrangling','viz_com','db_manag','software','GIS','ML','intuition','pub_policy','big_data','participation','trash')
nerds = nerds[,2:18]
nerds = tidyr::separate(nerds, name, c("First_Name","Last_Name"), " ", extra = 'merge')

eventbrite_data = eventbrite %>% 
  dplyr::filter(tic_type == 'Participant - Data Scientist') %>% 
  dplyr::filter(!is.na(company) | !is.na(gender)) %>% 
  dplyr::select(1:7,11:26)

merged_data = dplyr::bind_rows(nerds, eventbrite_data)
#Replace NAs in email and tic_type with eventbrite info
final_data = left_join(dplyr::select(merged_data, 1:18), eventbrite[1:4], by = c('First_Name','Last_Name'))
final_data = final_data[!duplicated(final_data),]
write.csv(final_data, file = "DataDive_DataScientist_questions20181108.csv")


##Lunch##
lunch <- read_csv("C:/Users/NHack/Downloads/2018 CA Trash Data Dive attendance (Responses) - Form Responses 1.csv")
name_tags = left_join(lunch, eventbrite[,c(1,2,4)], by = c('First_Name','Last_Name')) %>%
  dplyr::select(First_Name, Last_Name, tic_type) %>%
  dplyr::distinct(Last_Name, First_Name, .keep_all = T) %>%
  dplyr::filter(!is.na(Last_Name))
sum_tags = name_tags %>% group_by(tic_type) %>%
  dplyr::summarise(tixs = n())
write.csv(name_tags, file = 'DataDive_nametags.csv')

lunch = lunch %>% tidyr::separate(Name, c("First_Name","Last_Name"), " ", extra = 'merge')
trash_lunch = left_join(final_trash, lunch, by = c('First_Name','Last_Name')) %>% 
  dplyr::filter(!is.na(Timestamp)) %>% 
  dplyr::distinct(Last_Name, First_Name, .keep_all = T)
nerds_lunch = left_join(final_data, lunch, by = c('First_Name','Last_Name')) %>% 
  dplyr::filter(!is.na(Timestamp)) %>% 
  dplyr::distinct(Last_Name, First_Name, .keep_all = T)
merged_lunch = dplyr::bind_rows(dplyr::select(trash_lunch, Last_Name, First_Name, email, tic_type), dplyr::select(nerds_lunch, Last_Name, First_Name, email, tic_type))
write.csv(merged_lunch, file = 'DataDive_completed.csv')

