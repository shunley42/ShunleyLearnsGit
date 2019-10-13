

library(tidyverse)
library(vroom)


# Create Working Directory Short Cuts
local_dir <- "/Users/samhunley/Desktop/2018_all_Contracts_Full_20191009"
setwd(local_dir)

# Loading that data
# vroom is fun, loads multiple csvs at once
# The data are the 2019 contracts archive from USAspending.gov
files <- fs::dir_ls(glob = "2018_all_Contracts_Full_20191010_*csv")
contracts18 <- vroom(files, delim = ",", col_select = c(award_id_piid, award_description, 
                                                        product_or_service_code_description, action_date, 
                                                        naics_description, awarding_agency_name, 
                                                        awarding_sub_agency_name, recipient_name, 
                                                        recipient_country_name, recipient_state_name, 
                                                        recipient_city_name, 
                                                        federal_action_obligation))                    


######
# FOR LOCKHEED ANALYSIS LATER
#####

lockSSAcontracts <- contracts18 %>% 
  filter(awarding_agency_name == "SOCIAL SECURITY ADMINISTRATION (SSA)" &
           recipient_name == "LOCKHEED MARTIN CORPORATION")
rm(contracts18)

write_csv(lockSSAcontracts, "lockheedSSAContracts.csv")
#####
#
#####

# Smooshing it donw
contracts18 <- contracts18 %>% 
  group_by(recipient_name, product_or_service_code_description, naics_description, awarding_agency_name, 
           awarding_sub_agency_name) %>% 
  summarise(fundsObligated = sum(federal_action_obligation))


# Saving this shit as an RData file to save me time later

saveRDS(contracts18, "contracts18.RData")

# seeing what we're dealing with
contracts18 <- contracts18 %>% 
  arrange(desc(fundsObligated))

# So, we have several companies receiving awards from multiple agencies, producing multiple entries
# I think the best approach is to get a Top 10, and then use the above dataset to make sense of it

vendors <- contracts18 %>% 
  group_by(recipient_name) %>% 
  summarise(fundsObligated = sum(fundsObligated)) %>% 
  filter(fundsObligated > 0) %>% 
  arrange(desc(fundsObligated))


# Saving these data, too, because that makes sense
saveRDS(vendors, "vendors18.RData")

vendors <- readRDS("vendors18.RData")


vendorsLite <- vendors %>% 
  filter(fundsObligated > 2000000000)
write.csv(vendorsLite, "top32_Vendors.csv", row.names = FALSE)


# loading contract data
contracts18 <- readRDS("contracts18.RData")


vSmall <- vendors %>% 
  filter(fundsObligated > 8400000000)

top5 <- c(vSmall$recipient_name)

# loading just the top five
contracts18$recipient_name <- ifelse(contracts18$recipient_name %in% top5, contracts18$recipient_name, NA)

contracts18 <- contracts18 %>% 
  filter(!is.na(recipient_name))

naics <- contracts18 %>%
  group_by(naics_description) %>% 
  summarise(fundsObligated = sum(fundsObligated)) %>% 
  arrange(desc(fundsObligated))

write.csv(naics, "naics18.csv", row.names = FALSE)

psc <- contracts18 %>%
  group_by(product_or_service_code_description) %>% 
  summarise(fundsObligated = sum(fundsObligated))%>% 
  arrange(desc(fundsObligated))

write.csv(psc, "psc18.csv", row.names = FALSE)

# Looking at just MCKESSON CORPORATION because the rest of the top 5 are arms dealers or aerospace 

mckesson <- contracts18 %>% 
  filter(recipient_name == "MCKESSON CORPORATION")

# Funding agencies
mckesson %>% 
  group_by(awarding_agency_name) %>% 
  summarise(fundsObligated = sum(fundsObligated))

# PSCs
mckesson %>% 
  group_by(product_or_service_code_description) %>% 
  summarise(fundsObligated = sum(fundsObligated))

# NAICs
mckesson %>% 
  group_by(naics_description) %>% 
  summarise(fundsObligated = sum(fundsObligated))


# NAICs to PSCs
test <- mckesson %>% 
  group_by(naics_description, product_or_service_code_description) %>% 
  summarise(fundsObligated = sum(fundsObligated))


# Looking at just LOCKHEED MARTIN CORPORATION because the rest of the top 5 are arms dealers or aerospace 

lockheed <- contracts18 %>% 
  filter(recipient_name == "LOCKHEED MARTIN CORPORATION")

# Funding agencies
agencies <- lockheed %>% 
  group_by(awarding_agency_name) %>% 
  summarise(fundsObligated = sum(fundsObligated)) %>% 
  filter(fundsObligated > 0) %>% 
  arrange(desc(fundsObligated))

# SSA & NAICs/PSCs
ssa <- lockheed %>% 
  filter(awarding_agency_name == "SOCIAL SECURITY ADMINISTRATION (SSA)")

# NAICs
lockheed %>% 
  group_by(naics_description) %>% 
  summarise(fundsObligated = sum(fundsObligated))

# PSCs
lockheed %>% 
  group_by(product_or_service_code_description) %>% 
  summarise(fundsObligated = sum(fundsObligated)) %>% 
  arrange(desc(fundsObligated))


