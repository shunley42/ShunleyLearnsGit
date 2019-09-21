# HI THERE


rm(list = ls())
options(scipen = 9999, digits = 6)


library(tidyverse)
library(data.table)
library(scales)
library(gganimate)
library(gifski)
library(extrafont)




# Create Working Directory Short Cuts
local_dir <- "/Users/samhunley/Desktop/Analyses and Fun/Mindfulness"
setwd(local_dir)

# loading data
mindfulness <- readRDS("mindfulnessGrants.RData")


# descriptive fun
mindfulness$federal_action_obligation <- as.numeric(mindfulness$federal_action_obligation)

#FY sum
mindfulness %>% 
  group_by(fiscal_year) %>%
  summarise(TotalObligations = sum(federal_action_obligation))

#State sum
states <- mindfulness %>% 
  group_by(recipient_state_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>% 
  arrange(desc(TotalObligations))

write.csv(states, "mindfulstates.csv", row.names = FALSE)

#State sum FY18
states18 <- mindfulness %>%
  filter(fiscal_year == "2018") %>% 
  group_by(recipient_state_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>% 
  filter(TotalObligations > 0) %>% 
  arrange(desc(TotalObligations))

#City sum
mindfulness %>% 
  group_by(recipient_city_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>% 
  arrange(desc(TotalObligations))

#Agency Sum
mindfulness %>% 
  group_by(awarding_agency_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>%
  arrange(desc(TotalObligations))

#Subagency Sum
mindfulness %>% 
  group_by(awarding_sub_agency_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>%
  arrange(desc(TotalObligations))

# # Grouping agencies with less than $5 million in obligations
# lessThan5 <- c("DEPARTMENT OF STATE (DOS)", "CORPORATION FOR NATIONAL AND COMMUNITY SERVICE (CNCS)",
#                "NATIONAL SCIENCE FOUNDATION (NSF)", "DEPARTMENT OF AGRICULTURE (USDA)")
# 
# mindfulness$awarding_agency_name <- ifelse(mindfulness$awarding_agency_name %in% lessThan5, "Smaller Agency Obligations",
#                                        mindfulness$awarding_agency_name)

# Graphing stuff
mindfulness2 <- mindfulness %>% 
  group_by(awarding_agency_name, fiscal_year) %>% 
  summarise(TotalObligations = sum(federal_action_obligation))

#####
# One problem with this dataset is that not all agencies obligate funds each year
# And some obligations are in the negative
# This leads to an area graph with holes in it
# This overly convoluted step addresses that issue by adding 0s for each of the above cases
#####

#creating a dataset that has all FYs
agencies <- mindfulness2 %>%  
  select(awarding_agency_name) %>% 
  distinct()
agencies$fiscal_year <- "2018"
years <-  c("2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009")
agenciesA <- tibble()
for (i in years) {
  agenciesA <- distinct(select(agencies, -fiscal_year))
  agenciesA$fiscal_year <- i
  agencies <- bind_rows(agencies, agenciesA)
}

mindfulness3 <- left_join(agencies, mindfulness2, by = c("awarding_agency_name", "fiscal_year") )

mindfulness3$TotalObligations[is.na(mindfulness3$TotalObligations)] <- 0
mindfulness3$TotalObligations[mindfulness3$TotalObligations < 0] <- 0

# And there you have an overly complicated way to deal with the fact not all agencies have funds for all years
# Also for the fact that there are some negative values

#####
# Testing how all of the above turned out
#####
#FY sum
mindfulness3 %>% 
  group_by(fiscal_year) %>%
  summarise(TotalObligations = sum(TotalObligations))

#Agency Sum
mindfulness3 %>% 
  group_by(awarding_agency_name) %>%
  summarise(TotalObligations = sum(TotalObligations)) %>%
  arrange(desc(TotalObligations))

#Agency Sum by FY
check <- mindfulness3 %>% 
  group_by(awarding_agency_name, fiscal_year) %>%
  summarise(TotalObligations = sum(TotalObligations)) %>%
  arrange(desc(fiscal_year))

#####
# Cleaning the data to make it prettier
#####
# This line changes all awarding agency names to sentence capitalization and removes acronyms)
mindfulness3$awarding_agency_name <- str_to_title(gsub("\\s*\\([^\\)]+\\)","", 
                                                       as.character(mindfulness3$awarding_agency_name)),
                                                  locale = "en")

mindfulness3$fiscal_year <- as.character(mindfulness3$fiscal_year)
mindfulness3$TotalObligationsMillions <- mindfulness3$TotalObligations/1000000

# Changing order of factors to be by TotalObligationsMillions
mindfulness3$awarding_agency_name <- as.factor(mindfulness3$awarding_agency_name)
mindfulness3$awarding_agency_name <- reorder(mindfulness3$awarding_agency_name, mindfulness3$TotalObligationsMillions)
levels(mindfulness3$awarding_agency_name)

mindfulness3$awarding_agency_name_wrapped <- str_wrap(mindfulness3$awarding_agency_name, width = 15)

# Loading System Fonts for Plot
loadfonts()

scale_fill_brewer(palette = "Set3") 
#scale_fill_manual(values = c("#17301C", "#379392","#4DB0C6", "#4F86C6", "#744FC6", "#0B3954"))
# Generating Plot
p <-  ggplot(mindfulness3, aes(x = fiscal_year, y = TotalObligationsMillions, group = awarding_agency_name)) + 
  geom_area(aes(fill = awarding_agency_name), position = 'stack') +
  scale_fill_manual(values = c("#CBB3BF","#E6C79C", "#CDDFA0","#78586F", "#6FD08C", "#7B9EA8")) +
  labs(title = "Cumulative Federal Grant Funding Related to Mindfulnesss",
       subtitle = "Grants Including 'Mindfulness' in the Award Description, FY2009 - FY2018",
       caption = "Created by @Shunley42; Source: USAspending.gov",
       fill = "Awarding Agency",
       x = "Fiscal Year",
       y = "Total Obligations") +
  scale_y_continuous(labels=dollar_format(prefix = "$", suffix = " million"), limits = c(0, 30),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  guides(fill = guide_legend(keywidth = 3, keyheight = 3)) +
  theme(text = element_text(family = "Helvetica", 
                            size = 14,
                            color = "#000000"),
        axis.title.y = element_text(size = 15, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0),
                                    family = "Helvetica-Bold",
                                    color = "#000000"),
        axis.title.x = element_text(size = 15,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    family = "Helvetica-Bold",
                                    color = "#000000"),
        axis.text.y = element_text(size = 12,
                                   margin = margin(t = 0, r = 10, b = 0, l = 0),
                                   color = "#000000"),
        axis.text.x = element_text(size = 12,
                                   margin = margin(t = 10, r = 0, b = 0, l = 0),
                                   color = "#000000"),
        axis.line = element_line(colour = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 15, vjust = 0,
                                  family = "Helvetica-Bold",
                                  color = "#000000"),
        plot.subtitle = element_text(size = 13, vjust = 0.5,
                                     color = "#000000"),
        legend.title = element_text(family = "Helvetica-Bold",
                                    color = "#000000"),
        legend.text = element_text(size = 10,
                                   color = "#000000"))


p # Test

ggsave("Grant Funding for Mindfulness.png", plot = p, width = 12, height = 6, dpi = 300)

# Used to Animate
t <- p + 
  transition_reveal(as.numeric(fiscal_year)) 

gif <- animate(t, end_pause = 30, width = 800, height = 400, fps = 8)

anim_save("Grant Funding for Mindfulness.gif", gif)

# Saving wrangled data
saveRDS(mindfulness3, "organizedMindfulnessData.RData")






