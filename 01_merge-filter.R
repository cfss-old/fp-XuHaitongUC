# Load necessary libraries
library(tidyverse)
library(forcats)
library(feather)

# Read datasets
hOurworld_overview <- read.csv("data/hOurworld Overview.csv")
hOurworld_membership <- read.csv("data/hOurworld Membership.csv")

# Clean the datasets
# Overview dataset
hOurworld_overview <- hOurworld_overview %>%
  select(EID, Year, Name, Address, City, State, Zip.Code, Total.Members, Total.Exchange, Exchange.Past.Year, X.Admin.Account) %>%
  filter(!(EID %in% c(777))) %>% # EID 777 is basically hOurworld itself, where its members are time banks
  mutate(Exchange.Past.Year_per_Capita = (Exchange.Past.Year / Total.Members)) %>%
  droplevels()

# Overview for older time banks (Launched before 2016 and at least have 10 hrs of exchange in history)
hOurworld_old_overview <- hOurworld_overview %>%
  filter(!(Year %in% c(2016))) %>%
  filter(!(Total.Exchange <= 10))

# Membership dataset
hOurworld_membership <- hOurworld_membership %>%
  filter(memType..0.Individual.1.Organization.2.Business. %in% c(1)) %>%
  filter(!(memID %in% c(777))) %>% # EID 777 is basically hOurworld itself, where its members are time banks
  transform()

# Creating new dataset for the percentage of active members
hOurworld_membership_grp <- hOurworld_membership %>%
  group_by(EID) %>%
  summarize(ActiveRatio = (1 - mean(Inactive)))

# Merge datasets based on EID
hOurworld_df_all <- hOurworld_overview %>%
  left_join(hOurworld_membership_grp, by="EID")

hOurworld_df_old <- hOurworld_old_overview %>%
  left_join(hOurworld_membership_grp, by="EID")

# write data to file
write_feather(hOurworld_df_all, "data/hOurworld_df_all.feather")
write_feather(hOurworld_df_old, "data/hOurworld_df_old.feather")

# Let's see the general pattern of these time banks
# General Information
summary(hOurworld_overview[c("Total.Members","Total.Exchange","Exchange.Past.Year",
                             "Exchange.Past.Year_per_Capita","X.Admin.Account")])

# Geography
# All initiatives
hOurworld_overview %>%
  group_by(State) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
p1 <- ggplot(hOurworld_overview, aes(State)) +
  geom_bar() +
  labs(y= "Number of Timebanks", title = "Time Banks by State (hOurworld)")
ggsave("graphics/barchart_timebank-by-state-all.png", p1, width = 12, height = 6)

# Those that are actually somewhat established
hOurworld_old_overview %>%
  group_by(State) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
p2 <- ggplot(hOurworld_old_overview, aes(State)) +
  geom_bar() +
  labs(y= "Number of Timebanks",title = "Established Time Banks by State (hOurworld)")
ggsave("graphics/barchart_timebank-by-state-established.png", p2, width = 12, height = 6)

# Years which they are launched
p3 <- ggplot(hOurworld_overview, aes(Year)) +
  geom_bar() +
  labs(y= "Number of Timebanks",title = "Time Banks by the Year They are Founded (hOurworld)") +
  scale_x_continuous(breaks=seq(1995, 2016, 1))
ggsave("graphics/barchart_timebank-by-year.png", p3, width = 12, height = 6)