# Load necessary libraries
library(tidyverse)
library(feather)
library(stats)
library(tree)
library(caret)
library(knitr)

theme_set(theme_bw())

# Statistical Leanring
# Import data
hOurworld_df_old <- read_feather("data/hOurworld_df_old.feather")

# Graph analysis
# Total membership
p4 <- ggplot(hOurworld_df_old, aes(Total.Members, Exchange.Past.Year_per_Capita)) +
  geom_point() +
  labs(title = "Correlation between Time Bank Membership and Activty Level (hOurworld)",
       x = "Total Membership",
       y = "Exchange Past Year per Capita (hr)")
ggsave("graphics/scatter_activity-membership.png", p4, width = 8, height = 6)

# Active membership ratio
p5 <- ggplot(hOurworld_df_old, aes(ActiveRatio, Exchange.Past.Year_per_Capita)) +
  geom_point() +
  labs(title = "Correlation between Active Membership Ratio and Activty Level (hOurworld)",
       x = "Reported Ratio of Acitve Membership Accounts",
       y = "Exchange Past Year per Capita (hr)")
ggsave("graphics/scatter_activity-ratio.png", p5, width = 8, height = 6)

# Number of admin accounts
p6 <- ggplot(hOurworld_df_old, aes(X.Admin.Account, Exchange.Past.Year_per_Capita)) +
  geom_point() +
  labs(title = "Correlation between Number of Admin Accounts and Activty Level (hOurworld)",
       x = "Number of Admin Accounts",
       y = "Exchange Past Year per Capita (hr)")
ggsave("graphics/scatter_activity-admin.png", p6, width = 8, height = 6)

# Decision Tree
hOurworld_tree_data <- hOurworld_df_old %>%
  mutate(Exchange.Past.Year_per_Capita = ifelse(Exchange.Past.Year_per_Capita <= 2, "Inactive TB",
                           ifelse(Exchange.Past.Year_per_Capita > 2, "Active TB", NA)),
         Exchange.Past.Year_per_Capita = as.factor(Exchange.Past.Year_per_Capita),
         Total.Members = as.numeric(Total.Members),
         ActiveRatio = as.numeric(ActiveRatio), # Percentage of active members
         X.Admin.Account = as.numeric(X.Admin.Account) # Number of coordinators
         )
# Saving for future use
write_feather(hOurworld_tree_data, "data/hOurworld_tree_data.feather")

# Construct the tree
hOurworld_tree <- tree(Exchange.Past.Year_per_Capita ~ Total.Members + ActiveRatio +
                           X.Admin.Account, data = hOurworld_tree_data)
summary(hOurworld_tree)

# Plot tree
png(filename = "graphics/hOurworld_tree.png", width = 1000, height = 700, units = "px")
plot(hOurworld_tree)
text(hOurworld_tree, pretty = 0)
dev.off()

# Test the tree using random forest
# Clean
hOurworld_rf_data <- hOurworld_tree_data %>%
  select(Exchange.Past.Year_per_Capita, Total.Members, ActiveRatio, X.Admin.Account) %>%
  na.omit()
hOurworld_rf_data
# Saving for future use
write_feather(hOurworld_rf_data, "data/hOurworld_rf_data.feather")

# Test
hOurworld_rf <- train(Exchange.Past.Year_per_Capita ~ Total.Members + ActiveRatio +
                        X.Admin.Account, data = hOurworld_rf_data,
                    method = "rf",
                    ntree = 200,
                    trControl = trainControl(method = "oob"))
hOurworld_rf

# Illustrate the reasults
kable(hOurworld_rf$finalModel$confusion)

varImpPlot(hOurworld_rf$finalModel)