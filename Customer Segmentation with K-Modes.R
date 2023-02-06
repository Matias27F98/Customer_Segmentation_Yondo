library(tidyverse)



df <- read.csv("/Users/matiasfonolla/Desktop/Market Research - Training /Market Research/Yondo.xlsx - QUERY_FOR_YONDOFINAL.csv",
               na.strings = "")

# We have 60 variables and 284 observations
dim(df)

# Names of our variables
names(df)

# Every single one of our observations has at least one missing value
df %>%
  count(!complete.cases(.))


# important variables

df <- df %>%
  mutate(Age = as.factor(Age),
         Gender = as.factor(Gender),
         Industry = as.factor(Industry),
         CountryName = as.factor(CountryName),
         SignUpType = as.factor(SignUpType),
         SubscriptionStatus = as.factor(SubscriptionStatus),
         PlanName = as.factor(PlanName),
         AlreadySelling = as.factor(AlreadySelling),
         Revenue = as.factor(Revenue),
         Are.they.active = as.factor(Are.they.active))


# Re-coding overlapping age categories

unique(df$Age)

df$Age <- replace(df$Age, df$Age == "45-55", "45-54")

df$Age <- replace(df$Age, df$Age == "44-45", "45-54")


df$Age <- replace(df$Age, df$Age == "44-55", "45-54")


df$Age <- replace(df$Age, df$Age == "34-45", "35-44")


df$Age <- replace(df$Age, df$Age == "54-65", "55-64")


# Re-coding wrong PlanName and CountryName as NA 

df$PlanName <- replace(df$PlanName, df$PlanName == "/", NA)
df$CountryName <- replace(df$CountryName, df$CountryName == "0", NA)


# exploring relevant variables

df %>%
  dplyr::select(where(is.factor))%>%
  summary.data.frame()


df %>%
  ggplot(aes(Gender, fill = Industry)) +
  geom_bar() +
  ggtitle("Industry distribution across genders")

df %>%
  ggplot(aes(AlreadySelling, fill = AlreadySelling)) +
  geom_bar() +
  facet_grid(df$Industry~df$Are.they.active) + 
  theme(axis.text.x = element_blank()) +
  ggtitle("Account and sales activities across industry")

df %>%
  ggplot(aes(AlreadySelling, fill = AlreadySelling)) +
  geom_bar() +
  facet_grid(df$Industry~df$Revenue) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Revenue and sales activity across industry")

df %>%
  ggplot(aes(AlreadySelling, fill = AlreadySelling)) +
  geom_bar() +
  facet_grid(df$Industry~df$PlanName) + 
  theme(axis.text.x = element_blank()) +
  ggtitle("Sales activity across industry and plan name")


## Cluster analysis

# subsetting variables

condensed_df <- df%>%
  dplyr::select(Age, Gender, CountryName, 
         PlanName, Industry, AlreadySelling, Revenue, Are.they.active)


# K-mode clustering will not work if we feed it NA values. 
## We will work around this by turning them into a string.

library(gtools)

dfwNA <- condensed_df %>%
  mutate_if(is.factor, as.character)

dfwNA <- gtools::na.replace(dfwNA, replace = "NA")


# estimate the ideal number of clusters

library(klaR)

set.seed(1222)
Es <- numeric(10)
for(i in 1:10){
  kpres <- kmodes(dfwNA, modes = i, iter.max = 15, fast = TRUE)
  Es[i] <- kpres$withindiff
}
plot(1:10, Es, type = "b", ylab = "Within Cluster distance", xlab = "# Clusters",
     main = "Scree Plot") # figure 2


# Running k-modes with 4 clusters


mode_clusterswNA <- kmodes(dfwNA, modes = 4, iter.max = 15, fast = TRUE)

clusterswNA <- mode_clusterswNA$modes

clusterswNA <- clusterswNA %>%
  mutate(Size = mode_clusterswNA$size)





