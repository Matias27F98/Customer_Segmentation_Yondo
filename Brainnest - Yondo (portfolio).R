library(tidyverse)
library(skimr)



df <- read.csv("/Users/matiasfonolla/Desktop/Market Research - Training /Market Research/Yondo.xlsx - QUERY_FOR_YONDOFINAL.csv",
               na.strings = "")





# CLEANING DATA

# we have a lot of variables and missing data
skim(df)



# turining most important variables into factors

glimpse(df)

df <- df %>%
  mutate(Age = as.factor(Age),
         Gender = as.factor(Gender),
         Industry = as.factor(Industry),
         CountryName = as.factor(CountryName),
         SignUpType = as.factor(SignUpType),
         SubscriptionStatus = as.factor(SubscriptionStatus),
         PlanName = as.factor(PlanName),
         AlreadySelling = as.factor(AlreadySelling),
         Revenue = as.factor(Revenue))

# summary
df %>%
  select(where(is.factor))%>%
  summary.data.frame()
  

## Recoding overlaping age categories

unique(df$Age)

df$Age <- replace(df$Age, df$Age == "45-55", "45-54")

df$Age <- replace(df$Age, df$Age == "44-45", "45-54")


df$Age <- replace(df$Age, df$Age == "44-55", "45-54")


df$Age <- replace(df$Age, df$Age == "34-45", "35-44")


df$Age <- replace(df$Age, df$Age == "54-65", "55-64")



# recoding wrong PlanName as NA

df$PlanName <- replace(df$PlanName, df$PlanName == "/", NA)


# summary
df %>%
  select(where(is.factor))%>%
  summary.data.frame()




# EDA


# We have a lot of that are incomplete and don't offer clear insights on potential customer segment

# exploring most relevant descriptors of costumers
## graphing factor variables as barcharts with a loop

df$Industry <- as.factor(df$Industry)
df$Gender <- as.factor(df$Gender)



factors <- sapply(df, is.factor)
factor_vars <- names(factors[factors == TRUE])

plots_list <- list()
for (i in 1:length(factor_vars)) {
  plots_list[[i]] <- ggplot(df, aes_string(factor_vars[i], fill = factor_vars[i])) +
    geom_bar() +
    theme(axis.text.x = element_blank())
}

print(plots_list)



# we can 

df %>%
  ggplot(aes(AlreadySelling, fill = AlreadySelling)) +
  geom_bar() +
  facet_wrap(df$Gender)+
  theme(axis.text.x = element_blank())


## this one below might be better than the last one w facet wrap

df %>%
  ggplot(aes(Gender, fill = Industry)) +
  geom_bar() 



# these two are super important
df %>%
  ggplot(aes(AlreadySelling, fill = AlreadySelling)) +
  geom_bar() +
  facet_grid(df$Revenue~df$Industry) +
  theme(axis.text.x = element_blank())




df %>%
  ggplot(aes(AlreadySelling, fill = AlreadySelling)) +
  geom_bar() +
  facet_grid(df$Industry~df$PlanName) + 
  theme(axis.text.x = element_blank())



# this is useful too



condensed_df <- df%>%
  select(Age, Gender, CountryName, 
         PlanName, Industry, AlreadySelling, Revenue)


# Replacing NA rows with mode 
# Creating a User defined function for finding Mode as R does not come with a base mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Replacing categorical variable NAs with their mode and continuous variable NAs with Mean
## mode is actually NA, so we need to create a list without it to recall the real mode
AgeMode <- as.factor(condensed_df$Age) %>%
  na.omit()

condensed_df %>%
  filter(!complete.cases(.))



condensed_df$Age <- replace_na(condensed_df$Age, getmode(AgeMode))
  
condensed_df$Gender <- replace_na(condensed_df$Gender, getmode(condensed_df$Gender))

condensed_df$Industry <- replace_na(condensed_df$Industry, getmode(condensed_df$Industry))

condensed_df$AlreadySelling <- replace_na(condensed_df$AlreadySelling, getmode(condensed_df$AlreadySelling))


condensed_df$Revenue <- replace_na(condensed_df$Revenue, getmode(condensed_df$Revenue))


glimpse(condensed_df)

condensed_df <- condensed_df %>%
  mutate_if(is.character, as.factor)


# One Hot encoding categorical variables

library(fastDummies)

cluster_data<-dummy_cols(condensed_df)


## clustering



library(ClusterR)
library(cluster)

#set seed
set.seed(123)




## determining ideal number of clusters


library(NbClust)
library(factoextra)



# why are you doing [,8:50]?

fviz_nbclust(cluster_data[,8:50], kmeans, method = "silhouette")

fviz_nbclust(cluster_data[,8:50], kmeans, method = "wss")




stat_gap <- clusGap(cluster_data[,8:50], FUN = kmeans, nstart = 30,K.max = 10, B = 100)
fviz_gap_stat(stat_gap)



## 10 clusters feels excessive. 

## 8 seems to have an almost equal fit. to keep things more parsimoniuous we'll keep it at 8

### maybe let's stick with 3??




kmeans_1 <- kmeans(cluster_data[,8:50], centers = 8, nstart = 20)

## kmeans_1$size
### returns: 105  69 110


# adding segment to dataframe
condensed_df$Segment <- kmeans_1$cluster


## summarizing segments


segment_summary <- condensed_df %>%
  group_by(Segment)%>%
  summarise(n = n(),
            Age = getmode(Age),
            Gender = getmode(Gender),
            CountryName = getmode(CountryName),
            PlanName = getmode(PlanName),
            Industry = getmode(Industry),
            AlreadySelling = getmode(AlreadySelling),
            Revenue = getmode(Revenue))






## with NA stuff 

dfwNA <- df %>%
  select(Age, Gender, CountryName, 
         PlanName, Industry, AlreadySelling, Revenue)


## k means clustering doesn't work with NA values
library(DMwR2)



# Instead of specifying "NA" as a valid factor level for every variable, we'll just make everything into character variables
## it won't matter since we are one hot encoding everything anyway for k clustering

dfwNA <- dfwNA%>%
  mutate_if(is.factor, as.character)

dfwNA <- na.replace(dfwNA, replace = "NA")



cluster_NAdata<-dummy_cols(dfwNA)


kmeans_NA <- kmeans(cluster_NAdata[,8:50], centers = 8, nstart = 20)

dfwNA$Segment <- kmeans_1$cluster


NAsegment_summary <- dfwNA %>%
  group_by(Segment)%>%
  summarise(n = n(),
            Age = getmode(Age),
            Gender = getmode(Gender),
            CountryName = getmode(CountryName),
            PlanName = getmode(PlanName),
            Industry = getmode(Industry),
            AlreadySelling = getmode(AlreadySelling),
            Revenue = getmode(Revenue))


































