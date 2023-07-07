
library(maps)

library(tidyverse)

library(rvest)  # for html

library(uniformly) # for sampling uniformly from the sphere 

library(lubridate)  # for parsing time 

library(e1071) # skewness and kurtosis

library(maditr)

library(data.table)

library(caTools)

options(scipen=999)
library(gtools) #used for permutations in Q1
library(ggplot2) # used for plots in Q2
library(dplyr) #used for piped functions %>%
library(reshape2)# used to transpose matrix in Q2
library(RColorBrewer) # to get more distinct colors, for graph in Q2


# Q1a
sample <- 10000
n <- 100
confused_secretary <- function(n){
  envelopes<-rep(0,times=n)
  correct_assignment<-rep(0,times=n)
  positions <- sample(1:n)  # Randomly select a position
  for(i in 1:n){
    envelopes[i] <- positions[i]
    correct_assignment[i] <- i == positions[i]
  }
  false_count <- sum(!correct_assignment)
  if (false_count == n) {
    return(1) # returns 1 if all the assigments are false
  }
  return(0) # return 0 else
}
outcome <- list()
for (i in 1:sample) { # sample 10,000 times
  result <- confused_secretary(n)
  outcome[[i]] <- result
}
print(round(mean(unlist(outcome)),3))




#Q1b

find_all_cycles <- function(perm) {
  n <- length(perm)
  visited <- rep(FALSE, n)
  result <- rep(0, n)
  for (i in 1:n) {
    if (visited[i]){
      next
    }
    cycle_length <- 1
    start <- i
    cur <- perm[i]
    visited[i] <- TRUE
    while (cur != start) {
      cycle_length <- cycle_length + 1
      visited[cur] <- TRUE
      cur <- perm[cur]
    }
    result[cycle_length] <- result[cycle_length] + 1
  } 
  return(result)
}


n <- 100
simulations <- 10000

expectedXk <- vector(length=n)

for (i in 1:simulations) {
  perm <- sample(n)
  Xk <- find_all_cycles(perm)
  expectedXk <- expectedXk + Xk
}

expectedXk <- expectedXk / simulations

n <- 100
k <- 1:n

plot(k, expectedXk, type = "o", xlab = "Cycle Length (k)", ylab = "Estimated E^[Xk]", main = "Estimates of E^[Xk]")

# Perform log transformation
log_expectedXk <- log(expectedXk)
log_k <- log(k)

data <- data.frame(log_k = log(k), log_expectedXk = log(expectedXk))
model <- lm(log_expectedXk ~ log_k)

data$predicted <- predict(model)

# Create the plot using ggplot2
plot <- ggplot(data, aes(x = log_k, y = log_expectedXk)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "red") +
  labs(x = "log(Cycle Length log(k))", y = "log(Estimated log(E^[Xk]))", title = "Log-Log Plot of Estimates for n = 100")

print(plot)

#Q1c
p12_together <- function(perm) {
  n <- length(perm)
  visited <- rep(FALSE, n)
  
  for (i in 1:n) {
    if (visited[i]) {
      next
    }
    
    cycle <- c(i)
    start <- i
    cur <- perm[i]
    visited[i] <- TRUE
    
    while (cur != start) {
      cycle <- append(cycle, cur)
      visited[cur] <- TRUE
      cur <- perm[cur]
    }
    
    if (1 %in% cycle && 2 %in% cycle) {
      return(1)
    }
  }
  
  return(0)
}
n <- 100
simulations <- 1000
p12_values <- numeric(simulations)
count<-0

for(i in 1:simulations){
  result <- p12_together(sample(1:n))
  p12_values[i] <- result
}



p12_hat <- round(mean(p12_values),3)
sigma_p12<-round(sqrt(p12_hat*(1-p12_hat)/simulations),3)
p12_hat
sigma_p12


#Q1d

are_consecutive <- function(numbers) {
  return(all(diff(sort(numbers)) == 1))
}

count_alice<-0
count_bob<-0
simulations<-100000
for(i in 1:simulations){
  ndeck<-1:13
  alice_hand <- sample(ndeck,5)
  bob_hand<-sample(ndeck[-alice_hand],5)
  if(are_consecutive(alice_hand)){
    count_alice = count_alice + 1
  }
  if(are_consecutive(bob_hand)){
    count_bob = count_bob + 1
  }
}
P.a <- round(count_alice/simulations,3)
P.b <- round(count_bob/simulations,3)

nhands <- 5
ndeck <- 1:13

alice_count <- 0
bob_count <- 0
rejected_samples <- 0
simulations <- 100000

for (i in 1:simulations) {
  alice_hand <- sample(ndeck, nhands, replace = FALSE)
  if (all(diff(sort(alice_hand)) == 1)) {
    alice_count <- alice_count + 1
    bob_hand <- sample(setdiff(ndeck, alice_hand), nhands, replace = FALSE)
    if (all(diff(sort(bob_hand)) == 1)) {
      bob_count <- bob_count + 1
    }
  } else {
    rejected_samples <- rejected_samples + 1
  }
}
P.ba <- bob_count / alice_count

fraction_rejected <- rejected_samples / simulations
fraction_rejected


sd_A <- (sqrt(P.a*(1 - P.a)))/(sqrt(simulations))*sqrt(simulations)
sd_B.a <- (sqrt(P.ba*(1 - P.ba)))/(sqrt(simulations))*sqrt(simulations)

sd_A
sd_B.a


#Q2a


path <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
df <- read.csv(path)
colnames(df)[1] <- "Date"
df[,1] <- as.Date(df[,1])

head(df)
tail(df)


#Q2b

df$Country[df$Country == "Türkiye"] <- "Turkey"
df$Country[df$Country=="Iran (Islamic Republic of)"] <- "Iran"
df$Country[df$Country=="occupied Palestinian territory, including east Jerusalem"] <- "Palestine"
df$Country[df$Country=="Syrian Arab Republic"] <- "Syria"

neighboring_EMRO <- unique(df$Country[which(df$WHO_region=="EMRO")])
neighboring_EMRO<- append(neighboring_EMRO,c("Israel","Cyprus"))
neighboring_EMRO <- neighboring_EMRO[-c(1,2,3,8,10,11,14,17,18,20,21,22)]
cumcases_bycountry_graph<-ggplot(df[df$Country %in% neighboring_EMRO,], aes(x = Date, y = Cumulative_cases, color = Country)) + geom_line() +
  labs(x = "Date", y = "Cumulative Number of Cases", title = "Cumulative COVID-19 Cases by Country") +
  theme_minimal() + theme(legend.position = "left",legend.key.width = unit(0.5, 'cm')) +
  scale_y_continuous(labels = scales::comma)
cumcases_bycountry_graph


cumdeaths_bycountry_graph<-ggplot(df[df$Country %in% neighboring_EMRO,], aes(x = Date, y = Cumulative_deaths, color = Country)) + geom_line() +
  labs(x = "Date", y = "Cumulative Number of Deaths", title = "Cumulative COVID-19 Deaths by Country") +
  theme_minimal() + theme(legend.position = "left",legend.key.width = unit(0.5, 'cm')) +
  scale_y_continuous(labels = scales::comma)

cumdeaths_bycountry_graph

#Q2c



economic_df<-read.csv("economic_data.csv",stringsAsFactors = FALSE)
economic_df$X2018..YR2018.<-as.numeric(economic_df$X2018..YR2018.)
names(economic_df)[1] <- "Country"
clean_economic <- na.omit(economic_df)


economic_df <- dcast(clean_economic, `Country` ~  `Series.Name`, value.var = "X2018..YR2018.", fun.aggregate = sum)

economic_df$Country[economic_df$Country ==  "Egypt, Arab Rep."] <- "Egypt"
economic_df$Country[economic_df$Country ==  "Syrian Arab Republic"] <- "Syria"
economic_df$Country[economic_df$Country ==  "Iran, Islamic Rep."] <- "Iran"
merged_df<-left_join(df,economic_df,by="Country")

merged_df["Cumulative_Cases_per1m"] <- (merged_df$Cumulative_cases*1000000)/merged_df$`Population, total`
merged_df["Cumulative_Deaths_per1m"] <- (merged_df$Cumulative_deaths*1000000)/merged_df$`Population, total`
merged_df["New_Cases_per1m"] <- (merged_df$New_cases*1000000)/merged_df$`Population, total`

merged_df["New_Deaths_per1m"] <- (merged_df$New_deaths*1000000)/merged_df$`Population, total`


color_palette <- brewer.pal(12, "Set3")
# Graph 1: Log-Cumulative COVID-19 Cases by Country
logcumcases_bycountry_graph <- ggplot(merged_df[merged_df$Country %in% neighboring_EMRO,], aes(x = Date, y = log(Cumulative_Cases_per1m), color = Country)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Number of Cases", title = "Log-Cumulative COVID-19 Cases by Country per 1m") +
  theme_minimal() +
  theme(legend.position = "left", legend.key.width = unit(0.5, "cm")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = color_palette)

# Graph 2: Log-Cumulative COVID-19 Deaths by Country per 1m
logcumdeaths_bycountry_graph <- ggplot(merged_df[merged_df$Country %in% neighboring_EMRO,], aes(x = Date, y = log(Cumulative_Cases_per1m), color = Country)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Number of Deaths", title = "Log-Cumulative COVID-19 Deaths by Country per 1m") +
  theme_minimal() +
  theme(legend.position = "left", legend.key.width = unit(0.5, "cm")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = color_palette)

# Display the graphs
logcumcases_bycountry_graph
logcumdeaths_bycountry_graph

max(merged_df$Cumulative_Cases_per1m[which(merged_df$Country=="Israel")])/
  max(merged_df$Cumulative_Cases_per1m[which(merged_df$Country=="Iran")])

specific_country_deaths <- merged_df %>%
  filter(Country %in% neighboring_EMRO) %>%
  group_by(Country) %>%
  summarize(Max_Cumulative_Deaths = max(Cumulative_deaths)) %>%
  arrange(desc(Max_Cumulative_Deaths)) %>%
  distinct()

specific_country_deaths


#Q2d


latest_cases <- aggregate(log(Cumulative_Cases_per1m) ~ Country, data = merged_df, FUN = function(x) tail(x, 1))
latest_deaths <- aggregate(log(Cumulative_Deaths_per1m) ~ Country, data = merged_df, FUN = function(x) tail(x, 1))

latest_data <- merge(latest_cases, latest_deaths, by = "Country")
latest_data<- latest_data[-c(30,54,174),] 


colnames(latest_data) <- c("Country", "Cumulative_Cases_per1m", "Cumulative_Deaths_per1m")

# Create the scatter plot on a log scale
logdeaths_vs_logcases <- ggplot(latest_data, aes(x = log(Cumulative_Cases_per1m), y = log(Cumulative_Deaths_per1m))) +
  geom_point() +
  labs(x = "Log Cumulative Cases per million", y = "Log Cumulative Deaths per million", title = "COVID-19 Cases vs Deaths per million") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() 


model <- lm(log(Cumulative_Deaths_per1m) ~ log(Cumulative_Cases_per1m), data = latest_data)

logdeaths_vs_logcases + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "blue")



fatality_rate <- latest_data$Cumulative_Deaths / latest_data$Cumulative_Cases

# Create a data frame with fatality rate and country
fatality_data <- data.frame(Country = latest_data$Country, Fatality_Rate = fatality_rate)

# Plotting the histogram
ggplot(fatality_data, aes(x = Fatality_Rate)) +
  geom_histogram(binwidth = 0.02, fill = "steelblue", color = "white") +
  labs(x = "Fatality Rate", y = "Count", title = "Distribution of Fatality Rate") +
  theme_minimal()
# Plotting the violin graph
ggplot(fatality_data, aes(x = "", y = Fatality_Rate)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  labs(x = "", y = "Fatality Rate", title = "Distribution of Fatality Rate across Countries") +
  theme_minimal()

mean_fatality <- round(mean(fatality_data$Fatality_Rate),3)
median_fatality <- round(median(fatality_data$Fatality_Rate),3)



outliers <- fatality_data[fatality_data$Fatality_Rate > (mean_fatality + 2 * sd(fatality_data$Fatality_Rate)), "Country"]
outliers

above_mean <- sum(fatality_data$Fatality_Rate > mean_fatality)
proportion_above_mean <- round(above_mean / nrow(fatality_data),3)
skew<-round(skewness(fatality_data$Fatality_Rate),3)


#Q2e


most_suffered <- merged_df %>%
  group_by(Country) %>%
  filter(max(Cumulative_deaths) > 200000) %>%
  ungroup()

ggplot(most_suffered, aes(x = Date, y = New_cases, color = Country)) +
  geom_smooth() +
  labs(title = "Smoothed New Daily Cases for Countries with >200,000 Cumulative Deaths",
       x = "Date",
       y = "New Cases") +
  theme_minimal() + scale_y_continuous(labels = scales::comma_format())

regions <- c(unique(merged_df$WHO_region))

# Plot for new cases by WHO region
ggplot(merged_df, aes(x = Date, y = New_cases, color = WHO_region)) +
  geom_line() +
  labs(x = "Date", y = "New Cases", title = "New COVID-19 Cases by WHO Region") +
  scale_color_discrete(name = "WHO Region") +
  theme_minimal()


#Q2f

# Plot for new deaths by WHO region
ggplot(merged_df[merged_df$New_deaths >= 0, ], aes(x = Date, y = New_deaths, color = WHO_region)) +
  geom_line() +
  labs(x = "Date", y = "New Deaths", title = "New COVID-19 Deaths by WHO Region") +
  scale_color_discrete(name = "WHO Region") +
  theme_minimal()


regions <- c(unique(merged_df$WHO_region))

# Plot for new cases by WHO region
ggplot(merged_df, aes(x = Date, y = New_cases, color = WHO_region)) +
  geom_line() +
  labs(x = "Date", y = "New Cases", title = "New COVID-19 Cases by WHO Region") +
  scale_color_discrete(name = "WHO Region") +
  theme_minimal()


# Plot for new deaths by WHO region
ggplot(merged_df[merged_df$New_deaths >= 0, ], aes(x = Date, y = New_deaths, color = WHO_region)) +
  geom_line() +
  labs(x = "Date", y = "New Deaths", title = "New COVID-19 Deaths by WHO Region") +
  scale_color_discrete(name = "WHO Region") +
  theme_minimal()


names(merged_df)[13]<-"pop65"

merged_df$GDP_per_capita <- merged_df$`GDP, PPP (current international $)` / merged_df$pop65


ggplot(merged_df, aes(x = GDP_per_capita, color = WHO_region)) +
  stat_ecdf(geom = "step") +
  labs(x = "GDP per Capita", y = "Empirical CDF", title = "Empirical CDF of GDP per Capita by WHO Region") +
  scale_color_discrete(name = "WHO Region") +
  theme_minimal()



# Plot percentage of population above 65 (pop65) by WHO region
ggplot(merged_df, aes(x = pop65, color = WHO_region)) +
  stat_ecdf(geom = "step") +
  labs(x = "Percentage of Population Above 65", y = "Empirical CDF", title = "Empirical CDF of Pop65 by WHO Region") +
  scale_color_discrete(name = "WHO Region") +
  theme_minimal()

