library(tidyverse)
library(dplyr)
library(rstan)
library(bayesplot)
library(lubridate)
library(loo)
library(ggplot2)
library(reshape2)

#load the data
sp500 <- read.csv("your path to 'new_spy'")
macro_data <-read.csv("your path to 'macro_data'")

#check the date is appropriate
macro_data$DATE <- as.Date(macro_data$DATE, format="%Y-%m-%d")
sp500$DATE <- as.Date(sp500$DATE, format="%d/%m/%Y")

#merge them to one data for convinience
q1data <-macro_data %>%
  left_join(sp500, by ="DATE")

#making the data per month instead of per day

q1data <- q1data %>%
  mutate(YearMonth = floor_date(DATE, "month"))

# Aggregate the data to the monthly level
monthly_data <- q1data %>%
  group_by(YearMonth) %>%
  summarize(
    GDP = first(GDP),
    Interest = first(Interest),
    Unemployment_rate = first(Unemployment_rate),
    inflation_rate = first(inflation_rate),
    Open = first(Open),
    Close = last(Close),
    High = max(High),
    Low = min(Low),
    yield = ((last(Close) - first(Open)) / first(Open))*100  # Monthly yield
  ) %>%
  ungroup()

#####QUESTION 1#####

#setting the model data
model1_data <- monthly_data %>%
  select(yield,GDP,inflation_rate,Unemployment_rate,Interest) %>%
  na.omit()

#plot the observed yield percents
plot(density(model1_data$yield), main = "density of observed yield percents")

# prepare the data list for Stan
stan_data <- list(
  N = nrow(model1_data),
  yield_precents = model1_data$yield,
  GDP = model1_data$GDP,
  inflation_rate = model1_data$inflation_rate,
  unemployment_rate = model1_data$Unemployment_rate,
  interest = model1_data$Interest
)


# Compile the Stan model
model1 <-stan_model("your path to 'model1_stan'")

# run the MCMC sampling

fit <- sampling (model1,
                 data = stan_data,
                 chains = 4,
                 iter = 5000,
                 warmup = 1000,
                 thin = 1,
                 seed = 1234,
                 #for some reason, the next line increasing the ess
                 control = list(adapt_delta = 0.95)
)



# print the summary of the results
print(fit)

#plotting the posterior distribution
mcmc_dens(
  as.matrix(fit, pars = c("b_0", "b_gdp", "b_inflation_rate", "b_unemployment_rate", "b_interest"))
) +
  ggtitle("posterior density plots of beta")


#saving the results
fit_summary <- summary(fit)$summary
#write.csv(fit_summary,"C:/Users/gilad/OneDrive/שולחן העבודה/לימודים/R/עבודה מסכמת מידול והסקה בייסיאנים/נסיון 2/fit_summary.csv", row.names = TRUE)

#a summary table for the betas
beta_summary <- summary(fit, pars = c("b_0", "b_gdp", "b_inflation_rate", "b_unemployment_rate", "b_interest"),
                        probs = c(0.025, 0.5, 0.975))$summary
#write.csv(beta_summary,"C:/Users/gilad/OneDrive/שולחן העבודה/לימודים/R/עבודה מסכמת מידול והסקה בייסיאנים/נסיון 2/beta_summary_final.csv", row.names = TRUE)



# Traceplot for visual diagnostics of convergence
traceplot(fit, pars = c("b_0", "b_gdp", "b_inflation_rate", "b_unemployment_rate", "b_interest"))

#comparing actual data to ppd
plot(density(model1_data$yield), col = "red", 
     main = "Actual vs PPD ", 
     xlab = "S&P 500 returns")
lines(density(extract(fit)$y_rep), col = "blue")

# add a legend
legend("topright", legend = c("Actual", "PPD model1"), 
       col = c("red","blue"),lty = c(1,1))


posterior_samples <- extract(fit)




#####QUESTION 2#####

#setting the model data
model2_data <- monthly_data %>%
  select(YearMonth, yield,GDP,inflation_rate,Unemployment_rate,Interest) %>%
  na.omit()

#we need to calclate the gdp_growth for every 3 months
model2_data <- model2_data %>%
  mutate(
    GDP_growth = ((lead(GDP, 3) - GDP) / GDP) * 100
  )
#because we dont have new values for gdp later:
model2_data <- model2_data[1:146,]

# classify based on GDP growth
model2_data <- model2_data %>%
  mutate(
    economic_phase = ifelse(GDP_growth < 1, "no_progression", "significant_growth")
  )

#set what is high unemployment
high_unemployment <- quantile(model2_data$Unemployment_rate, 0.8) 

# Calculate Inflation Rate change
model2_data <- model2_data %>%
  mutate(
    Inflation_Change = ((lead(inflation_rate, 3) - inflation_rate) / inflation_rate) * 100
  )

#set what is high inflation change
high_inflation_change <- quantile(model2_data$Inflation_Change, 0.8, na.rm = TRUE)  



model2_data <- model2_data %>%
  mutate(
    economic_phase = case_when(
      GDP_growth < 1 ~ "no_progression",  # Negative GDP growth
      Unemployment_rate > high_unemployment | Inflation_Change > high_inflation_change ~ "no_progression",  # High unemployment and inflation
      TRUE ~ "significant_growth"
    )
  )
#compile the stan model
model2 <-stan_model("your path to 'model1_stan'")

# prepare the data list for Stan
stan_data2 <- list(
  N = nrow(model2_data),
  yield_precents = model2_data$yield,
  GDP = model2_data$GDP,
  inflation_rate = model2_data$inflation_rate,
  unemployment_rate = model2_data$Unemployment_rate,
  interest = model2_data$Interest
)

# run the MCMC sampling
fit2 <- sampling(model2,
                data = stan_data2,
                chains = 4,
                iter = 5000,
                warmup = 1000,
                thin = 1,
                seed = 1234,
                control = list(adapt_delta = 0.95)
)

#saving the results
fit2_summary <- summary(fit2)$summary
#write.csv(fit2_summary,"C:/Users/gilad/OneDrive/שולחן העבודה/לימודים/R/עבודה מסכמת מידול והסקה בייסיאנים/נסיון 2/fit_summary_q2.csv", row.names = TRUE)

#a summary table for the betas
beta2_summary <- summary(fit2, pars = c("b_0", "b_gdp", "b_inflation_rate", "b_unemployment_rate", "b_interest"),
                        probs = c(0.025, 0.5, 0.975))$summary
#write.csv(beta_summary,"C:/Users/gilad/OneDrive/שולחן העבודה/לימודים/R/עבודה מסכמת מידול והסקה בייסיאנים/נסיון 2/beta_summary_q2_final.csv", row.names = TRUE)

#all of the results here:
print(fit2)

# Traceplot for visual diagnostics of convergence
traceplot(fit2, pars = c("b_0", "b_gdp", "b_inflation_rate", "b_unemployment_rate", "b_interest"))


#take the indices
no_progression_indices <- which(model2_data$economic_phase == "no_progression")
significant_growth_indices <- which(model2_data$economic_phase == "significant_growth")
#extract the ppd for each phase
ppd_no_progression <- extract(fit)$y_rep[, no_progression_indices]
ppd_significant_growth <- extract(fit)$y_rep[, significant_growth_indices]

#check the distribution - only ppd
plot(density(ppd_no_progression), col = "red", main = "posterior predictive distributions", xlab = "S&P 500 returns")
lines(density(ppd_significant_growth), col = "blue")
legend("topright", legend = c("No Progression", "Significant growth"), col = c("red", "blue"),lwd =2)



#check the distribution - actual vs ppd
actual_no_progression <- model2_data$yield[no_progression_indices]
actual_significant_growth <- model2_data$yield[significant_growth_indices]

# plot the actual vs PPD for "No Progression"
plot(density(actual_no_progression), col = "red", lty = 2, 
     main = "Actual vs PPD ", 
     xlab = "S&P 500 returns")
lines(density(ppd_no_progression), col = "red", lty = 1,lwd =2)

# plot the actual vs PPD for "Significant growth"
lines(density(actual_significant_growth), col = "blue", lty = 2)
lines(density(ppd_significant_growth), col = "blue", lty = 1,lwd=2)

#lwd make the line thicker, lty decide if its continuous line or not 

# add a legend
legend("topright", legend = c("Actual No Progression", "PPD No Progression", 
                              "Actual Significant Growth", "PPD Significant Growth"), 
       col = c("red", "red", "blue", "blue"), lty = c(2, 1, 2, 1),cex = 0.8)
#cex fitted the size of the legend for me


#####QUESTION 3#####

#read the news data
news_sp500 <- read.csv("your path to 'news_data'")

#order the number of positive & negative sentiment, sp500 news and analyst comments
model3_data <- news_sp500 %>%
  group_by(DATE) %>%
  summarise(
    Positive_sentiment = sum(Positive_sentiment, na.rm = TRUE),
    Negative_sentiment = sum(Negative_sentiment, na.rm = TRUE),
    sp500_news = sum(sp500_news, na.rm = TRUE),
    analyst_comments = sum(analyst_comments, na.rm = TRUE)
  )

#make the date in date format and arrange the data by date
model3_data$DATE<-as.Date(model3_data$DATE, format = "%d/%m/%Y")
model3_data <- model3_data %>%
  arrange(DATE)

#add the sp500 value to the data
model3_data <- merge(model3_data, sp500[, c("DATE", "Close")], by = "DATE", all.x = TRUE)

#calculate the daily yield
model3_data <- model3_data %>%
  mutate(daily_yield = ((lead(Close)-Close)/Close) * 100) %>%
  na.omit()
#because the first row has no data
model3_data <-model3_data[2:nrow(model3_data),]


#work on the data for the model
stan_data_model3 <- list(
  N = nrow(model3_data),
  daily_yield = model3_data$daily_yield,
  Positive_sentiment = model3_data$Positive_sentiment,
  Negative_sentiment = model3_data$Negative_sentiment,
  analyst_Comments = model3_data$analyst_comments)

#load the model
model3 <- stan_model("your path to 'model3_stan'")

#run the model
fit_model3 <- sampling(model3,
                data = stan_data_model3,
                chains = 4,
                iter = 5000,
                warmup = 1000,
                thin = 1,
                seed = 1234,
                control = list(adapt_delta = 0.95)
)


#a summary table for the betas
model3_beta_summary <- summary(fit_model3, pars = c("b_0", "b_positive", "b_negative", "b_analyst"),
                               probs = c(0.025, 0.5, 0.975))$summary 
#write.csv(model3_beta_summary,"C:/Users/gilad/OneDrive/שולחן העבודה/לימודים/R/עבודה מסכמת מידול והסקה בייסיאנים/נסיון 2/model3_beta_summary_final.csv", row.names = TRUE)

print(fit_model3)

# Traceplot for visual diagnostics of convergence
traceplot(fit_model3, pars = c("b_0", "b_positive", "b_negative", "b_analyst"))


# plot the actual vs PPD
plot(density(model3_data$daily_yield), col = "red", 
     main = "Actual vs PPD ", 
     xlab = "S&P 500 returns")
lines(density(extract(fit_model3)$y_rep), col = "blue")

# add a legend
legend("topright", legend = c("Actual", "PPD_model3"), 
       col = c("red","blue"),lty = c(1,1))






#saving the log-likelihoods from the models
log_lik1 <- extract_log_lik(fit)
log_lik3 <- extract_log_lik(fit_model3)

#calculating waic
waic1 <- waic(log_lik1)
waic3 <- waic(log_lik3)

#calculating loo
loo1 <- loo(log_lik1)
loo3 <- loo(log_lik3)

#calculating root mean squared error
rmse_model1 <- sqrt(mean((extract(fit)$y_rep-model1_data$yield)^2))
rmse_model3 <- sqrt(mean((extract(fit_model3)$y_rep-model3_data$daily_yield)^2))

#comparison
comparison_df <- data.frame(
  model = c('model 3', 'model 1'),
  waic = c(waic3$estimates['waic','Estimate'],waic1$estimates['waic','Estimate']),
  loo = c(loo3$estimates['looic','Estimate'],loo1$estimates['looic','Estimate']),
  RMSE = c(rmse_model3,rmse_model1)
)

#change the dataframe a little to manage it better
comparison_melted <- melt(comparison_df, id.vars = "model")

#plot the results:
ggplot(comparison_melted, aes(x = model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "models comparison") +
  scale_fill_manual(values = c("blue", "green", "red"), , 
                    labels = c("WAIC", "LOOIC", "RMSE")) +
  theme_minimal()
