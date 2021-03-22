library(data.table)
library(CHAID)
library(ROCR)
library(caret)
library(kableExtra)
library(purrr) 
library(dplyr)
library(partykit)

# a
path <- paste0(getwd(), "/credit-risk-course/ex_2/Bondora_preprocessed.csv")
data_raw <- data.table(readr::read_csv(path))
col_list <- c("UseOfLoan",
              "LoanDuration", 
              "Age",
              "Gender",
              "County",
              "City",
              "HomeOwnershipType",
              "EmploymentStatus",
              "EmploymentDurationCurrentEmployer",
              "OccupationArea",
              "IncomeTotal",
              "MaritalStatus",
              "Education",
              "ExistingLiabilities",
              "DebtToIncome",
              "RefinanceLiabilities",
              "Default")

data <- data_raw[Country == "ES", ..col_list]

# b

table(data$UseOfLoan)
hist(data$LoanDuration)
hist(data$Age)
table(data$Gender)
table(data$HomeOwnershipType)
table(data$EmploymentStatus)
table(data$EmploymentDurationCurrentEmployer)
table(data$OccupationArea)

hist(data$IncomeTotal)
tail(sort(data$IncomeTotal))
summary(data$IncomeTotal)

table(data$MaritalStatus)
table(data$Education)
hist(data$ExistingLiabilities)
summary(data$ExistingLiabilities)

hist(data$DebtToIncome)
hist(data$RefinanceLiabilities)
table(data$Default)

# c

data[,UseOfLoan := as.factor(UseOfLoan)]
data[,Gender := as.factor(Gender)]
data[,County := as.factor(County)]
data[,City := as.factor(City)]
data[,HomeOwnershipType := as.factor(HomeOwnershipType)]
data[,EmploymentStatus := as.factor(EmploymentStatus)]
data[,EmploymentDurationCurrentEmployer := as.factor(EmploymentDurationCurrentEmployer)]
data[,OccupationArea := as.factor(OccupationArea)]
data[,MaritalStatus := as.factor(MaritalStatus)]
data[,Education := as.factor(Education)]
data[,Default := as.factor(Default)]

data[,LoanDuration := cut(LoanDuration, 5)]
data[,Age := cut(Age, 5)]
data[,IncomeTotal := cut(IncomeTotal, 3)]
data[,ExistingLiabilities := cut(ExistingLiabilities, 5)]
data[,DebtToIncome := cut(DebtToIncome, 5)]
data[,RefinanceLiabilities := cut(RefinanceLiabilities, 5)]


# d 

# Formula all
str(data)
chaid_full <- chaid(Default ~ UseOfLoan +
                       LoanDuration +
                       Age + 
                       Gender +
                       HomeOwnershipType + 
                       EmploymentStatus + 
                       EmploymentDurationCurrentEmployer + 
                       OccupationArea +
                       IncomeTotal + 
                       MaritalStatus +
                       Education + 
                       ExistingLiabilities + 
                       DebtToIncome + 
                       RefinanceLiabilities,
                     data = data)

plot(chaid_full)
summary(chaid_full)

sort(varimp(chaid_full), decreasing = TRUE)
chisq.test(data$Default, data$LoanDuration)
pred <- predict(chaid_1)

# Formula 1
chaid_1 <- chaid(Default ~
                   MaritalStatus +
                   Education + 
                   LoanDuration + 
                   Age,
                 data = data)

# Formula 2
chaid_2 <- chaid(Default ~ LoanDuration +
                       Gender +
                       MaritalStatus +
                       Education, 
                     data = data)


# Formula 3
chaid_3 <- chaid(Default ~ LoanDuration +
                   EmploymentStatus +
                   IncomeTotal +
                   ExistingLiabilities, 
                 data = data)



# Formula 4
chaid_4 <- chaid(Default ~ LoanDuration +
                   IncomeTotal +
                   DebtToIncome +
                   RefinanceLiabilities, 
                 data = data)


# comparison
modellist <- list(Model1 = chaid_1, Model2 = chaid_2, Model3 = chaid_3, Model4 = chaid_4)

CHAIDResults <- map(modellist, ~ predict(.x)) %>% 
  map(~ confusionMatrix(data$Default, .x)) %>%
  map_dfr(~ cbind(as.data.frame(t(.x$overall)),as.data.frame(t(.x$byClass))), .id = "ModelNumb")

kable(CHAIDResults, "html") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                font_size = 12)


confusionMatrix(predict(chaid_2), data$Default)

# selected model: chaid_2

predtree <- predict(chaid_1, data, type = "prob")
pred <- prediction(predtree[,2], data$Default)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)
auc <- performance(pred, "auc")
auc@y.values
