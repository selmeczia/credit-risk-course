library(data.table)

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

data
