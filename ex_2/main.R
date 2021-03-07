# Excercise 4 - Monitoring

#install.packages("RColorBrewer")
library("RColorBrewer")
library("data.table")
library(ggplot2)
library(ggthemes)

# 1
path <- paste0(getwd(), "/credit-risk-course/ex_2/Bondora_raw.csv")
ddata <- readr::read_csv(path)

# 2
dL <- ftable(lubridate::year(ddata$LoanDate),lubridate::year(ddata$DefaultDate))


###########

# 1
L <-as.matrix(t(apply(dL,1,cumsum)))

# 2
matplot(t(L), type = "l",
        xlab = "Hitelek felvételének éve",
        ylab = "Hitelek száma",
        xaxt = "n")
axis(side = 1, at = 1:12, labels = 2009:2020)

# 3
L_i <- table(lubridate::year(ddata$LoanDate))

# 4
L_ij <- matrix(cbind(rep(L_i, 12)), nrow = 12, ncol = 12)
N_ij = round(dL / L_ij, 4)

# 5
matplot(t(N_ij), type = "h", col = rainbow(12), lwd = 10, lty = 1,
        xlab = "Évek",
        ylab = "Hitelek nemfizetésének valószínűsége",
        xaxt = "n")
axis(side = 1, at = 1:12, labels = 2009:2020)
legend(title = "Hitel felvételének éve",
       "top", legend = 2009:2020,
       col = rainbow(12),
       pch = 16, horiz = T,
       inset=c(0, -.15), xpd = T, cex = .8)


# 6

N_dt <- data.table(N_ij)
colnames(N_dt) <- c("Start_year", "Inspection_year", "Default_prob")
N_dt[,Start_year := as.integer(as.character(N_dt$Start_year))]
N_dt[,Inspection_year := as.integer(as.character(N_dt$Inspection_year))]
N_dt[,Years_passed := Inspection_year - Start_year]
N_dt[N_dt[,Years_passed < 0]]$Years_passed <- NA
N_dt[, Cum_prob := cumsum(Default_prob), by = Start_year]

for (year in 2009:2020){
 
  change_vector <- c(NA,
                     N_dt[Start_year == year]$Cum_prob[2:12] /
    N_dt[Start_year == year]$Cum_prob[1:11] - 1)
  #if (year == 2015){browser()}
  change_vector <- replace(change_vector, which(change_vector < 0), NA)

  N_dt[Start_year == year, Cum_prob_change := change_vector]

  
}
  

ggplot(N_dt, aes(x = Years_passed, y = Cum_prob))+
  geom_line(aes(color = as.factor(Start_year)), size = 1)+
  labs(x = "Eltelt évek", y = "Nemfizetés valószínűsége", col = "Hitel felvétel éve")+
  theme_economist()

# 7
Year_to_inspect <- 2015
N_dt[Start_year == Year_to_inspect]
# A "Default_prob" oszlopban láthatóak a vizsgált évben indult hitelek bedőlése
# a különböző években.

# 8
Year_to_inspect <- 2017
N_dt[Inspection_year == Year_to_inspect]
# A "Default_prob" oszlopban láthatóak a különböző években indult hitelek átlagos 
# nemfizetésének valószínűsége

# 9 
Year_to_inspect <- 2009

inspect_dt <- N_dt[Start_year == Year_to_inspect]
ggplot(inspect_dt, aes(x = Inspection_year, y = Default_prob))+
  geom_bar(stat = "identity", fill = "brown")+
  labs(x = "Vizsgált év", y = "Bedőlés valószínűsége")+
  theme_economist()


## Github link: 
# https://github.com/selmeczia/credit-risk-course



