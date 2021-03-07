## Excercise 2

# 1

M <- matrix(sample(0:1, 100, replace = T), 10, 10)

# 2

rowsum <- rowSums(M)

# 3

K <- t(matrix(1:100, 10, 10))

# 4

K_squared <- K %*% K

# 5

N <- matrix(as.integer(K_squared %% 2 == 0), 10, 10)

# 6

fibonacci <- function(n){
  if(n <= 1){
    return(n)
  } else {
    return(fibonacci(n-1) + fibonacci(n-2))
  }
}

n <- 1
k <- 10

for (i in n:k){
  cat(paste0("Element ", i, ": ", fibonacci(i)), "\n")
}

# 7 

keresztnevek <- c("Toni", "Alexandra", "Adam", "Andreasz", "Akos")
szuletesi_evek <- c(1996, 1996, 1996, 1995, 1996)
magassagok <- c(170, 165, 168, 175, 185)
nemek <- c("ferfi", "no", "ferfi", "ferfi", "ferfi")
elkotelezettsegek <- c(75, 90, 80, 85, 90)

csapat_df <- data.frame("keresztnev" = keresztnevek,
                          "szuletesi_ev" = szuletesi_evek,
                          "magassag" = magassagok,
                          "nem" = nemek,
                          "elkotelezettseg" = elkotelezettsegek)
csapat_df$keresztnev <- as.character(csapat_df$keresztnev) 

egyedi_keresztnevek <- unique(csapat_df$keresztnev)
atlagos_ferfi_magassag <- mean(subset(csapat_df, nem == "ferfi")$magassag)
atlagos_noi_eletkor <- as.integer(format(Sys.Date(), "%Y")) -
  mean(subset(csapat_df, nem == "no")$szuletesi_ev)
eloszlas_180_alatt <- table(subset(csapat_df, magassag < 180)$nem)


## Github link: https://github.com/selmeczia/credit-risk-course 



