#Pakiety

if(!require(stats)) {
  install.packages("stats")
  library(stats)
}

cvm.test(data$price, pnorm, mean = mean(data$price), sd = sd(data$price))

####
#MAD
####

MAD <- mad(data$price)

median(data$price) + 3 * MAD

# Utworzenie data.frame dla wartości spełniających kryteria
outliers_MAD <- data %>% filter(price > (median(data$price) + 3 * MAD)) %>% select(id, price)
outliers_MAD

###
#Metoda 3 sigm - transformacja
###

lambda <- boxcox(data$price)

??boxcox
boxcox(as.numeric(data$price))
table(data$price<0)
install.packages("bestNormalize")
library("bestNormalize")

library(bestNormalize)

# Przefiltrowanie i usunięcie wartości nie-dodatnich i brakujących z wektora "price"
price <- data$price[!is.na(data$price) & data$price > 0]

# Wykonanie transformacji Box-Cox za pomocą funkcji bestNormalize
bc_price <- bestNormalize(price, method = "boxcox")

# Sprawdzenie wyestymowanej lambdy
bc_price$lambda

# Zastosowanie estymowanej transformacji Box-Cox do zmiennej "price"
data$price_bc <- predict(bc_price, data$price)

ad.test(x, pnorm, mean = mean(x), sd = sd(x))

x <- log(data$price)

####
#LOF
####

library(dbscan)

#Wybór minPts
data_price_group <- data
data_price_group$pg <- cut(data$price, breaks = seq(0, max(data$price), MAD), labels = FALSE)
df <- data.frame(table(data_price_group$pg)) 

df %>%
  filter(Freq <= 100 & Freq >= 50) %>% 
  arrange(-Freq)

#Wartość 90 za duża, bo daje zbyt dużą liczbę outlierów. Wybieram 54.

lof_scores <- lof(as.matrix(data$price), minPts = 20)
table(lof_scores)
inf_rows <- which(is.infinite(lof_scores))

outlier_LOF <- data[c(inf_rows), c("id", "price")]
outlier_LOF
max(outlier_LOF$price)
library(dplyr)
data %>% filter(price > 7000)