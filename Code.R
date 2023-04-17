###
#Metoda 3 sigma - transformacja
###

lambda <- boxcox(data$price)

boxcox(as.numeric(data$price))
table(data$price<0)
install.packages("bestNormalize")
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

lof_scores <- lof(as.matrix(df$a), minPts = 54)
table(lof_scores)
inf_rows <- which(is.infinite(lof_scores))

outlier_LOF <- df[c(inf_rows), c("id", "a")]
outlier_LOF
max(outlier_LOF$a)