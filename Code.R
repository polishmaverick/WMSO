#
data_one_dim <- data

#Zdefiniowanie dla każdej z metod zmiennej om (outliers membership) świadczącej o przynależności do grupy outlierów
data_one_dim$om1 <- as.numeric(data$id %in% outliers_threesigma$id)
data_one_dim$om2 <- as.numeric(data$id %in% outliers_Tukey$id)
data_one_dim$om3 <- as.numeric(data$id %in% outliers_hampel$id)
data_one_dim$om4 <- as.numeric(data$id %in% outliers_zscore_sd$id)
data_one_dim$om5 <- as.numeric(data$id %in% outliers_zscore_mad$id)

#Wektor z nazwami wszystkich kolumn om
outliers_membership <- grep("om.$", names(data_one_dim), value=TRUE)

#Wyliczenie liczby wystąpień 1 w poszczególnych kolumnach om1-om5
data_one_dim$count <- rowSums(data_one_dim[, outliers_membership])

#Wyświetlenie ramki danych z kolumnami id i count
data_one_dim[order(data_one_dim$count, decreasing = TRUE), ]

#Tabela
table(data_one_dim$count)

colSums(data_one_dim[data_one_dim$count == 3, c("id", outliers_membership)])

#Wartość dla zmiennej 'price', od której zaczyna się grupa outlierów (W oparciu o wszystkie metody)
outlier_one_dim_min <- min(data_one_dim[data_one_dim$count == 5, c("id", "price")])
outlier_one_dim_min

#Obliczenie kwantyli
quantile(data$price, probs = seq(0.01, 1, 0.01), names = FALSE)

#Do wykrycia / wyboru wartości odstających można podejść podobnie, jak do prognoz, które stawiane są różnymi metodami
#Racjonalnym wyborem wydaje się być wyciąganie średniej z prognoz
#W przypadku wartości odstających analogią może być wybór tych obserwacji, które zostały zaklasyfikowane jako
#outliery przez wszystkie metody lub jak największą liczbę metod.