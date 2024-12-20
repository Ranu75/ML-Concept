# Générer des échantillons normaux
norm_a <- rnorm(n = 500, mean = 0, sd = 1)
norm_b <- rnorm(n = 500, mean = 0.1, sd = 1)

# Générer un échantillon avec une distribution F
f_a <- rf(n = 500, df1 = 5, df2 = 10)

# Visualisation la distribution des échantillons
## Définir les couleurs pour chaque histogramme
colors <- c(rgb(1, 0, 0, 0.5),   # Rouge transparent
            rgb(0, 0, 1, 0.5),   # Bleu transparent
            rgb(0.5, 0, 0.5, 0.5)) # Violet transparent

## Créer le premier histogramme
hist(norm_a, col = colors[1], freq = FALSE, main = "Histogrammes Superposés", 
     xlab = "Valeurs", ylab = "Densité", xlim = c(-3, 5), ylim = c(0, 0.5), border = "white")

## Ajouter les autres histogrammes
hist(norm_b, col = colors[2], freq = FALSE, add = TRUE, border = "white")
hist(f_a, col = colors[3], freq = FALSE, add = TRUE, border = "white")

## Ajouter une légende
legend("topright", legend = c("norm_a", "norm_b", "norm_c", "f_a"), fill = colors)

# Test de Kolmogorov-Smirnov -- Tester la normalité des échantillons
ks.test(norm_a, "pnorm", mean = mean(norm_a), sd = sd(norm_a)) # p-value non significative, on ne rejette pas l'hypothèse nulle --> suit une loi normale
ks.test(f_a, "pnorm", mean = mean(f_a), sd = sd(f_a)) # p-value significative, on rejette l'hypothèse nulle --> ne suit pas une loi normale

# Test de Kolmogorov-Smirnov -- Tester la loi suivie par les deux distributions
ks.test(norm_a, norm_b) # p-value non significative, on ne rejette pas l'hypothèse nulle --> les deux distributions suivent une même loi
ks.test(norm_a, f_a) # p-value significative, on rejette l'hypothèse nulle --> les deux distributions ne suivent pas la même loi

