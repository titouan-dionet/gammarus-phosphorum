test_that("growth_rates_matrix calculates correct transitions", {
  # Configuration
  L_max <- 8.5
  delta_t <- 30
  theta <- 12
  class_lim <- c(1.5, 3.5, 5.2, 6, 7, 11)
  class_names <- c("J1", "J2", "A1", "A2", "A3")
  
  # Exécution
  result <- growth_rates_matrix(
    L_max = L_max, 
    delta_t = delta_t, 
    theta = theta, 
    class_lim = class_lim, 
    class_names = class_names
  )
  
  # Vérifications
  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(5, 5))
  expect_equal(colnames(result), class_names)
  expect_equal(rownames(result), class_names)
  expect_true(all(result >= 0 & result <= 1))
  
  # Vérifier que chaque colonne a une somme ≤ 1
  col_sums <- colSums(result)
  expect_true(all(col_sums <= 1 + 1e-10))  # Tolérance pour erreurs d'arrondi
})