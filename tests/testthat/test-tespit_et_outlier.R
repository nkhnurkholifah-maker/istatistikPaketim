test_that("outliers are detected correctly", {
  veri <- c(10, 12, 11, 13, 12, 14, 15, 11, 12, 200)
  
  sonuc <- tespit_et_outlier(veri)
  
  expect_type(sonuc, "list")
  expect_named(sonuc, c("alt_sinir", "ust_sinir", "aykiri_degerler", "indisler"))
  expect_equal(sonuc$alt_sinir, 7.5)
  expect_equal(sonuc$ust_sinir, 17.5)
  expect_equal(sonuc$aykiri_degerler, 200)
  expect_equal(sonuc$indisler, 10)
})

test_that("no outliers are returned for regular data", {
  veri <- c(10, 11, 12, 13, 14, 15, 16)
  
  sonuc <- tespit_et_outlier(veri)
  
  expect_length(sonuc$aykiri_degerler, 0)
  expect_length(sonuc$indisler, 0)
})

test_that("function throws error for non-numeric input", {
  expect_error(
    tespit_et_outlier(c("a", "b", "c"))
  )
})

test_that("changing k still returns a valid result", {
  veri <- c(10, 12, 11, 13, 12, 14, 15, 11, 12, 200)
  
  sonuc <- tespit_et_outlier(veri, k = 1)
  
  expect_type(sonuc, "list")
  expect_named(sonuc, c("alt_sinir", "ust_sinir", "aykiri_degerler", "indisler"))
})