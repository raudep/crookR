test_that("crook_deform preserves nrow", {
  set.seed(1)
  xyz <- data.frame(X=rnorm(100,0,0.05), Y=rnorm(100,0,0.05), Z=runif(100,0,10))
  out <- crook_deform(xyz, krok_length=3, krok_start=5, krok_type="2dir",
                      krok_deviation=0.1, inflektion_X=0.5, inflektion_ext=0.5)
  expect_equal(nrow(out), nrow(xyz))
})


