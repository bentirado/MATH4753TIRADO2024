test_that("Function tested with mu=10, sigma=5, a=6", {
  expect_equal(myncurve(10, 5, 6), list(mu=10, sigma=5, area=0.1891))
})
test_that("Function tested with mu=5, sigma=5, a=0", {
  expect_equal(myncurve(5, 5, 0), list(mu=5, sigma=5, area=0))
})

test_that("Function tested with mu=1, sigma=1, a=1", {
  expect_equal(myncurve(1, 1, 1), list(mu=1, sigma=1, area= 0.3413))
})
