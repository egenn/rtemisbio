
# gene2sequence
test_that("gene2sequence works", {
  expect_s3_class(gene2sequence("MAPT"), "data.frame")
})

# aa_sub
test_that("aa_sub works", {
  expect_equal(aa_sub("ACDEFG", c("C2A")), c("A", "A", "D", "E", "F", "G"))
})
