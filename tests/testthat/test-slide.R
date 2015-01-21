test_that('slide works with ungrouped data', {
    expect_equivalent(object = slide(cars, Var = 'dist', slideBy = -2)[, 3],
    expected = c(NA, NA, 2, 10, 4, 22, 16, 10, 18, 26, 34, 17, 28, 14, 20, 24,
        28, 26, 34, 34, 46, 26, 36, 60, 80, 20, 26, 54, 32, 40, 32, 40, 50, 42,
        56, 76, 84, 36, 46, 68, 32, 48, 52, 56, 64, 66, 54, 70, 92, 93))
})
