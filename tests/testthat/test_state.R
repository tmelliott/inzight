test_that("Initializes correctly", {
    s <- inzight()
    expect_s3_class(s, "inzstate")
    expect_s3_class(s$docs, "inzdocs")
    expect_s3_class(s$controls, "inzcontrols")
})
