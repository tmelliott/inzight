test_that("Initializes correctly", {
    s <- inzight()
    expect_s3_class(s, "inzstate")
    expect_s3_class(s$documents, "inzdocuments")
    expect_s3_class(s$settings, "inzsettings")
    expect_s3_class(s$graph, "inzgraph")
})

test_that("Data can be loaded", {
    t <- tempfile(fileext = ".csv")
    on.exit(unlink(t))
    write.csv(iris, file = t, quote=F, row.names=F)

    a <- inzaction("LOAD_DATA", file = t, name = "iris")
    z <- inzight(inzight(), a)

    expect_equal(z$documents$docs[[1]]$store$get(), iris, ignore_attr = TRUE)
})

test_that("Variables can be set", {
    t <- tempfile(fileext = ".csv")
    on.exit(unlink(t))
    write.csv(iris, file = t, quote=F, row.names=F)

    a <- inzaction("LOAD_DATA", file = t, name = "iris")
    z0 <- inzight(inzight(), a)

    devtools::load_all()
    a <- inzaction("SET_V1", value = "Sepal.Length")
    z1 <- inzight(z0, a)
    expect_equal(
        z1$documents$docs[[1]]$controls$controls$v1$value,
        "Sepal.Length"
    )
})
