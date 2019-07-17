library(sf)
pol = st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
h = st_sf(x = "a", pol)

test_that("poly_wkt works", {
  expect_equal(poly_wkt(h), "POLYGON((0 0,0 3,3 3,3 0,0 0))")
  expect_equal(poly_wkt(pol), "POLYGON((0 0,0 3,3 3,3 0,0 0))")
  expect_equal(poly_wkt(h, names = "x"),
               c(a = "POLYGON((0 0,0 3,3 3,3 0,0 0))")
  )
})

test_that("check_sf works", {
  expect_error(check_sf("Hello", names = NULL),
               "data must be either an sf or an sfc object")
  expect_error(check_sf(pol, names = "foo"),
               "'names' is only useful with sf objects")
  expect_error(check_sf(h, names = "foo"),
               "foo is not a column in 'data'")
  expect_silent(check_sf(h, names = "x"))
})

## make test for names that are not characters
