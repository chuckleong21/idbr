test_that('FALSE when `IDB_API` is not available in the environment', {
  expect_false(idb_api_key_available())
})

test_that("default variables should not be passed to argument", {
  expect_error(api_conflict("genc"))
  expect_error(api_conflict("geocomp"))
  expect_error(api_conflict("geo_id"))
  expect_error(api_conflict("pop"))
  expect_error(api_conflict("sumlevel"))
  expect_error(api_conflict("yr"))
  expect_error(api_conflict("for"))
  expect_error(api_conflict("in"))
  expect_error(api_conflict("time"))
  expect_error(api_conflict("ucgid"))
})

test_that("API level should be homogeneous", {
  expect_error(api_conflict(c("age", "births")))
  expect_no_error(api_conflict(c("sex", "age")))
})

test_that("Countries defined as invalid would return error", {
  expect_error(country_check("AQ"))
  expect_error(country_check("UK"))
  expect_no_error(country_check("GB"))
})
