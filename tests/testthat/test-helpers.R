test_that('FALSE when `IDB_API` is not available in the environment', {
  expect_false(idb_api_key_available())
})
