test_that(
  desc = "SQL filter",
  code = {
    expect_equal(
      object = getSQL_filter("dummy_column", "value"),
      expected = "dummy_column IN ('value')"
    )
    expect_equal(
      object = getSQL_filter("dummy_column", c("val1", "val2", "val3")),
      expected = "dummy_column IN ('val1','val2','val3')"
    )
    expect_null(getSQL_filter("dummy_column", NULL))
  }
)

test_that(
  desc = "SQL conditions",
  code = {
    expect_null(prepareConditionSql())
    expect_equal(
      object = prepareConditionSql(dummy = "val"),
      expected = "dummy = 'val'"
    )
    expect_null(prepareConditionSql(dummy = NULL))
    expect_null(prepareConditionSql(dummy = c()))
    expect_equal(
      object = prepareConditionSql(dummy = "val", foo = "bar"),
      expected = "dummy = 'val' AND foo = 'bar'"
    )
    expect_equal(
      object = prepareConditionSql(foo = "bar", dummy = "val"),
      expected = "foo = 'bar' AND dummy = 'val'"
    )
    expect_equal(
      object = prepareConditionSql(foo = "bar", dummy = NULL),
      expected = "foo = 'bar'"
    )
    expect_null(prepareConditionSql(foo = NULL, dummy = NULL))
    expect_equal(
      object = prepareConditionSql(dummy = c("val1", "val2", "val3")),
      expected = "dummy IN ('val1','val2','val3')"
    )
    expect_equal(
      object = prepareConditionSql(dummy = c("val1", "val2", "val3"), foo = "bar"),
      expected = "dummy IN ('val1','val2','val3') AND foo = 'bar'"
    )
    expect_equal(
      object = prepareConditionSql(dummy = c("val1", "val2", "val3"), foo = c("v1", "v2", "v3")),
      expected = "dummy IN ('val1','val2','val3') AND foo IN ('v1','v2','v3')"
    )
    expect_equal(
      object = prepareConditionSql(dummy = c("val1", "val2", "val3"), x = NULL, foo = c("v1", "v2", "v3")),
      expected = "dummy IN ('val1','val2','val3') AND foo IN ('v1','v2','v3')"
    )
  }
)
