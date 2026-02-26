test_that("lakebase host construction works correctly", {
  expect_equal(
    lakebase_host("3860009d-c108-4632-945c-460ba27870e3", "https://adb-1234567890123456.7.azuredatabricks.net"),
    "instance-3860009d-c108-4632-945c-460ba27870e3.database.azuredatabricks.net"
  )
  expect_equal(
    lakebase_host("40386f23-47ae-4b62-96f5-b69e0e1d5a23", "https://adb-9876543210987654.3.azuredatabricks.net/"),
    "instance-40386f23-47ae-4b62-96f5-b69e0e1d5a23.database.azuredatabricks.net"
  )
  # Without https prefix
  expect_equal(
    lakebase_host("abc-123", "adb-1234567890123456.7.azuredatabricks.net"),
    "instance-abc-123.database.azuredatabricks.net"
  )
})

test_that("lakebase host validates inputs", {
  expect_snapshot(lakebase_host("instance-id", ""), error = TRUE)
})

test_that("fallbacks to driver name", {
  local_mocked_bindings(
    lakebase_default_driver_paths = function() character(),
    odbcListDrivers = function() list(name = c("bar", "PostgreSQL"))
  )
  expect_equal(lakebase_default_driver(), "PostgreSQL")
})

test_that("errors if can't find driver", {
  local_mocked_bindings(
    lakebase_default_driver_paths = function() character(),
    odbcListDrivers = function() list()
  )
  expect_snapshot(lakebase_default_driver(), error = TRUE)
})

test_that("errors if auth fails", {
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CONFIG_FILE = NULL,
    DATABRICKS_CLIENT_ID = ""
  )

  lakebase_args1 <- function(...) {
    lakebase_args("instance-id", "workspace", driver = "driver", ...)
  }

  expect_snapshot(. <- lakebase_args1(), error = TRUE)

  expect_silent(lakebase_args1(uid = "uid", pwd = "pwd"))
})

test_that("uid and pwd suppress automated auth", {
  auth <- lakebase_auth_args("workspace", uid = "uid", pwd = "pwd")
  expect_equal(auth, list(uid = "uid", pwd = "pwd"))
})

test_that("must supply both uid and pwd", {
  expect_snapshot(lakebase_auth_args("workspace", uid = "uid"), error = TRUE)
})

test_that("supports PAT in env var", {
  withr::local_envvar(DATABRICKS_TOKEN = "abc", DATABRICKS_USER = "user@example.com")
  auth <- lakebase_auth_args("workspace")
  expect_equal(auth$pwd, "abc")
  expect_equal(auth$uid, "user@example.com")
})

test_that("supports PAT with default uid", {
  withr::local_envvar(DATABRICKS_TOKEN = "abc", DATABRICKS_USER = "")
  auth <- lakebase_auth_args("workspace")
  expect_equal(auth$pwd, "abc")
  expect_equal(auth$uid, "token")
})

test_that("supports OAuth M2M in env var", {
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CLIENT_ID = "client-id",
    DATABRICKS_CLIENT_SECRET = "client-secret"
  )

  auth <- lakebase_auth_args("workspace")
  expect_equal(auth$uid, "client-id")
  expect_equal(auth$pwd, "client-secret")
})

test_that("Workbench-managed credentials are detected correctly", {
  # Emulate the databricks.cfg file written by Workbench.
  db_home <- tempfile("posit-workbench")
  dir.create(db_home)
  writeLines(
    c(
      '[workbench]',
      'host = workspace',
      'token = token123',
      'email = user@example.com'
    ),
    file.path(db_home, "databricks.cfg")
  )
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = file.path(db_home, "databricks.cfg")
  )
  auth <- lakebase_auth_args("workspace")
  expect_equal(auth$uid, "user@example.com")
  expect_equal(auth$pwd, "token123")
})

test_that("Workbench-managed credentials are ignored for other hosts", {
  # Emulate the databricks.cfg file written by Workbench.
  db_home <- tempfile("posit-workbench")
  dir.create(db_home)
  writeLines(
    c(
      '[workbench]',
      'host = nonmatching',
      'token = token'
    ),
    file.path(db_home, "databricks.cfg")
  )
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = file.path(db_home, "databricks.cfg")
  )
  expect_equal(lakebase_auth_args("workspace"), NULL)
})

test_that("we hint viewer-based credentials on Connect", {
  local_mocked_bindings(
    running_on_connect = function() TRUE
  )
  expect_snapshot(
    lakebase_args(
      instance_id = "instance-id",
      workspace = "workspace",
      driver = "driver"
    ),
    error = TRUE
  )
})

test_that("tokens can be requested from a Connect server", {
  skip_if_not_installed("connectcreds")

  connectcreds::local_mocked_connect_responses(token = "token", user_name = "user@example.com")
  auth <- lakebase_auth_args("https://example.cloud.databricks.com")
  expect_equal(auth$uid, "user@example.com")
  expect_equal(auth$pwd, "token")
})

test_that("default args include required PostgreSQL connection params", {
  local_mocked_bindings(
    lakebase_default_driver = function() "PostgreSQL"
  )
  args <- lakebase_default_args(NULL, "host", "database")
  expect_equal(args$driver, "PostgreSQL")
  expect_equal(args$server, "host")
  expect_equal(args$database, "database")
  expect_equal(args$port, 5432)
  expect_equal(args$sslmode, "require")
})

test_that("manually supplied arguments override automatic", {
  withr::local_envvar(DATABRICKS_TOKEN = "abc")
  args <- lakebase_args("instance", "workspace", driver = "driver")
  expect_equal(args$pwd, "abc")

  args <- lakebase_args("instance", "workspace", driver = "driver", sslmode = "verify-full")
  expect_equal(args$sslmode, "verify-full")
})
