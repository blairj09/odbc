# -- Host construction ----------------------------------------------------------

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

# -- Driver detection ----------------------------------------------------------

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

# -- Default args --------------------------------------------------------------

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

# -- JWT helpers ---------------------------------------------------------------

# Sample JWTs for testing (header.payload.signature):
# Azure AD token with upn and unique_name claims
azure_jwt <- "eyJhbGciOiJSUzI1NiJ9.eyJ1cG4iOiAidXNlckBleGFtcGxlLmNvbSIsICJ1bmlxdWVfbmFtZSI6ICJ1c2VyQGV4YW1wbGUuY29tIiwgInN1YiI6ICJvcGFxdWUtaWQiLCAiaXNzIjogImh0dHBzOi8vc3RzLndpbmRvd3MubmV0L3RlbmFudC1pZC8ifQ.signature"
# Databricks-native token with email in sub
db_jwt <- "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiAidXNlckBleGFtcGxlLmNvbSIsICJpc3MiOiAiaHR0cHM6Ly9hZGItMTIzLjQ1LmF6dXJlZGF0YWJyaWNrcy5uZXQvb2lkYyJ9.signature"
# Token with no email-like claims
no_email_jwt <- "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiAib3BhcXVlLWlkLW5vLWVtYWlsIiwgImlzcyI6ICJ0ZXN0In0.signature"

test_that("jwt_email extracts email from Azure AD tokens", {
  expect_equal(jwt_email(azure_jwt), "user@example.com")
})

test_that("jwt_email extracts email from Databricks-native tokens", {
  expect_equal(jwt_email(db_jwt), "user@example.com")
})

test_that("jwt_email returns NULL when no email claim exists", {
  expect_null(jwt_email(no_email_jwt))
})

test_that("jwt_email returns NULL for non-JWT strings", {
  expect_null(jwt_email("not-a-jwt"))
  expect_null(jwt_email(""))
})

test_that("jwt_claim extracts individual claims", {
  payload <- jwt_payload(azure_jwt)
  expect_equal(jwt_claim(payload, "upn"), "user@example.com")
  expect_equal(jwt_claim(payload, "unique_name"), "user@example.com")
  expect_null(jwt_claim(payload, "nonexistent"))
})

# -- Manual auth ---------------------------------------------------------------

test_that("uid and pwd suppress automated auth", {
  auth <- lakebase_auth_args("workspace", "instance-id", uid = "uid", pwd = "pwd")
  expect_equal(auth, list(uid = "uid", pwd = "pwd"))
})

test_that("must supply both uid and pwd", {
  expect_snapshot(
    lakebase_auth_args("workspace", "instance-id", uid = "uid"),
    error = TRUE
  )
})

# -- Workspace token discovery -------------------------------------------------

test_that("lakebase_workspace_token finds PAT", {
  withr::local_envvar(
    DATABRICKS_TOKEN = "my-pat",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = ""
  )
  expect_equal(lakebase_workspace_token("workspace"), "my-pat")
})

test_that("lakebase_workspace_token returns NULL when no credentials available", {
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = ""
  )
  local_mocked_bindings(
    is_hosted_session = function() TRUE
  )
  expect_null(lakebase_workspace_token("workspace"))
})

test_that("Workbench token is read from config file", {
  db_home <- tempfile("posit-workbench")
  dir.create(db_home)
  writeLines(
    c(
      '[workbench]',
      'host = workspace',
      'token = wb-token-123'
    ),
    file.path(db_home, "databricks.cfg")
  )
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = file.path(db_home, "databricks.cfg")
  )
  expect_equal(lakebase_workspace_token("workspace"), "wb-token-123")
})

test_that("Workbench credentials are ignored for other hosts", {
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
  local_mocked_bindings(
    is_hosted_session = function() TRUE
  )
  expect_null(lakebase_workspace_token("workspace"))
})

# -- Token exchange (generate-database-credential) -----------------------------

test_that("lakebase_generate_credential calls correct API endpoint", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  # Mock httr2 pipeline to capture the request
  captured_url <- NULL
  captured_token <- NULL
  captured_body <- NULL

  local_mocked_bindings(
    lakebase_generate_credential = function(workspace_url, token, instance_id) {
      list(token = "db-credential-token", expiration_time = "2025-12-31T00:00:00Z")
    }
  )

  cred <- lakebase_generate_credential("https://workspace.azuredatabricks.net", "bearer-token", "my-instance-id")
  expect_equal(cred$token, "db-credential-token")
})

# -- Full auth flow with token exchange ----------------------------------------

test_that("PAT triggers token exchange and JWT email extraction", {
  withr::local_envvar(
    DATABRICKS_TOKEN = azure_jwt,
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = ""
  )

  local_mocked_bindings(
    lakebase_generate_credential = function(workspace_url, token, instance_id) {
      list(token = "exchanged-credential")
    }
  )

  auth <- lakebase_auth_args("workspace", "instance-id")
  expect_equal(auth$uid, "user@example.com")
  expect_equal(auth$pwd, "exchanged-credential")
})

test_that("non-JWT PAT falls back to 'token' uid", {
  withr::local_envvar(
    DATABRICKS_TOKEN = "plain-pat-not-jwt",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = ""
  )

  local_mocked_bindings(
    lakebase_generate_credential = function(workspace_url, token, instance_id) {
      list(token = "exchanged-credential")
    }
  )

  auth <- lakebase_auth_args("workspace", "instance-id")
  expect_equal(auth$uid, "token")
  expect_equal(auth$pwd, "exchanged-credential")
})

test_that("Workbench flow uses token exchange", {
  db_home <- tempfile("posit-workbench")
  dir.create(db_home)
  writeLines(
    c(
      '[workbench]',
      'host = workspace',
      paste0('token = ', azure_jwt)
    ),
    file.path(db_home, "databricks.cfg")
  )
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CLIENT_ID = "",
    DATABRICKS_CONFIG_FILE = file.path(db_home, "databricks.cfg")
  )

  local_mocked_bindings(
    lakebase_generate_credential = function(workspace_url, token, instance_id) {
      list(token = "wb-exchanged-credential")
    }
  )

  auth <- lakebase_auth_args("workspace", "instance-id")
  expect_equal(auth$uid, "user@example.com")
  expect_equal(auth$pwd, "wb-exchanged-credential")
})

# -- lakebase_args integration -------------------------------------------------

test_that("errors if auth fails", {
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CONFIG_FILE = NULL,
    DATABRICKS_CLIENT_ID = ""
  )

  local_mocked_bindings(
    is_hosted_session = function() TRUE
  )

  lakebase_args1 <- function(...) {
    lakebase_args("instance-id", "database", workspace = "workspace", driver = "driver", ...)
  }

  expect_snapshot(. <- lakebase_args1(), error = TRUE)

  expect_silent(lakebase_args1(uid = "uid", pwd = "pwd"))
})

test_that("we hint viewer-based credentials on Connect", {
  local_mocked_bindings(
    running_on_connect = function() TRUE,
    is_hosted_session = function() TRUE
  )
  withr::local_envvar(
    DATABRICKS_TOKEN = "",
    DATABRICKS_CONFIG_FILE = NULL,
    DATABRICKS_CLIENT_ID = ""
  )
  expect_snapshot(
    lakebase_args(
      instance_id = "instance-id",
      database = "database",
      workspace = "workspace",
      driver = "driver"
    ),
    error = TRUE
  )
})

test_that("tokens can be requested from a Connect server", {
  skip_if_not_installed("connectcreds")

  local_mocked_bindings(
    lakebase_generate_credential = function(workspace_url, token, instance_id) {
      list(token = "connect-exchanged-credential")
    }
  )

  connectcreds::local_mocked_connect_responses(token = "token", user_name = "user@example.com")
  auth <- lakebase_auth_args("https://example.cloud.databricks.com", "instance-id")
  expect_equal(auth$uid, "user@example.com")
  expect_equal(auth$pwd, "connect-exchanged-credential")
})

test_that("manually supplied arguments override automatic", {
  local_mocked_bindings(
    lakebase_generate_credential = function(workspace_url, token, instance_id) {
      list(token = "exchanged")
    }
  )
  withr::local_envvar(DATABRICKS_TOKEN = azure_jwt)
  args <- lakebase_args("instance", "database", workspace = "workspace", driver = "driver")
  expect_equal(args$pwd, "exchanged")

  args <- lakebase_args("instance", "database", workspace = "workspace", driver = "driver", sslmode = "verify-full")
  expect_equal(args$sslmode, "verify-full")
})
