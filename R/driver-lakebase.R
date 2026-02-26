#' @include dbi-connection.R
#' @include dbi-driver.R
NULL


#' Helper for Connecting to Databricks Lakebase via ODBC
#'
#' @description
#'
#' Connect to Databricks Lakebase (managed PostgreSQL service) via the
#' PostgreSQL ODBC driver.
#'
#' In particular, the custom `dbConnect()` method for the Lakebase ODBC driver
#' implements a subset of the [Databricks client unified authentication](https://docs.databricks.com/en/dev-tools/auth.html#databricks-client-unified-authentication)
#' model, with support for personal access tokens, OAuth machine-to-machine
#' credentials, and OAuth user-to-machine credentials supplied via Posit
#' Workbench or the Databricks CLI on desktop. It can also detect viewer-based
#' credentials on Posit Connect if the \pkg{connectcreds} package is
#' installed. All of these credentials are detected automatically if present
#' using [standard environment variables](https://docs.databricks.com/en/dev-tools/auth.html#environment-variables-and-fields-for-client-unified-authentication).
#'
#' @param drv an object that inherits from [DBI::DBIDriver-class],
#' or an existing [DBI::DBIConnection-class]
#' object (in order to clone an existing connection).
#' @param instance_id The UUID identifying the Lakebase instance, e.g.
#'   `"3860009d-c108-4632-945c-460ba27870e3"`.
#' @param workspace The URL of a Databricks workspace, e.g.
#'   `"https://adb-1234567890123456.7.azuredatabricks.net"`. Defaults to the
#'   `DATABRICKS_HOST` environment variable.
#' @param database The name of the database to connect to.
#' @param driver The name of the PostgreSQL ODBC driver, or `NULL` to use the
#'   default name.
#' @param uid,pwd Manually specify a username and password for authentication.
#'
#'   For Lakebase, use your email as `uid` and a Databricks personal access
#'   token as `pwd`. Specifying these options will disable automated credential
#'   discovery.
#' @param ... Further arguments passed on to [`dbConnect()`].
#'
#' @returns An `OdbcConnection` object with an active connection to a Databricks
#'   Lakebase instance.
#'
#' @examples
#' \dontrun{
#' DBI::dbConnect(
#'   odbc::lakebase(),
#'   instance_id = "3860009d-c108-4632-945c-460ba27870e3",
#'   database = "my_database"
#' )
#'
#' # Use credentials from the viewer (when possible) in a Shiny app
#' # deployed to Posit Connect.
#' library(connectcreds)
#' server <- function(input, output, session) {
#'   conn <- DBI::dbConnect(
#'     odbc::lakebase(),
#'     instance_id = "3860009d-c108-4632-945c-460ba27870e3",
#'     database = "my_database"
#'   )
#' }
#' }
#' @export
lakebase <- function() {
  new("LakebaseOdbcDriver")
}

#' @rdname lakebase
#' @export
setClass("LakebaseOdbcDriver", contains = "OdbcDriver")

#' @rdname lakebase
#' @export
setMethod("dbConnect", "LakebaseOdbcDriver",
  function(drv,
           instance_id,
           database,
           workspace = Sys.getenv("DATABRICKS_HOST"),
           driver = NULL,
           uid = NULL,
           pwd = NULL,
           ...) {
    call <- caller_env()
    check_string(instance_id, call = call)
    check_string(workspace, allow_null = TRUE, call = call)
    check_string(database, call = call)
    check_string(driver, allow_null = TRUE, call = call)
    check_string(uid, allow_null = TRUE, call = call)
    check_string(pwd, allow_null = TRUE, call = call)

    args <- lakebase_args(
      instance_id = instance_id,
      workspace = workspace,
      database = database,
      driver = driver,
      uid = uid,
      pwd = pwd,
      ...
    )
    inject(dbConnect(odbc(), !!!args))
  }
)

lakebase_args <- function(instance_id,
                          database,
                          workspace = Sys.getenv("DATABRICKS_HOST"),
                          driver = NULL,
                          uid = NULL,
                          pwd = NULL,
                          ...) {
  host <- lakebase_host(instance_id, workspace)

  args <- lakebase_default_args(
    driver = driver,
    host = host,
    database = database
  )

  auth <- lakebase_auth_args(workspace, instance_id = instance_id, uid = uid, pwd = pwd)
  all <- utils::modifyList(c(args, auth), list(...))

  arg_names <- tolower(names(all))
  if (!all(c("uid", "pwd") %in% arg_names)) {
    msg <- c(
      "Failed to detect ambient Databricks credentials.",
      "i" = "Supply {.arg uid} and {.arg pwd} to authenticate manually."
    )
    if (running_on_connect()) {
      msg <- c(
        msg,
        "i" = "Or consider enabling Posit Connect's Databricks integration \
              for viewer-based credentials. See {.url \
              https://docs.posit.co/connect/user/oauth-integrations/#adding-oauth-integrations-to-deployed-content}
              for details."
      )
    }
    cli::cli_abort(msg, call = quote(DBI::dbConnect()))
  }

  all
}

lakebase_default_args <- function(driver, host, database) {
  list(
    driver = driver %||% lakebase_default_driver(),
    server = host,
    database = database,
    port = 5432,
    sslmode = "require"
  )
}

# Returns a sensible driver name even if odbc.ini and odbcinst.ini do not
# contain an entry for the PostgreSQL ODBC driver. For Linux and macOS we
# default to known shared library paths used by the official installers.
# On Windows we use the official driver name.
lakebase_default_driver <- function() {
  find_default_driver(
    lakebase_default_driver_paths(),
    fallbacks = c("PostgreSQL", "PostgreSQL ANSI", "PostgreSQL Unicode"),
    label = "PostgreSQL",
    call = quote(DBI::dbConnect())
  )
}

lakebase_default_driver_paths <- function() {
  if (Sys.info()["sysname"] == "Linux") {
    c(
      "/opt/rstudio-drivers/postgresql/bin/lib/libpostgresqlodbc.so",
      "/usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so",
      "/usr/lib/aarch64-linux-gnu/odbc/psqlodbcw.so"
    )
  } else if (Sys.info()["sysname"] == "Darwin") {
    c(
      "/opt/homebrew/lib/psqlodbcw.so",
      "/usr/local/lib/psqlodbcw.so"
    )
  } else {
    character()
  }
}

lakebase_host <- function(instance_id, workspace) {
  if (nchar(workspace) == 0) {
    abort(
      c(
        "No Databricks workspace URL provided.",
        i = "Either supply `workspace` argument or set env var `DATABRICKS_HOST`."
      ),
      call = quote(DBI::dbConnect())
    )
  }

  # Extract the base domain from the workspace URL

# Example workspace: "https://adb-1234567890123456.7.azuredatabricks.net"
  # We want: "azuredatabricks.net"
  workspace_clean <- gsub("https://|/$", "", workspace)
  # Remove the adb-*.*.  prefix to get the base domain
  domain <- gsub("^[^.]+\\.[^.]+\\.", "", workspace_clean)

  paste0("instance-", instance_id, ".database.", domain)
}

lakebase_auth_args <- function(workspace, instance_id, uid = NULL, pwd = NULL) {
  # Detect viewer-based credentials from Posit Connect.
  workspace_url <- if (grepl("^https://", workspace)) {
    workspace
  } else {
    paste0("https://", workspace)
  }

  if (is_installed("connectcreds") && connectcreds::has_viewer_token(workspace_url)) {
    viewer_token <- connectcreds::connect_viewer_token(workspace_url)
    cred <- lakebase_generate_credential(workspace_url, viewer_token$access_token, instance_id)
    return(list(
      uid = viewer_token$user_name %||% jwt_email(viewer_token$access_token) %||% "token",
      pwd = cred$token
    ))
  }

  if (!is.null(uid) && !is.null(pwd)) {
    return(list(uid = uid, pwd = pwd))
  } else if (xor(is.null(uid), is.null(pwd))) {
    abort(
      c(
        "Both `uid` and `pwd` must be specified for manual authentication.",
        i = "Or leave both unset for automated authentication."
      ),
      call = quote(DBI::dbConnect())
    )
  }

  # Obtain a Databricks workspace token via one of several methods, then
  # exchange it for a Lakebase database credential.
  workspace_token <- lakebase_workspace_token(workspace)

  if (is.null(workspace_token)) {
    return(NULL)
  }

  email <- jwt_email(workspace_token) %||% "token"
  cred <- lakebase_generate_credential(workspace_url, workspace_token, instance_id)

  list(
    uid = email,
    pwd = cred$token
  )
}

# Obtains a Databricks workspace token from environment variables, Workbench
# config, or the Databricks CLI. Returns NULL if no token can be found.
lakebase_workspace_token <- function(workspace) {
  token <- Sys.getenv("DATABRICKS_TOKEN")
  client_id <- Sys.getenv("DATABRICKS_CLIENT_ID")
  client_secret <- Sys.getenv("DATABRICKS_CLIENT_SECRET")
  cli_path <- Sys.getenv("DATABRICKS_CLI_PATH", "databricks")
  cfg_file <- Sys.getenv("DATABRICKS_CONFIG_FILE")

  if (nchar(token) != 0) {
    return(token)
  }

  if (nchar(client_id) != 0 && nchar(client_secret) != 0) {
    return(lakebase_m2m_token(workspace, client_id, client_secret))
  }

  # Check for Workbench-provided credentials.
  if (grepl("posit-workbench", cfg_file, fixed = TRUE)) {
    wb_token <- workbench_databricks_token(
      gsub("https://|/$", "", workspace),
      cfg_file
    )
    if (!is.null(wb_token)) {
      return(wb_token)
    }
  }

  # When on desktop, try using the Databricks CLI for auth.
  if (!is_hosted_session() && nchar(Sys.which(cli_path)) != 0) {
    host <- gsub("https://|/$", "", workspace)
    output <- suppressWarnings(
      system2(
        cli_path,
        c("auth", "token", "--host", host),
        stdout = TRUE,
        stderr = TRUE
      )
    )
    output <- paste(output, collapse = "\n")
    if (grepl("access_token", output, fixed = TRUE)) {
      return(gsub(".*access_token\":\\s?\"([^\"]+).*", "\\1", output))
    }
  }

  NULL
}

# Exchanges OAuth M2M credentials for a Databricks workspace token.
lakebase_m2m_token <- function(workspace, client_id, client_secret) {
  check_installed("httr2", reason = "for OAuth M2M authentication with Lakebase.")

  workspace_url <- if (grepl("^https://", workspace)) {
    workspace
  } else {
    paste0("https://", workspace)
  }

  token_url <- paste0(gsub("/$", "", workspace_url), "/oidc/v1/token")
  resp <- httr2::request(token_url) |>
    httr2::req_body_form(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret,
      scope = "all-apis"
    ) |>
    httr2::req_error(body = function(resp) {
      httr2::resp_body_json(resp)$error_description %||%
        httr2::resp_body_json(resp)$error %||%
        "Unknown error"
    }) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)
  body$access_token
}

# Exchanges a Databricks workspace token for a Lakebase database credential.
lakebase_generate_credential <- function(workspace_url, token, instance_id) {
  check_installed("httr2", reason = "for Lakebase credential generation.")

  api_url <- paste0(
    gsub("/$", "", workspace_url),
    "/api/2.0/database/generate-database-credential"
  )

  resp <- httr2::request(api_url) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(instance_names = list(instance_id))) |>
    httr2::req_error(body = function(resp) {
      httr2::resp_body_json(resp)$message %||%
        httr2::resp_body_json(resp)$error %||%
        "Unknown error"
    }) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)
}

# Extracts the user's email address from a JWT token by checking common claims.
jwt_email <- function(token) {
  payload <- jwt_payload(token)
  if (is.null(payload)) return(NULL)

  # Try claims in order of preference:
  # - upn / unique_name: Azure AD tokens
  # - sub: Databricks-native tokens (contains email directly)
  for (claim in c("upn", "unique_name", "sub")) {
    value <- jwt_claim(payload, claim)
    if (!is.null(value) && grepl("@", value, fixed = TRUE)) {
      return(value)
    }
  }

  NULL
}

# Decodes the payload (second segment) of a JWT token.
# Returns the raw JSON string, or NULL if decoding fails.
jwt_payload <- function(token) {
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3) return(NULL)

  payload <- parts[2]
  # Convert base64url to base64
  payload <- chartr("-_", "+/", payload)
  # Add padding
  padding <- (4 - nchar(payload) %% 4) %% 4
  payload <- paste0(payload, strrep("=", padding))

  tryCatch(
    rawToChar(jsonlite::base64_dec(payload)),
    error = function(e) NULL
  )
}

# Extracts a single claim value from a JSON payload string using regex.
jwt_claim <- function(payload, claim) {
  pattern <- paste0('"', claim, '"\\s*:\\s*"([^"]+)"')
  m <- regmatches(payload, regexpr(pattern, payload, perl = TRUE))
  if (length(m) == 0 || m == "") return(NULL)
  sub(pattern, "\\1", m, perl = TRUE)
}
