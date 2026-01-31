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
#' @param database The database name. Defaults to `"databricks_postgres"`.
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
#'   instance_id = "3860009d-c108-4632-945c-460ba27870e3"
#' )
#'
#' # Use credentials from the viewer (when possible) in a Shiny app
#' # deployed to Posit Connect.
#' library(connectcreds)
#' server <- function(input, output, session) {
#'   conn <- DBI::dbConnect(
#'     odbc::lakebase(),
#'     instance_id = "3860009d-c108-4632-945c-460ba27870e3"
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
           workspace = Sys.getenv("DATABRICKS_HOST"),
           database = "databricks_postgres",
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
                          workspace = Sys.getenv("DATABRICKS_HOST"),
                          database = "databricks_postgres",
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

  auth <- lakebase_auth_args(workspace, uid = uid, pwd = pwd)
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
    port = 443,
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

lakebase_auth_args <- function(workspace, uid = NULL, pwd = NULL) {
  # Detect viewer-based credentials from Posit Connect.
  workspace_url <- if (grepl("^https://", workspace)) {
    workspace
  } else {
    paste0("https://", workspace)
  }

  if (is_installed("connectcreds") && connectcreds::has_viewer_token(workspace_url)) {
    token <- connectcreds::connect_viewer_token(workspace_url)
    # For Lakebase/PostgreSQL auth, use the token as the password
    return(list(
      uid = token$user_name %||% "token",
      pwd = token$access_token
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

  # Check some standard Databricks environment variables. This is used to
  # implement a subset of the "Databricks client unified authentication" model.
  token <- Sys.getenv("DATABRICKS_TOKEN")
  client_id <- Sys.getenv("DATABRICKS_CLIENT_ID")
  client_secret <- Sys.getenv("DATABRICKS_CLIENT_SECRET")
  cli_path <- Sys.getenv("DATABRICKS_CLI_PATH", "databricks")
  cfg_file <- Sys.getenv("DATABRICKS_CONFIG_FILE")

  # Check for Workbench-provided credentials.
  wb_token <- NULL
  wb_email <- NULL
  if (grepl("posit-workbench", cfg_file, fixed = TRUE)) {
    wb_creds <- workbench_lakebase_credentials(workspace, cfg_file)
    wb_token <- wb_creds$token
    wb_email <- wb_creds$email
  }

  if (nchar(token) != 0) {
    # An explicit PAT takes precedence over everything else.
    # For Lakebase, the user typically uses their email as uid
    databricks_user <- Sys.getenv("DATABRICKS_USER")
    list(
      uid = if (nchar(databricks_user) > 0) databricks_user else "token",
      pwd = token
    )
  } else if (nchar(client_id) != 0) {
    # OAuth2 M2M credentials
    list(
      uid = client_id,
      pwd = client_secret
    )
  } else if (!is.null(wb_token)) {
    # Workbench-provided credentials.
    list(
      uid = wb_email %||% "token",
      pwd = wb_token
    )
  } else if (!is_hosted_session() && nchar(Sys.which(cli_path)) != 0) {
    # When on desktop, try using the Databricks CLI for auth.
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
    # If we don't get an error message, try to extract the token from the JSON-
    # formatted output.
    if (grepl("access_token", output, fixed = TRUE)) {
      cli_token <- gsub(".*access_token\":\\s?\"([^\"]+).*", "\\1", output)
      list(
        uid = "token",
        pwd = cli_token
      )
    }
  }
}

# Reads Posit Workbench-managed Databricks credentials from a
# $DATABRICKS_CONFIG_FILE. Returns both token and email if available.
workbench_lakebase_credentials <- function(workspace, cfg_file) {
  cfg <- readLines(cfg_file)
  host <- gsub("https://|/$", "", workspace)

  if (!any(grepl(host, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this host.
    return(list(token = NULL, email = NULL))
  }

  token_line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[token_line])
  if (nchar(token) == 0) {
    token <- NULL
  }

  # Try to get email from config if available
  email <- NULL
  email_line <- grepl("email = |user = ", cfg)
  if (any(email_line)) {
    email <- gsub("(email|user) = ", "", cfg[email_line][1])
    if (nchar(email) == 0) {
      email <- NULL
    }
  }

  list(token = token, email = email)
}
