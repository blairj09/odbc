#' @include dbi-connection.R
#' @include dbi-driver.R
NULL


#' Helper for Connecting to Databricks via ODBC
#'
#' @description
#'
#' Connect to Databricks clusters and SQL warehouses via the [Databricks ODBC
#' driver](https://www.databricks.com/spark/odbc-drivers-download).
#'
#' In particular, the custom `dbConnect()` method for the Databricks ODBC driver
#' implements a subset of the [Databricks client unified authentication](https://docs.databricks.com/en/dev-tools/auth.html#databricks-client-unified-authentication)
#' model, with support for personal access tokens, OAuth machine-to-machine
#' credentials, and OAuth user-to-machine credentials supplied via Posit
#' Workbench or the Databricks CLI on desktop. It can also detect viewer-based
#' credentials on Posit Connect if the \pkg{connectcreds} package is
#' installed. All of these credentials are detected automatically if present
#' using [standard environment variables](https://docs.databricks.com/en/dev-tools/auth.html#environment-variables-and-fields-for-client-unified-authentication).
#'
#' In addition, on macOS platforms, the `dbConnect()` method will check
#' for irregularities with how the driver is configured,
#' and attempt to fix in-situ, unless the `odbc.no_config_override`
#' environment variable is set.
#'
#' @inheritParams DBI::dbConnect
#' @param httpPath,HTTPPath To query a cluster, use the HTTP Path value found
#'   under `Advanced Options > JDBC/ODBC` in the Databricks UI. For SQL
#'   warehouses, this is found under `Connection Details` instead.
#' @param useNativeQuery Suppress the driver's conversion from ANSI SQL 92 to
#'   HiveSQL? The default (`TRUE`), gives greater performance but means that
#'   paramterised queries (and hence `dbWriteTable()`) do not work.
#' @param workspace The URL of a Databricks workspace, e.g.
#'   `"https://example.cloud.databricks.com"`.
#' @param driver The name of the Databricks ODBC driver, or `NULL` to use the
#'   default name.
#' @param uid,pwd Manually specify a username and password for authentication.
#'   Specifying these options will disable automated credential discovery.
#' @param ... Further arguments passed on to [`dbConnect()`].
#'
#' @returns An `OdbcConnection` object with an active connection to a Databricks
#'   cluster or SQL warehouse.
#'
#' @examples
#' \dontrun{
#' DBI::dbConnect(
#'   odbc::databricks(),
#'   httpPath = "sql/protocolv1/o/4425955464597947/1026-023828-vn51jugj"
#' )
#'
#' # Use credentials from the viewer (when possible) in a Shiny app
#' # deployed to Posit Connect.
#' library(connectcreds)
#' server <- function(input, output, session) {
#'   conn <- DBI::dbConnect(
#'     odbc::databricks(),
#'     httpPath = "sql/protocolv1/o/4425955464597947/1026-023828-vn51jugj"
#'   )
#' }
#' }
#' @export
databricks <- function() {
  new("DatabricksOdbcDriver")
}

#' @rdname databricks
#' @export
setClass("DatabricksOdbcDriver", contains = "OdbcDriver")

#' @rdname databricks
#' @export
setMethod("dbConnect", "DatabricksOdbcDriver",
  function(drv,
           httpPath,
           workspace = Sys.getenv("DATABRICKS_HOST"),
           useNativeQuery = TRUE,
           driver = NULL,
           HTTPPath,
           uid = NULL,
           pwd = NULL,
           ...) {
    call <- caller_env()
    # For backward compatibility with RStudio connection string
    http_path <- check_exclusive(httpPath, HTTPPath, .call = call)
    check_string(get(http_path), allow_null = TRUE, arg = http_path, call = call)
    check_string(workspace, allow_null = TRUE, call = call)
    check_bool(useNativeQuery, call = call)
    check_string(driver, allow_null = TRUE, call = call)
    check_string(uid, allow_null = TRUE, call = call)
    check_string(pwd, allow_null = TRUE, call = call)

    args <- databricks_args(
      httpPath = if (missing(httpPath)) HTTPPath else httpPath,
      workspace = workspace,
      useNativeQuery = useNativeQuery,
      driver = driver,
      uid = uid,
      pwd = pwd,
      ...
    )
    # Perform some sanity checks on MacOS
    configure_simba(spark_simba_config(args$driver),
      action = "modify")
    inject(dbConnect(odbc(), !!!args))
  }
)

databricks_args <- function(httpPath,
                            workspace = Sys.getenv("DATABRICKS_HOST"),
                            useNativeQuery = FALSE,
                            driver = NULL,
                            uid = NULL,
                            pwd = NULL,
                            ...) {
  host <- databricks_host(workspace)

  args <- databricks_default_args(
    driver = driver,
    host = host,
    httpPath = httpPath,
    useNativeQuery = useNativeQuery
  )

  auth <- databricks_auth_args(host, uid = uid, pwd = pwd)
  all <- utils::modifyList(c(args, auth), list(...))

  arg_names <- tolower(names(all))
  if (!"authmech" %in% arg_names && !all(c("uid", "pwd") %in% arg_names)) {
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

databricks_default_args <- function(driver, host, httpPath, useNativeQuery) {
  list(
    driver = driver %||% databricks_default_driver(),
    host = host,
    httpPath = httpPath,
    thriftTransport = 2,
    userAgentEntry = databricks_user_agent(),
    useNativeQuery = as.integer(useNativeQuery),
    # Connections to Databricks are always over HTTPS.
    port = 443,
    protocol = "https",
    ssl = 1
  )
}



# Returns a sensible driver name even if odbc.ini and odbcinst.ini do not
# contain an entry for the Databricks ODBC driver. For Linux and macOS we
# default to known shared library paths used by the official installers.
# On Windows we use the official driver name.
databricks_default_driver <- function() {
  find_default_driver(
    databricks_default_driver_paths(),
    fallbacks = c("Databricks", "Simba Spark ODBC Driver"),
    label = "Databricks/Spark",
    call = quote(DBI::dbConnect())
  )
}

databricks_default_driver_paths <- function() {
  if (Sys.info()["sysname"] == "Linux") {
    paths <- Sys.glob(c(
      "/opt/rstudio-drivers/spark/bin/lib/libsparkodbc_sb*.so",
      "/opt/simba/spark/lib/64/libsparkodbc_sb*.so"
    ))
  } else if (Sys.info()["sysname"] == "Darwin") {
    paths <- Sys.glob("/Library/simba/spark/lib/libsparkodbc_sb*.dylib")
  } else {
    paths <- character()
  }
  paths
}

databricks_host <- function(workspace) {
  if (nchar(workspace) == 0) {
    abort(
      c(
        "No Databricks workspace URL provided.",
        i = "Either supply `workspace` argument or set env var `DATABRICKS_HOST`."
      ),
      call = quote(DBI::dbConnect())
    )
  }
  gsub("https://|/$", "", workspace)
}

#' @importFrom utils packageVersion
databricks_user_agent <- function() {
  user_agent <- paste0("r-odbc/", packageVersion("odbc"))
  if (nchar(Sys.getenv("SPARK_CONNECT_USER_AGENT")) != 0) {
    # Respect the existing user-agent if present. Normally we'd append, but the
    # Databricks ODBC driver does not yet support space-separated entries in
    # this field.
    user_agent <- Sys.getenv("SPARK_CONNECT_USER_AGENT")
  }
  user_agent
}

databricks_auth_args <- function(host, uid = NULL, pwd = NULL) {
  # Detect viewer-based credentials from Posit Connect.
  workspace <- paste0("https://", host)
  if (is_installed("connectcreds") && connectcreds::has_viewer_token(workspace)) {
    token <- connectcreds::connect_viewer_token(workspace)
    return(list(
      authMech = 11,
      auth_flow = 0,
      auth_accesstoken = token$access_token
    ))
  }

  if (!is.null(uid) && !is.null(pwd)) {
    return(list(uid = uid, pwd = pwd, authMech = 3))
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
  if (grepl("posit-workbench", cfg_file, fixed = TRUE)) {
    wb_token <- workbench_databricks_token(host, cfg_file)
  }

  if (nchar(token) != 0) {
    # An explicit PAT takes precedence over everything else.
    list(
      authMech = 3,
      uid = "token",
      pwd = token
    )
  } else if (nchar(client_id) != 0) {
    # Next up are explicit OAuth2 M2M credentials.
    list(
      authMech = 11,
      auth_flow = 1,
      auth_client_id = client_id,
      auth_client_secret = client_secret
    )
  } else if (!is.null(wb_token)) {
    # Next up are Workbench-provided credentials.
    list(
      authMech = 11,
      auth_flow = 0,
      auth_accesstoken = wb_token
    )
  } else if (!is_hosted_session() && nchar(Sys.which(cli_path)) != 0) {
    # When on desktop, try using the Databricks CLI for auth.
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
      token <- gsub(".*access_token\":\\s?\"([^\"]+).*", "\\1", output)
      list(
        authMech = 11,
        auth_flow = 0,
        auth_accesstoken = token
      )
    }
  }
}

# Try to determine whether we can redirect the user's browser to a server on
# localhost, which isn't possible if we are running on a hosted platform.
#
# This is based on the strategy pioneered by the {gargle} package and {httr2}.
is_hosted_session <- function() {
  # If RStudio Server or Posit Workbench is running locally (which is possible,
  # though unusual), it's not acting as a hosted environment.
  Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" &&
    !grepl("localhost", Sys.getenv("RSTUDIO_HTTP_REFERER"), fixed = TRUE)
}


is_camel_case <- function(x) {
  grepl("^[a-z]+([A-Z_][a-z]+)*$", x)
}

dot_names <- function(...) names(substitute(...()))

# Reads Posit Workbench-managed Databricks credentials from a
# $DATABRICKS_CONFIG_FILE. The generated file will look as follows:
#
# [workbench]
# host = some-host
# token = some-token
workbench_databricks_token <- function(host, cfg_file) {
  cfg <- readLines(cfg_file)
  # We don't attempt a full parse of the INI syntax supported by Databricks
  # config files, instead relying on the fact that this particular file will
  # always contain only one section.
  if (!any(grepl(host, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this host.
    return(NULL)
  }
  line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[line])
  if (nchar(token) == 0) {
    return(NULL)
  }
  token
}

# p. 44 https://downloads.datastax.com/odbc/2.6.5.1005/Simba%20Spark%20ODBC%20Install%20and%20Configuration%20Guide.pdf
spark_simba_config <- function(driver) {
  spark_env <- Sys.getenv("SIMBASPARKINI")
  URL <- "https://www.databricks.com/spark/odbc-drivers-download"
  if (!identical(spark_env, "")) {
    return(list(path = spark_env, url = URL))
  }
  common_dirs <- c(
    driver_dir(driver),
    "/Library/simba/spark/lib",
    "/etc",
    getwd(),
    Sys.getenv("HOME")
  )
  return(list(
    path = list.files(
      common_dirs,
      pattern = "simba\\.sparkodbc\\.ini$",
      full.names = TRUE),
    url = URL
  ))
}
