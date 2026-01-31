# lakebase host validates inputs

    Code
      lakebase_host("instance-id", "")
    Condition
      Error in `DBI::dbConnect()`:
      ! No Databricks workspace URL provided.
      i Either supply `workspace` argument or set env var `DATABRICKS_HOST`.

# errors if can't find driver

    Code
      lakebase_default_driver()
    Condition
      Error in `DBI::dbConnect()`:
      ! Failed to automatically find the PostgreSQL ODBC driver.
      i Set `driver` to known driver name or path.

# errors if auth fails

    Code
      . <- lakebase_args1()
    Condition
      Error in `DBI::dbConnect()`:
      ! Failed to detect ambient Databricks credentials.
      i Supply `uid` and `pwd` to authenticate manually.

# must supply both uid and pwd

    Code
      lakebase_auth_args("workspace", uid = "uid")
    Condition
      Error in `DBI::dbConnect()`:
      ! Both `uid` and `pwd` must be specified for manual authentication.
      i Or leave both unset for automated authentication.

# we hint viewer-based credentials on Connect

    Code
      lakebase_args(instance_id = "instance-id", workspace = "workspace", driver = "driver")
    Condition
      Error in `DBI::dbConnect()`:
      ! Failed to detect ambient Databricks credentials.
      i Supply `uid` and `pwd` to authenticate manually.
      i Or consider enabling Posit Connect's Databricks integration for viewer-based credentials. See <https://docs.posit.co/connect/user/oauth-integrations/#adding-oauth-integrations-to-deployed-content> for details.

