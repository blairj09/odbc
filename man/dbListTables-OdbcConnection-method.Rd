% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbi-connection.R
\name{dbListTables,OdbcConnection-method}
\alias{dbListTables,OdbcConnection-method}
\title{List remote tables and fields for an ODBC connection}
\usage{
\S4method{dbListTables}{OdbcConnection}(
  conn,
  catalog_name = NULL,
  schema_name = NULL,
  table_name = NULL,
  table_type = NULL,
  ...
)
}
\arguments{
\item{conn}{A \link[DBI:DBIConnection-class]{DBI::DBIConnection} object,
as returned by \code{\link[DBI:dbConnect]{dbConnect()}}.}

\item{catalog_name, schema_name, table_name}{Catalog, schema, and table names.

By default, \code{catalog_name}, \code{schema_name} and \code{table_name} will
automatically escape underscores to ensure that you match exactly one
table. If you want to search for multiple tables using wild cards, you
will need to use \code{odbcConnectionTables()} directly instead.}

\item{table_type}{The type of the table to return, the default returns
all table types.}

\item{...}{Other parameters passed on to methods.}
}
\value{
A character vector of table or field names respectively.
}
\description{
\code{dbListTables()} provides names of remote tables accessible through this
connection; \code{dbListFields()} provides names of columns within a table.
}
