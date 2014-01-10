### Analysing Phenotype MicroArray data: database I/O with PostgreSQL

# This is example R code for using opm to store PM data in a PostgreSQL database
# and retrieving them again.
#
# This code can be used to check whether a database either found in an
# environment variable or identical to the default value (see below) is
# correctly set up for this purpose. The code also shows how to include a
# user-defined selection of metadata.
#
# The database must be accessible with 'localhost' as server, the current user
# as user and without a password.
#
# Author: Markus Goeker


library(opm)
library(RPostgreSQL)

conn <- dbConnect("PostgreSQL",
  dbname = Sys.getenv("OPM_POSTGRESQL_DB", "pmdata"))

# check without metadata
result <- opm_dbcheck(conn)

print(opm_dbnext(2L, conn))

if (all(result == "ok")) {

  # addition of metadata columns
  dbGetQuery(conn,
    "ALTER TABLE plates ADD COLUMN strain text, ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md)

  # removal of metadata columns
  dbGetQuery(conn,
    "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}

dbDisconnect(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")

