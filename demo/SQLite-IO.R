### Analysing Phenotype MicroArray data: database I/O with SQLite

# This is example R code for using opm to store PM data in an SQLite database
# and retrieving them again.
#
# This code can be used to check whether a database either found in an
# environment variable or identical to the default value (see below) is
# correctly set up for this purpose. The code also shows how to include a
# user-defined selection of metadata.
#
# The database file must be accessible with the current user as user and without
# a password.
#
# Author: Markus Goeker


library(opm)
library(RSQLite)


conn <- dbConnect("SQLite",
  dbname = Sys.getenv("OPM_SQLITE_DB", file.path("misc", "pmdata.db")))

result <- opm_dbcheck(conn)

print(opm_dbnext(2L, conn))

if (all(result == "ok")) {

  # addition of metadata columns
  dbGetQuery(conn, "ALTER TABLE plates ADD COLUMN strain text;")
  dbGetQuery(conn, "ALTER TABLE plates ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md)

  # removal of metadata columns (impossible with SQLite 3.7.9)
  #dbGetQuery(conn,
  #  "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}

dbDisconnect(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")

