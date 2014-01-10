### Analysing Phenotype MicroArray data: database I/O with RODBC

# This is example R code for using opm to store PM data in a database
# accessible via ODBC and retrieving them again.
#
# This code can be used to check whether a database either found in an
# environment variable or identical to the default value (see below) is
# correctly set up for this purpose. The code also shows how to include a
# user-defined selection of metadata.
#
# The ODBC connection must be accordingly defined beforehand to allow for the
# simple-minded connection attempt stated below.
#
# Author: Markus Goeker


library(opm)
library(RODBC)


conn <- odbcConnect(Sys.getenv("OPM_RODBC_DB", "test_opm"))

# Insertions via RODBC in this manner are slow. Subsetting speeds things up.
result <- opm_dbcheck(conn, time.points = 1:5, wells = 12:14)

print(opm_dbnext(2L, conn))

if (all(result == "ok")) {

  # addition of metadata columns
  sqlQuery(conn,
    "ALTER TABLE plates ADD COLUMN strain text, ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md, time.points = 1:5, wells = 12:14)

  # removal of metadata columns
  sqlQuery(conn,
    "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}

odbcClose(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")

