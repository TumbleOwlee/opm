

-- -----------------------------------------------------------------------------
--
-- Code for removing the tables that hold PM data in the correct order. Tested
-- with PostgreSQL (9.1, 9.3.8), SQLite (3.7.9, 3.8.2) and MySQL (5.5.32,
-- 5.5.43).
--
-- -----------------------------------------------------------------------------

BEGIN;

DROP TABLE IF EXISTS discretized;
DROP TABLE IF EXISTS disc_settings;

DROP TABLE IF EXISTS aggregated;
DROP TABLE IF EXISTS aggr_settings;

DROP TABLE IF EXISTS measurements;

DROP TABLE IF EXISTS wells;
DROP TABLE IF EXISTS plates;

COMMIT;

