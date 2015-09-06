####
source('~/Research/WardProjects/tensor/R/setup.R')
####

####
mysqlSetup('shahryarm', 'xxxxx', 'event_data')
on.exit(dbDisconnect(conn))
####

###
# Clear tables
dbSendQuery(conn, "DROP TABLE my_tables.tensorVerbCoop")
dbSendQuery(conn, "DROP TABLE my_tables.tensorMatlCoop")
dbSendQuery(conn, "DROP TABLE my_tables.tensorVerbConf")
dbSendQuery(conn, "DROP TABLE my_tables.tensorMatlConf")
###

###
# Create tables
dbSendQuery(conn, 
	"CREATE TABLE my_tables.tensorVerbCoop AS
	SELECT YEAR(e.event_date) AS year
	  , MONTH(e.event_date) AS month
	  , cSource.ISOA3Code AS source_country
	  , cTarget.ISOA3Code AS target_country
	  , COUNT(*) AS verb_coop
	FROM simple_events e
	  JOIN eventtypes t ON e.eventtype_id = t.eventtype_ID
	  JOIN countries cSource ON e.source_country_id = cSource.id
	  JOIN countries cTarget ON e.target_country_id = cTarget.id
	WHERE SUBSTRING(t.code, 1, 2) IN ('01', '02', '03', '04', '05')
	GROUP BY YEAR(e.event_date), MONTH(e.event_date), e.source_country_id, e.target_country_id"
	)

dbSendQuery(conn, 
	"CREATE TABLE my_tables.tensorMatlCoop AS
	SELECT YEAR(e.event_date) AS year
		  , MONTH(e.event_date) AS month
		  , cSource.ISOA3Code AS source_country
		  , cTarget.ISOA3Code AS target_country
		  , COUNT(*) AS matl_coop
		FROM simple_events e
		  JOIN eventtypes t ON e.eventtype_id = t.eventtype_ID
		  JOIN countries cSource ON e.source_country_id = cSource.id
		  JOIN countries cTarget ON e.target_country_id = cTarget.id
		WHERE SUBSTRING(t.code, 1, 2) IN ('06', '07', '08')
		GROUP BY YEAR(e.event_date), MONTH(e.event_date), e.source_country_id, e.target_country_id"	
	)

dbSendQuery(conn, 
	"CREATE TABLE my_tables.tensorVerbConf AS
	SELECT YEAR(e.event_date) AS year
		  , MONTH(e.event_date) AS month
		  , cSource.ISOA3Code AS source_country
		  , cTarget.ISOA3Code AS target_country
		  , COUNT(*) AS verb_conf
		FROM simple_events e
		  JOIN eventtypes t ON e.eventtype_id = t.eventtype_ID
		  JOIN countries cSource ON e.source_country_id = cSource.id
		  JOIN countries cTarget ON e.target_country_id = cTarget.id
		WHERE SUBSTRING(t.code, 1, 2) IN ('09', '10', '11', '12', '13')
		GROUP BY YEAR(e.event_date), MONTH(e.event_date), e.source_country_id, e.target_country_id"	
	)

dbSendQuery(conn, 
	"CREATE TABLE my_tables.tensorMatlConf AS
	SELECT YEAR(e.event_date) AS year
	  , MONTH(e.event_date) AS month
	  , cSource.ISOA3Code AS source_country
	  , cTarget.ISOA3Code AS target_country
	  , COUNT(*) AS matl_conf
	FROM simple_events e
	  JOIN eventtypes t ON e.eventtype_id = t.eventtype_ID
	  JOIN countries cSource ON e.source_country_id = cSource.id
	  JOIN countries cTarget ON e.target_country_id = cTarget.id
	WHERE SUBSTRING(t.code, 1, 2) IN ('14', '15', '16', '17', '18', '19', '20')
	GROUP BY YEAR(e.event_date), MONTH(e.event_date), e.source_country_id, e.target_country_id"	
	)
###