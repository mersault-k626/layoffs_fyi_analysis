--- review imported table from R

SELECT 
    column_name, 
    data_type 
FROM 
    information_schema.columns
WHERE 
    table_name = 'layoffs'
ORDER BY ordinal_position;

--- alter dbl to numeric

ALTER TABLE layoffs
ALTER COLUMN pct_laid_off TYPE numeric,
ALTER COLUMN mn_raised_dollar TYPE numeric;

--- review changes

SELECT 
    column_name, 
    data_type 
FROM 
    information_schema.columns
WHERE 
    table_name = 'layoffs'
ORDER BY ordinal_position;

--- Normalisation 1. dim_company

CREATE TABLE dim_company (
    company_name TEXT UNIQUE
);

	--- insert company alphabetically

INSERT INTO dim_company(company_name)
SELECT DISTINCT company_name
FROM layoffs
WHERE company_name IS NOT NULL
ORDER BY company_name;

	--- insert ID

ALTER TABLE dim_company ADD COLUMN company_id INTEGER;

WITH alphabetical_rank AS(
	SELECT
		company_name,
		ROW_NUMBER() OVER (ORDER BY company_name) AS row_num
	FROM dim_company)
	
UPDATE dim_company AS dc
SET company_id = ar.row_num
FROM alphabetical_rank as ar
WHERE dc.company_name = ar.company_name;

ALTER TABLE dim_company
ADD CONSTRAINT dim_company_pkey PRIMARY KEY (company_id);

SELECT *
FROM dim_company
LIMIT 5;

 --- check type

 SELECT 
    column_name, 
    data_type 
FROM 
    information_schema.columns
WHERE 
    table_name = 'dim_company'
ORDER BY ordinal_position;

--- Normalisation 2. dim_location

CREATE TABLE dim_location(
	location_id SERIAL PRIMARY KEY,
	office TEXT,
	country TEXT,
	region TEXT,
	continent TEXT,
	country_code TEXT,
	UNIQUE (office, country, region, continent, country_code)
);


INSERT INTO dim_location(
	office,
	country,
	region,
	continent,
	country_code
)
SELECT DISTINCT 
	office, 
	country, 
	region, 
	continent, 
	country_code
FROM layoffs
WHERE country IS NOT NULL;


--- Normalisation 3. dim_date

CREATE TABLE dim_date (
    date_announced DATE UNIQUE PRIMARY KEY,
    year INTEGER,
    quarter TEXT,
    month INTEGER,
	week INTEGER,
    day INTEGER,
    weekday TEXT
);

INSERT INTO 
	dim_date (date_announced, year, quarter, month, week, day, weekday)
SELECT DISTINCT 
    date_announced,
    EXTRACT(YEAR FROM date_announced) AS year,
    'Q' || EXTRACT(QUARTER FROM date_announced) AS quarter,
    EXTRACT(MONTH FROM date_announced) AS month,
	EXTRACT(WEEK FROM date_announced) AS week,
    EXTRACT(DAY FROM date_announced) AS day,
    TRIM(TO_CHAR(date_announced, 'Day')) AS weekday
FROM layoffs
WHERE date_announced IS NOT NULL;


--- Normalisation 4. dim_industry

CREATE TABLE 
	dim_industry (
    industry_id SERIAL PRIMARY KEY,
    industry  TEXT UNIQUE
);

INSERT INTO dim_industry (industry)
SELECT DISTINCT industry
FROM layoffs
WHERE industry IS NOT NULL;

--- Normalisation 5. dim_stage

CREATE TABLE dim_stage(
	stage_id SERIAL PRIMARY KEY,
	stage TEXT UNIQUE
);

INSERT INTO dim_stage (stage)
SELECT DISTINCT stage
FROM layoffs
WHERE stage IS NOT NULL;


--- Normalisation 6. fact_layoffs

CREATE TABLE fact_layoffs(
	layoff_id INTEGER PRIMARY KEY,
	company_id INTEGER REFERENCES dim_company(company_id),
	company_name TEXT,
	num_laid_off INTEGER,
	pct_laid_off NUMERIC,
	date_announced DATE REFERENCES dim_date(date_announced),
	office TEXT,
	country_code TEXT,
	location_id INTEGER REFERENCES dim_location(location_id),
	mn_raised_dollar INTEGER,
	stage_id integer REFERENCES dim_stage(stage_id),
	industry_id INTEGER REFERENCES dim_industry(industry_id),
	source TEXT
);

INSERT INTO fact_layoffs(
	layoff_id,
	company_id,
	company_name,
	num_laid_off,
	pct_laid_off,
	date_announced,
	office,
	country_code,
	location_id,
	mn_raised_dollar,
	stage_id,
	industry_id,
	source
)

SELECT
	ROW_NUMBER() OVER (ORDER BY f.date_announced ASC) AS layoff_id,
	c.company_id,
	c.company_name,
	f.num_laid_off,
	f.pct_laid_off,
	d.date_announced,
	l.office,
	l.country_code,
	l.location_id,
	f.mn_raised_dollar,
	s.stage_id,
	i.industry_id,
	f.source

FROM layoffs AS f
JOIN dim_company AS c
	USING(company_name)
JOIN dim_location AS l
	USING(office, country, country_code)
JOIN dim_date AS d
	USING(date_announced)
JOIN dim_industry AS i
	USING(industry)
JOIN dim_stage AS s
	USING(stage);

--- Review fact_layoffs

SELECT *
FROM fact_layoffs
ORDER BY date_announced DESC
LIMIT 5;



SELECT 
    column_name, 
    data_type 
FROM 
    information_schema.columns
WHERE 
    table_name = 'fact_layoffs'
ORDER BY ordinal_position;


--- Create view for R

CREATE OR REPLACE VIEW layoffs_export_view AS
SELECT 
	c.company_id,
	c.company_name,
	f.num_laid_off,
	f.pct_laid_off,
	i.industry,
	l.office,
	l.country_code,
	l.country,
	l.region,
	l.continent,
	d.date_announced,
    d.year,
    d.quarter,
    d.month,
    d.weekday,
    d.week,
	s.stage,
	f.mn_raised_dollar,
	f.source
FROM fact_layoffs AS f
JOIN dim_company AS c 
	ON f.company_id = c.company_id
JOIN dim_location AS l 
	ON f.location_id = l.location_id
JOIN dim_date AS d 
	ON f.date_announced = d.date_announced
JOIN dim_industry AS i 
	ON f.industry_id = i.industry_id
JOIN dim_stage AS s 
	ON f.stage_id = s.stage_id;

	


	

--- Review view
SELECT COUNT(*)
	FROM layoffs_export_view;

SELECT 
    column_name, 
    data_type 
FROM 
    information_schema.columns
WHERE 
    table_name = 'layoffs_export_view'
ORDER BY 
    ordinal_position;


SELECT *
FROM layoffs
LIMIT 7;


SELECT *
FROM dim_location;

SELECT
	dc.company_name AS company_name,
	SUM(f.num_laid_off) AS num_laid_off,
	ROUND(AVG(f.pct_laid_off), 2) AS pct_laid_off,
	l.office AS office
FROM fact_layoffs AS f
JOIN dim_company AS dc
	USING(company_id)
JOIN dim_location AS l
	USING(location_id)
WHERE l.country = 'United States'
GROUP BY dc.company_name, l.office
ORDER BY num_laid_off DESC NULLS LAST;

SELECT *
FROM dim_location
ORDER BY country;






