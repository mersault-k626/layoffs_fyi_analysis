SELECT 
    column_name, 
    data_type 
FROM 
    information_schema.columns
WHERE 
    table_name = 'layoffs'
ORDER BY ordinal_position;

--- establish dim_company

CREATE TABLE dim_company (
    company_id     SERIAL PRIMARY KEY,
    company_name   TEXT UNIQUE
);

INSERT INTO dim_company (company_name)
SELECT DISTINCT company_name
FROM layoffs
WHERE company_name IS NOT NULL;

SELECT * FROM dim_company LIMIT 10;

SELECT COUNT(*)
FROM dim_company

---- establish dim_location

CREATE TABLE dim_location (
    location_id    SERIAL PRIMARY KEY,
    office         TEXT,
    country        TEXT,
    region         TEXT,
    continent      TEXT,
    country_code   TEXT,
    UNIQUE (office, country, region, continent, country_code)
);

INSERT INTO dim_location (office, country, region, continent, country_code)
SELECT DISTINCT office, country, region, continent, country_code
FROM layoffs
WHERE country IS NOT NULL;

SELECT * FROM dim_location LIMIT 10;


---- dim_date

CREATE TABLE dim_date (
    date_id     SERIAL PRIMARY KEY,
    full_date   DATE UNIQUE,
    year        INTEGER,
    quarter     TEXT,
    month       INTEGER,
    day         INTEGER,
    weekday     TEXT,
    week        INTEGER
);

INSERT INTO dim_date (full_date, year, quarter, month, day, weekday, week)
SELECT DISTINCT 
    date_announced                          AS full_date,
    EXTRACT(YEAR FROM date_announced)       AS year,
    'Q' || EXTRACT(QUARTER FROM date_announced) AS quarter,
    EXTRACT(MONTH FROM date_announced)      AS month,
    EXTRACT(DAY FROM date_announced)        AS day,
    TRIM(TO_CHAR(date_announced, 'Day'))    AS weekday,
    EXTRACT(WEEK FROM date_announced)       AS week
FROM layoffs
WHERE date_announced IS NOT NULL;

SELECT *
FROM dim_date
LIMIT 5;

ALTER TABLE dim_date DROP COLUMN date_id;

ALTER TABLE dim_date ADD PRIMARY KEY (full_date);

SELECT conname
FROM pg_constraint
WHERE conrelid = 'fact_layoffs'::regclass
AND contype = 'f';


--- dim_industry
CREATE TABLE dim_industry (
    industry_id SERIAL PRIMARY KEY,
    industry    TEXT UNIQUE
);
INSERT INTO dim_industry (industry)
SELECT DISTINCT industry
FROM layoffs
WHERE industry IS NOT NULL;

SELECT * FROM dim_industry ORDER BY ;


--- dim_stage
CREATE TABLE dim_stage(
stage_id SERIAL PRIMARY KEY,
stage TEXT UNIQUE
);
INSERT INTO dim_stage (stage)
SELECT DISTINCT stage
FROM layoffs
WHERE stage IS NOT NULL;


--- fact_layoffs
DROP TABLE IF EXISTS fact_layoffs;

CREATE TABLE fact_layoffs (
    layoff_id         INTEGER PRIMARY KEY,
    company_id        INTEGER REFERENCES dim_company(company_id),
    company_name      TEXT,
    location_id       INTEGER REFERENCES dim_location(location_id),
    full_date         DATE REFERENCES dim_date(full_date),
    industry_id       INTEGER REFERENCES dim_industry(industry_id),
    stage_id          INTEGER REFERENCES dim_stage(stage_id),
    num_laid_off      DOUBLE PRECISION,
    pct_laid_off      DOUBLE PRECISION,
    funding_usd_mn    DOUBLE PRECISION
);


INSERT INTO fact_layoffs (
    layoff_id,
    company_id,
    company_name,
    location_id,
    full_date,
    industry_id,
    stage_id,
    num_laid_off,
    pct_laid_off,
    funding_usd_mn
)
SELECT
    ROW_NUMBER() OVER (ORDER BY f.date_announced ASC) AS layoff_id,
    c.company_id,
    c.company_name,
    l.location_id,
    d.full_date,
    i.industry_id,
    s.stage_id,
    f.num_laid_off,
    f.pct_laid_off,
    f.mn_raised_dollar
FROM layoffs f
JOIN dim_company  c ON f.company_name = c.company_name
JOIN dim_location l ON f.office = l.office AND f.country = l.country
JOIN dim_date     d ON f.date_announced = d.full_date
JOIN dim_industry i ON f.industry = i.industry
JOIN dim_stage    s ON f.stage = s.stage;

SELECT layoff_id, full_date, company_name
FROM fact_layoffs
ORDER BY layoff_id
LIMIT 10;

--- full_date rename

ALTER TABLE dim_date
RENAME COLUMN full_date TO date_announced;

ALTER TABLE fact_layoffs
RENAME COLUMN full_date TO date_announced;

ALTER TABLE fact_layoffs
ADD CONSTRAINT fk_fact_dim_date
FOREIGN KEY (date_announced)
REFERENCES dim_date(date_announced);

SELECT *
FROM fact_layoffs
WHERE company_name = 'Intel'

--- staging for export to R

CREATE OR REPLACE VIEW layoffs_export_view AS
SELECT
    c.company_name,
    s.stage,
    f.funding_usd_mn,
    l.office,
    l.country,
    l.region,
    l.continent,
    d.date_announced,
    d.year,
    d.quarter,
    d.month,
    d.weekday,
    d.week,
    i.industry,
    f.num_laid_off,
    f.pct_laid_off
FROM fact_layoffs f
JOIN dim_company  c ON f.company_id = c.company_id
JOIN dim_location l ON f.location_id = l.location_id
JOIN dim_date     d ON f.date_announced = d.date_announced
JOIN dim_industry i ON f.industry_id = i.industry_id
JOIN dim_stage    s ON f.stage_id = s.stage_id;




SELECT COUNT(*)
FROM fact_layoffs


SELECT COUNT(*)
FROM layoffs

---
SELECT f.industry, COUNT(*)
FROM layoffs f
LEFT JOIN dim_industry i ON f.industry = i.industry
WHERE i.industry IS NULL
GROUP BY f.industry;

SELECT f.stage, COUNT(*)
FROM layoffs f
LEFT JOIN dim_stage s ON f.stage = s.stage
WHERE s.stage IS NULL
GROUP BY f.stage;

INSERT INTO dim_stage (stage)
VALUES ('Unknown')
ON CONFLICT DO NOTHING;

UPDATE layoffs
SET stage = 'Unknown'
WHERE stage IS NULL;


SELECT f.stage, COUNT(*)
FROM layoffs f
LEFT JOIN dim_stage s ON f.stage = s.stage
WHERE s.stage IS NULL
GROUP BY f.stage;

--- getting the missing rows :(
DROP TABLE IF EXISTS fact_layoffs CASCADE;

CREATE TABLE fact_layoffs (
    layoff_id         INTEGER PRIMARY KEY,
    company_id        INTEGER REFERENCES dim_company(company_id),
    company_name      TEXT,
    location_id       INTEGER REFERENCES dim_location(location_id),
    date_announced    DATE REFERENCES dim_date(date_announced),
    industry_id       INTEGER REFERENCES dim_industry(industry_id),
    stage_id          INTEGER REFERENCES dim_stage(stage_id),
    num_laid_off      DOUBLE PRECISION,
    pct_laid_off      DOUBLE PRECISION,
    funding_usd_mn    DOUBLE PRECISION
);
INSERT INTO fact_layoffs (
    layoff_id,
    company_id,
    company_name,
    location_id,
    date_announced,
    industry_id,
    stage_id,
    num_laid_off,
    pct_laid_off,
    funding_usd_mn
)
SELECT
    ROW_NUMBER() OVER (ORDER BY f.date_announced ASC) AS layoff_id,
    c.company_id,
    c.company_name,
    l.location_id,
    f.date_announced,
    i.industry_id,
    s.stage_id,
    f.num_laid_off,
    f.pct_laid_off,
    f.mn_raised_dollar
FROM layoffs f
JOIN dim_company  c ON f.company_name = c.company_name
JOIN dim_location l ON f.office = l.office AND f.country = l.country
JOIN dim_date     d ON f.date_announced = d.date_announced
JOIN dim_industry i ON f.industry = i.industry
JOIN dim_stage    s ON f.stage = s.stage;
SELECT COUNT(*) FROM fact_layoffs;
-- Should return 4,095

--- RECREATE VIEW
DROP VIEW IF EXISTS layoffs_export_view;

CREATE OR REPLACE VIEW layoffs_export_view AS
SELECT
    c.company_name,
    s.stage,
    f.funding_usd_mn,
    l.office,
    l.country,
    l.region,
    l.continent,
    f.date_announced,
    d.year,
    d.quarter,
    d.month,
    d.weekday,
    d.week,
    i.industry,
    f.num_laid_off,
    f.pct_laid_off
FROM fact_layoffs f
JOIN dim_company  c ON f.company_id = c.company_id
JOIN dim_location l ON f.location_id = l.location_id
JOIN dim_date     d ON f.date_announced = d.date_announced
JOIN dim_industry i ON f.industry_id = i.industry_id
JOIN dim_stage    s ON f.stage_id = s.stage_id;

SELECT *
FROM layoffs_export_view
































