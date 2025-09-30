CREATE DATABASE traffic_DB

USE traffic_DB




SELECT * FROM [traffic_data1];

-- 1. Count total records
SELECT COUNT(*) AS total_records FROM traffic_data1;

-- 2. Count unique locations
SELECT COUNT(DISTINCT location) AS total_locations FROM traffic_data1;

-- 3. Total vehicle count by location
SELECT location, SUM(vehicle_count) AS total_vehicles
FROM traffic_data1
GROUP BY location;

-- 4. Top 5 hours with highest traffic
SELECT hour_of_day, SUM(vehicle_count) AS total_vehicles
FROM traffic_data1
GROUP BY hour_of_day
ORDER BY total_vehicles DESC
LIMIT 5;

-- 5. Frequency of each congestion level
SELECT congestion_level, COUNT(*) AS occurrences
FROM traffic_data1
GROUP BY congestion_level;

-- 6. Average congestion index by weather condition
SELECT weather, AVG(congestion_index) AS avg_congestion
FROM traffic_data1
GROUP BY weather;

-- 7. Compare traffic on weekdays vs weekends
SELECT is_weekend, AVG(vehicle_count) AS avg_vehicles
FROM traffic_data1
GROUP BY is_weekend;

-- 8. Average travel time by location
SELECT location, AVG(travel_time_minutes) AS avg_travel_time
FROM traffic_data1
GROUP BY location
ORDER BY avg_travel_time DESC;

-- 9. Congestion based on rainfall categories
SELECT 
    CASE 
        WHEN rainfall_mm = 0 THEN 'No Rain'
        WHEN rainfall_mm BETWEEN 0.1 AND 5 THEN 'Light Rain'
        WHEN rainfall_mm BETWEEN 5.1 AND 20 THEN 'Moderate Rain'
        ELSE 'Heavy Rain'
    END AS rain_category,
    AVG(congestion_index) AS avg_congestion
FROM traffic_data1
GROUP BY rain_category;

-- 10. Top 5 timestamps with highest congestion
SELECT timestamp, location, congestion_index
FROM traffic_data1
ORDER BY congestion_index DESC LIMIT 5;



-- 11. Daily vehicle count (time series aggregation)
SELECT 
    CAST(timestamp AS DATE) AS day,
    SUM(vehicle_count) AS total_vehicles
FROM traffic_data1
GROUP BY  CAST(timestamp AS DATE)
ORDER BY day;


-- 12. Busiest location per day (using window function)
-- 12. Busiest location per day (using window function)
SELECT day, location, total_vehicles
FROM (
    SELECT 
        CAST(timestamp AS DATE) AS day,
        location,
        SUM(vehicle_count) AS total_vehicles,
        RANK() OVER (
            PARTITION BY CAST(timestamp AS DATE)
            ORDER BY SUM(vehicle_count) DESC
        ) AS rnk
    FROM traffic_data1
    GROUP BY CAST(timestamp AS DATE), location
) t
WHERE rnk = 1;


-- 13. Hourly average speed vs congestion (using CTE)
WITH hourly_stats AS (
    SELECT hour_of_day, 
           AVG(avg_speed) AS avg_speed,
           AVG(congestion_index) AS avg_congestion
    FROM traffic_data1
    GROUP BY hour_of_day
)
SELECT * FROM hourly_stats ORDER BY hour_of_day;

-- 14. Impact of events on traffic
SELECT event, 
       AVG(vehicle_count) AS avg_vehicles, 
       AVG(congestion_index) AS avg_congestion
FROM traffic_data1
GROUP BY event;

-- 15. Heavy vs light vehicle ratio by location
SELECT location,
       SUM(heavy_vehicle_count) / SUM(light_vehicle_count) AS hv_lv_ratio
FROM traffic_data1
GROUP BY location
ORDER BY hv_lv_ratio DESC;
