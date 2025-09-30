# Real-time-traffic-congestion-analysis

# Traffic Data Analysis Project

## Project title

Traffic Data Analysis — Exploratory analysis, SQL queries, and visualizations to identify congestion patterns, busiest locations and peak hours.

---

## Project overview (one-line)

Analyze sensor-based traffic observations to understand temporal patterns (hourly/daily), location hotspots, congestion levels, and the effect of weather/events across the month of September 2025.

---

## Files included in this project

* `Traffic_Analysis.ipynb` — Jupyter notebook containing the full step-by-step analysis, code, and visualizations.
* `traffic_data1.csv` — Raw dataset used for analysis (sensor/time-series records).
* `traffic_data1.sql` — Collection of SQL statements and analytical queries used to run similar analysis in a relational database (MySQL/MariaDB).

---

## Quick dataset summary (auto-computed)

* **Rows (observations):** 4,320
* **Date range:** 2025-09-01 to 2025-09-30
* **Unique locations / sensors:** 6
* **Top locations (by total vehicle_count):**

| Rank | Location          | Total vehicle_count |
| ---: | ----------------- | ------------------: |
|    1 | HSR Layout        |             146,297 |
|    2 | Indiranagar       |             142,021 |
|    3 | Koramangala       |             134,083 |
|    4 | MG Road           |             117,182 |
|    5 | Whitefield        |             115,946 |
|    6 | Bannerghatta Road |              78,891 |

* **Busiest hour (by average vehicle_count):** 8 AM (hour_of_day = 8) — average ~214 vehicles.
* **Average congestion_index:** ~8.26 (dataset mean)
* **Missing values:** Key columns like `timestamp`, `vehicle_count`, `congestion_index` contain 0 missing values in this dataset.

---

## Data dictionary (column descriptions)

> Below are short, presentation-ready definitions for each column present in `traffic_data1.csv`. Use these when describing the dataset.

* `timestamp` — timestamp string when the observation was recorded (e.g. `2025-09-01 08:00:00`).
* `location` — name of the sensor location / road segment (categorical).
* `vehicle_count` — total number of vehicles counted at that time and location.
* `heavy_vehicle_count` — number of heavy vehicles (buses, trucks).
* `light_vehicle_count` — number of light vehicles (cars, two-wheelers).
* `avg_speed` — mean speed (across all vehicles) in km/h.
* `avg_speed_heavy` — mean speed for heavy vehicles in km/h.
* `avg_speed_light` — mean speed for light vehicles in km/h.
* `speed_variance` — variance in vehicle speeds (measure of spread).
* `traffic_density` — an estimate of vehicles per unit road length (dataset-specific formula).
* `congestion_index` — numeric index indicating congestion severity (higher means worse congestion).
* `congestion_level` — categorical label derived from `congestion_index` (e.g. `Low`, `Medium`, `High`).
* `travel_time_minutes` — estimated time (in minutes) to traverse the monitored road segment.
* `weather` — short label for weather condition (e.g. `Clear`, `Rain`).
* `rainfall_mm` — rainfall in millimeters during the observation interval.
* `event` — binary/flag indicator if a special/event/incident was recorded (0/1 or similar).
* `day_of_week` — integer representing day of week (0 = Monday or dataset-specific mapping).
* `is_weekend` — binary flag (0 or 1) indicating weekend.
* `hour_of_day` — hour of day (0–23) — already present in the dataset as a derived column.

> *Note:* The above descriptions match the columns in the uploaded dataset. If you change column names, update the dictionary before presenting.

---

## Analysis objectives (what we aimed to answer)

1. Understand overall traffic volumes and how they vary by time (hour, day) and location.
2. Identify busiest locations and hours — where and when congestion appears most frequently.
3. Analyze the relationship between speed, vehicle mix (heavy vs light), and congestion.
4. Check impact of weather or events on congestion.
5. Provide actionable recommendations for traffic management based on observed patterns.

---

## Step-by-step methodology (detailed; use for presentation)

Below is a reproducible walkthrough you can read aloud while presenting — each step explains what was done and why.

### 1. Environment & files

* Tools used: Python (Pandas, Matplotlib), Jupyter Notebook, and MySQL (for .sql queries).
* Place the files in a single project folder. Open `Traffic_Analysis.ipynb` to run the analysis or import `traffic_data1.csv` into your local database.

### 2. Data loading

* Load CSV with Pandas:

```python
import pandas as pd
df = pd.read_csv('traffic_data1.csv')
```

* Inspect shape and columns:

```python
print(df.shape)
print(df.columns)
df.head()
```

* Confirm `timestamp` parsing and convert to `datetime` type for time-based operations:

```python
df['parsed_ts'] = pd.to_datetime(df['timestamp'], errors='coerce', dayfirst=True)
```

### 3. Data cleaning and validation

* Check for missing values and duplicates.
* Ensure numeric columns are coerced to numeric types (`vehicle_count`, `avg_speed`, `congestion_index`).
* Handle outliers: inspect extreme percentiles (e.g. 99th) and decide whether to cap, remove, or keep them depending on domain knowledge.

```python
df['vehicle_count'] = pd.to_numeric(df['vehicle_count'], errors='coerce')
missing = df.isnull().sum()
```

* If `parsed_ts` has `NaT` values (parsing failures), check formats and re-parse with explicit format strings where necessary.

### 4. Feature engineering

* Extract time features useful for analysis:

```python
df['date'] = df['parsed_ts'].dt.date
df['hour'] = df['parsed_ts'].dt.hour
df['day_of_week'] = df['parsed_ts'].dt.dayofweek  # 0=Monday
```

* Create aggregated columns if needed (e.g., `pct_heavy = heavy_vehicle_count / vehicle_count`).

### 5. Exploratory Data Analysis (EDA)

* **Time series:** total vehicle_count per day to see trends.
* **Hourly patterns:** average vehicle_count by hour to show peak commute hours.
* **Location ranking:** total vehicle_count by location (bar chart of top N locations).
* **Speed vs congestion:** scatter plot of `avg_speed` vs `congestion_index` to show negative correlation.
* **Weather / Event impact:** group by `weather` or `event` and compare `congestion_index`.

Example (Pandas + Matplotlib):

```python
daily = df.groupby('date')['vehicle_count'].sum()
daily.plot(title='Daily total vehicle count')

hourly = df.groupby('hour')['vehicle_count'].mean()
hourly.plot(kind='bar', title='Average vehicles by hour')
```

### 6. SQL-based analysis (for database users)

* Import the CSV into a database and run analytic queries. Example queries included in `traffic_data1.sql`.
* Example: daily total vehicles per location

```sql
SELECT DATE(parsed_ts) AS day, location, SUM(vehicle_count) AS total_vehicles
FROM traffic_data1
GROUP BY day, location
ORDER BY day, total_vehicles DESC;
```

* Example (busiest location per day using window function):

```sql
SELECT day, location, total_vehicles FROM (
  SELECT DATE(parsed_ts) AS day,
         location,
         SUM(vehicle_count) AS total_vehicles,
         ROW_NUMBER() OVER (PARTITION BY DATE(parsed_ts) ORDER BY SUM(vehicle_count) DESC) AS rn
  FROM traffic_data1
  GROUP BY DATE(parsed_ts), location
) t
WHERE rn = 1;
```

### 7. Visualizations included

* Time series chart: daily total vehicle_count (line chart).
* Bar chart: top-10 busiest locations by total vehicle_count.
* Hourly heatmap: average vehicle_count by hour (rows) and location (columns) — useful to show commute patterns per location.
* Scatterplots: `avg_speed` vs `congestion_index`, `heavy_vehicle_count` vs `congestion_index`.

### 8. Key findings (what you can present)

* The dataset covers the whole month of September 2025 (2025-09-01 to 2025-09-30) with 4,320 observations.
* **Peak hour:** 8 AM is the busiest hour on average (~214 vehicles).
* **Top congestion locations:** HSR Layout, Indiranagar, Koramangala show consistently high vehicle counts and high congestion indices; they should be first targets for traffic monitoring and mitigation.
* **Weather/events:** average congestion increases slightly during rain or recorded events — use the notebook grouping to show exact numbers during your presentation.

### 9. Recommendations (actionable)

* Short-term: adaptive signal timings during the 7–10 AM and 5–8 PM windows; temporary traffic rerouting during major events.
* Mid-term: expand public transit / last-mile solutions around identified hotspots (HSR Layout, Indiranagar).
* Long-term: install permanent sensors on adjacent corridors, and build a real-time dashboard to trigger alerts when `congestion_index` crosses a threshold.

### 10. Next steps & further analysis ideas

* Build a forecasting model (SARIMA/Prophet/LightGBM) for short-term traffic prediction (15–60 minutes ahead) to support dynamic control.
* Anomaly detection for incidents using rolling z-scores or isolation forest.
* Combine with external data: Google Maps travel times, planned events, roadworks, or POIs to explain anomalies.
* Deploy the notebook as an interactive dashboard using Streamlit, Dash, or R Shiny.

---

## Reproducibility & how to run

1. Create a Python virtual environment and install dependencies (example `requirements.txt`):

```
pandas
matplotlib
jupyter
```

2. From the project folder run Jupyter:

```bash
jupyter notebook Traffic_Analysis.ipynb
```

3. To run SQL queries, import `traffic_data1.csv` into your MySQL database and run `traffic_data1.sql` or execute queries manually.

---

## Appendix — selected code snippets (quick reference)

**Pandas: group and aggregate daily totals**

```python
daily = df.groupby(df['parsed_ts'].dt.date)['vehicle_count'].sum().reset_index()
print(daily.head())
```

**Matplotlib: plot top locations bar chart**

```python
top = df.groupby('location')['vehicle_count'].sum().sort_values(ascending=False).head(10)
ax = top.plot(kind='bar', title='Top 10 locations by vehicle count')
ax.set_ylabel('Total vehicles')
```

**SQL: busiest location per day (window function)**

```sql
SELECT day, location, total_vehicles FROM (
  SELECT DATE(parsed_ts) AS day,
         location,
         SUM(vehicle_count) AS total_vehicles,
         ROW_NUMBER() OVER (PARTITION BY DATE(parsed_ts) ORDER BY SUM(vehicle_count) DESC) AS rn
  FROM traffic_data1
  GROUP BY DATE(parsed_ts), location
) t
WHERE rn = 1;
```

---

## If you present this: short script / flow to read aloud

1. Start with *Project overview*: what the dataset is and the project objective.
2. Show *Quick stats* (rows, date range, top locations, peak hour).
3. Explain *methodology* briefly: loading, cleaning, feature engineering, SQL and Python analyses.
4. Show 2–3 plots: daily time series, top locations bar chart, and hourly heatmap.
5. Present *key findings* and finish with *recommendations* and next steps.





