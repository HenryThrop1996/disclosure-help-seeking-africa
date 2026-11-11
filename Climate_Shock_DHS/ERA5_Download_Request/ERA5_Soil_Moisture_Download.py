import ee
import time

# 1. Initialize
ee.Authenticate()
ee.Initialize(project='INSERT HERE') 

# 2. Configuration
# Map display names to FAO GAUL dataset ADM0_NAME values
country_mapping = {
    'Benin': 'Benin',
    'Burkina Faso': 'Burkina Faso',
    'Cote dIvoire': "Côte d'Ivoire",
    'Gambia': 'Gambia',
    'Ghana': 'Ghana',
    'Liberia': 'Liberia',
    'Mali': 'Mali',
    'Mauritania': 'Mauritania',
    'Nigeria': 'Nigeria',
    'Senegal': 'Senegal',
    'Sierra Leone': 'Sierra Leone',
    'Togo': 'Togo',
    'Angola': 'Angola',
    'Cameroon': 'Cameroon',
    'Chad': 'Chad',
    'DR Congo': 'Democratic Republic of the Congo',
    'Gabon': 'Gabon',
    'Burundi': 'Burundi',
    'Comoros': 'Comoros',
    'Ethiopia': 'Ethiopia',
    'Kenya': 'Kenya',
    'Madagascar': 'Madagascar',
    'Malawi': 'Malawi',
    'Mozambique': 'Mozambique',
    'Rwanda': 'Rwanda',
    'Tanzania': 'United Republic of Tanzania',
    'Uganda': 'Uganda',
    'Zambia': 'Zambia',
    'Zimbabwe': 'Zimbabwe',
    'Lesotho': 'Lesotho',
    'Namibia': 'Namibia',
    'South Africa': 'South Africa'
}

start_year = 1990
end_year = 2025  

# Access the FAO GAUL dataset for country boundaries
world_boundaries = ee.FeatureCollection("FAO/GAUL/2015/level0")

# 3. Nested Loop: Countries and then Years
for display_name, gaul_name in country_mapping.items():
    print(f"\n--- Initializing Exports for: {display_name} ---")
    
    # Get the bounding box of the country to capture all partial edge grid points
    country_geom = world_boundaries.filter(ee.Filter.eq('ADM0_NAME', gaul_name)) \
        .geometry().bounds()

    for year in range(start_year, end_year + 1):
        start_date = ee.Date.fromYMD(year, 1, 1)
        end_date = start_date.advance(1, 'year')
        
        # Access the daily aggregated ERA5-Land dataset for Soil Water Layers 1 & 2
        # Layer 1: 0-7cm, Layer 2: 7-28cm
        daily_stack = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") \
            .filterDate(start_date, end_date) \
            .select(['volumetric_soil_water_layer_1', 'volumetric_soil_water_layer_2']) \
            .toBands()

        # 4. Launch Export Task
        # Unique folder naming convention for Soil Moisture
        folder_name = f'ERA5_SoilMoisture_{display_name}'
        
        # Unique file naming convention
        task_description = f'{display_name}_SoilMoisture_{year}'

        task = ee.batch.Export.image.toDrive(
            image=daily_stack, 
            description=task_description,
            folder=folder_name,
            scale=11132,       # Native ERA5-Land resolution in meters
            region=country_geom,
            fileFormat='GeoTIFF',
            maxPixels=1e10
        )
        
        task.start()
        print(f"Submitted: {task_description}")
        time.sleep(0.5) # Brief pause to help the task manager keep up

print("\nAll soil moisture tasks 1990-2025 have been queued.")
print("Monitor progress at: https://code.earthengine.google.com/tasks")