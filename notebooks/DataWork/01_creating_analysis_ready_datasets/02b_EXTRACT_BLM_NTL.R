# ================================================================
# Script Name: 02b_EXTRACT_BLM_NTL.R
# Purpose: Extract NASA Black Marble NTL data from 2012 to 2022
# Input Dataset: "grid_10km.shp"
# Output Dataset: VNP46A4_annual_rasters
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# ================================================================



##Using a package called blackmarbler created by Rob Marty : https://github.com/worldbank/blackmarbler
##Note: Follow the instructions in the Github to generate token
bearer <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImNoaXRyYWIzIiwiZXhwIjoxNzQzNzM1NjgxLCJpYXQiOjE3Mzg1NTE2ODEsImlzcyI6Imh0dHBzOi8vdXJzLmVhcnRoZGF0YS5uYXNhLmdvdiIsImlkZW50aXR5X3Byb3ZpZGVyIjoiZWRsX29wcyIsImFjciI6ImVkbCIsImFzc3VyYW5jZV9sZXZlbCI6M30.7ZLJPkCaf568L0u1pmcY0arki4hyzqAYsddWjn2xvpvYp1EXQS580jthkzX2cdGFFBJXZJ6N4eTy7UbeimidRNjQ7Z-lMOumVZxAmVMdtzsm3NbMxKkT1yMiN_1bBCE34sFq9xHKIKhzyhCPwEE8Ro5Dk7KCdESg2Tpv5dpgthITKbmMeoHNuuyclS2m8aE2terpnZlRXGSbSMKI6gkM4W6I4eKnAqbc3bQEPoYiIs8YYophP6zEj0siFQA9Yfwd9sYkVNgJuNuaaQz3Zcts3u6syzksuPGJaeyzbyv89iAJ2GYcd1ylwRbITUdk_K3bc4y35JZCoO-tJcgK2_Nt9A"



# Load Data ---------------------------------------------------------------
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))






# Downloading Black Marble Data -------------------------------------------
#transform the grid shapefile
grid_sf_prj <- st_transform(grid_sf, 4326)


ntl_df <- bm_raster(roi_sf = grid_sf_prj,
                    product_id = "VNP46A4",
                    date = 2012:2022,
                    bearer = bearer,
                    output_location_type = "file",
                    file_dir = file.path(raw_replication,"NTL", "raw", "VNP46A4_annual_rasters"))



