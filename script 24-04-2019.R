library(GADMTools)
library(sf)
library(tidyverse)
library(rmapshaper)

#import data shp 
indonesia_sf <- st_read("SHP_Indonesia_desa/Indo_Desa_region.shp")
prov_indonesia_sf <- st_read("SHP_Indonesia_provinsi/INDONESIA_PROP.shp")
kec_indonesia_sf <- st_read("SHP_Indonesia_kecamatan/INDONESIA_KEC.shp")
indonesia_wrapper <- gadm_sf.loadCountries("IDN", level = 4, basefile = "./")

#look summary about spatial data
class(indonesia_sf)
head(indonesia_sf, 3)
glimpse(indonesia_sf)

# convert to simplified geometry
simp_indonesia_geometry <- ms_simplify(indonesia_sf, keep = 0.01, keep_shapes = TRUE)
simp_prov_indonesia_geometry <- ms_simplify(prov_indonesia_sf, keep = 0.01, keep_shapes = TRUE)
simp_diy_geometry <- ms_simplify(diy_sf, keep = 0.01, keep_shapes = TRUE)

# convert to geometry
indonesia_geometry <- st_geometry(indonesia_sf)
prov_indonesia_geometry <- st_geometry(prov_indonesia_sf)
kec_indonesia_geometry <- st_geometry(kec_indonesia_sf)
diy_geometry <- st_geometry(diy_sf)

#plot into graphical map
plot(indonesia_geometry)
plot(prov_indonesia_geometry)
plot(simp_indonesia_geometry)
plot(simp_prov_indonesia_geometry)
plot(kec_indonesia_geometry)
plot(diy_sf['KEMATIAN_B'])
 
#check file size
library(pryr)
object.size(prov_indonesia_geometry)
object.size(simp_prov_indonesia_geometry)

