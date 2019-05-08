library(GADMTools)
library(sf)
library(tidyverse)
library(rmapshaper)
library(tmap)
library(formattable) #untuk function percent()

#import data shp 
indonesia_sf <- st_read("SHP_Indonesia_desa/Indo_Desa_region.shp")

#filter to the exact province (in this case Daerah Istimewa Yogyakarta)
diy_sf <- indonesia_sf %>%
  filter(PROPINSI == "DI. Yogya")

#filter only Kota Yogyakarta
kotayk_sf <- indonesia_sf %>%
  filter(KABUPATEN == "Kdy. Yogyakart")

#select only 4 columns
kotayk_sf_1 <- kotayk_sf %>%
  select(DESA, KECAMATAN, KABUPATEN, PROPINSI, geometry)

#edit column names
colnames(kotayk_sf_1) <- c("kelurahan", "kecamatan", "kabupaten", "provinsi", "geometry")

#make lowercase to all kelurahan and kecamatan value
kotayk_sf_1$kelurahan <- tolower(kotayk_sf_1$kelurahan)
kotayk_sf_1$kecamatan <- tolower(kotayk_sf_1$kecamatan)

#edit typo on shp file
kotayk_sf_1$kelurahan <- gsub("bausaran","bausasran", kotayk_sf_1$kelurahan)
kotayk_sf_1$kecamatan <- gsub("kota gede","kotagede", kotayk_sf_1$kecamatan)
kotayk_sf_1$kelurahan <- gsub("majumuju","muja muju", kotayk_sf_1$kelurahan)
kotayk_sf_1$kelurahan <- gsub("brotokusuman","brontokusuman", kotayk_sf_1$kelurahan)
kotayk_sf_1$kelurahan <- gsub("sosromanduran","sosromenduran", kotayk_sf_1$kelurahan)

#make change back to factor type
kotayk_sf_1$kelurahan <-  as.factor(kotayk_sf_1$kelurahan)
kotayk_sf_1$kecamatan <-  as.factor(kotayk_sf_1$kecamatan)

#import data pilpres 2019
pilpreskotayk <- read_csv("201905031112543471ExportPilpres.csv")

#set kelurahan, kecamatan, kota, and provinsi to factor type
pilpreskotayk$kelurahan <-  as.factor(pilpreskotayk$kelurahan)
pilpreskotayk$kecamatan <-  as.factor(pilpreskotayk$kecamatan)
pilpreskotayk$kota <- as.factor(pilpreskotayk$kota)
pilpreskotayk$provinsi <- as.factor(pilpreskotayk$provinsi)

#count vote based on kelurahan
pilpres <- pilpreskotayk %>%
  select(kecamatan, kelurahan, jokowi_amin, prabowo_uno) %>%
  group_by(kecamatan, kelurahan) %>%
  summarise_if(is.numeric, sum)


#make lowercase to all kelurahan and kecamatan value
pilpres$kelurahan <- tolower(pilpres$kelurahan)
pilpres$kecamatan <- tolower(pilpres$kecamatan)

#make change back to factor type
pilpres$kelurahan <-  as.factor(pilpres$kelurahan)
pilpres$kecamatan <-  as.factor(pilpres$kecamatan)


#joining two tables (GIS data and pilpres result)
pilpres_kotayk_gis <- kotayk_sf_1 %>%
  left_join(pilpres, by = "kelurahan")

#count sum all for all candidates
pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(sum_vote = jokowi_amin + prabowo_uno)

#calculate perbandingan suara 01
pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(jokowi_amin_std = jokowi_amin/sum_vote)

#calculate perbandingan suara 02
pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(prabowo_uno_std = prabowo_uno/sum_vote)

#change to percent
pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(jokowi_amin_std = percent(jokowi_amin_std))

pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(prabowo_uno_std = percent(prabowo_uno_std))

#hitung selisih suara
pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(difference = abs(jokowi_amin_std - prabowo_uno_std))

pilpres_kotayk_gis <- pilpres_kotayk_gis %>%
  mutate(winner = ifelse(jokowi_amin_std > prabowo_uno_std, "01", "02"))

pilpres_kotayk_gis$winner <-  as.factor(pilpres_kotayk_gis$winner)

#look summary about spatial data
class(pilpres_kotayk_gis)
head(pilpres_kotayk_gis, 3)
glimpse(pilpres_kotayk_gis)

# convert to simplified geometry
simp_pilpres_kotayk_gis_geometry <- ms_simplify(pilpres_kotayk_gis, keep = 0.01, keep_shapes = TRUE)

# convert to geometry
pilpres_kotayk_gis_geometry <- st_geometry(pilpres_kotayk_gis)

#plot into graphical map
plot(pilpres_kotayk_gis_geometry)
plot(pilpres_kotayk_gis['jokowi_amin'])

#check file size
library(pryr)
object.size(prov_indonesia_geometry)
object.size(simp_prov_indonesia_geometry)



#choropleths map for vote margin
plot_selisih <- pilpres_kotayk_gis %>% 
  tm_shape() +
  tm_fill(col = "difference", title =  "Selisih Suara (dalam persen)", palette = "OrRd", n = 4, legend.is.portrait = FALSE, labels = c("<20%", "20-40%", ">60%")) +
  tm_borders(lwd = 0.2) +
  tm_text("kelurahan", size = 0.2) +
  tm_style("gray") +
  tm_layout(
    main.title = "Margin Suara pada Pemilihan Umum Presiden 2019 Kota Yogyakarta",
    main.title.position = c("center"),
    main.title.size = 0.5,
    legend.title.size = 0.4,
    legend.text.size = 0.3,
    legend.position = c("left", "bottom")
  ) +
tm_credits("Data: Badan Saksi Pemilu Nasional PDI Perjuangan", position = c("left", "top"), size = 0.2)
#save in high resolution images
tmap_save(plot_selisih, dpi = 1200, filename = "selisih suara pilpres kota yk.png")

#bubble maps
bubblemaps_pilpreskotayk <- pilpres_kotayk_gis %>% 
  tm_shape() +
  tm_polygons() +
  tm_bubbles(col = "red", size = "jokowi_amin_std", scale = 1, title.size = "") +
  tm_text("kelurahan", size = 0.3, root = 5, legend.size.show = FALSE) +
  tm_layout(
    main.title = "Jumlah Suara Jokowi-Amin di Kota Yogyakarta",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("left", "bottom")
  )

tmap_save(bubblemaps_pilpreskotayk, dpi = 600, filename = "bubblemaps_pilpreskotayk.png")

#choropleths map for winner in kelurahan
plot_pemenang <- pilpres_kotayk_gis %>% 
  tm_shape() +
  tm_fill(col = "winner", title =  "Nomor Urut Pilpres", palette = c("white", "red")) +
  tm_borders(lwd = 0.2) +
  tm_text("kelurahan", size = 0.2) +
  tm_style("gray") +
  tm_layout(
    main.title = "Pemenang Pemilihan Presiden 2019 Kota Yogyakarta",
    main.title.position = c("center"),
    main.title.size = 0.5,
    legend.title.size = 0.4,
    legend.text.size = 0.4,
    legend.position = c("left", "bottom")
  ) +
  tm_credits("Data: Badan Saksi Pemilu Nasional PDI Perjuangan", position = c("left", "top"), size = 0.2)
#save in high resolution images
tmap_save(plot_pemenang, dpi = 1200, filename = "pemenang pilpres kota yk.png")

#choropleths map for percentage jokowi-amin
plot_persen_jokowi_amin <- pilpres_kotayk_gis %>% 
  tm_shape() +
  tm_fill(col = "jokowi_amin_std", title =  "Persentase Suara", palette = "YlOrRd") +
  tm_borders(lwd = 0.2) +
  tm_text("kelurahan", size = 0.2) +
  tm_style("gray") +
  tm_layout(
    main.title = "Persentase Suara 01. Jokowi-Amin di Kota Yogyakarta",
    main.title.position = c("center"),
    main.title.size = 0.5,
    legend.title.size = 0.4,
    legend.text.size = 0.4,
    legend.position = c("left", "bottom")
  ) +
  tm_credits("Data: Badan Saksi Pemilu Nasional PDI Perjuangan", position = c("right", "bottom"), size = 0.2)
#save in high resolution images
tmap_save(plot_persen_jokowi_amin, dpi = 1200, filename = "Persentase Suara 01. Jokowi-Amin di Kota Yogyakarta.png")

library(grid)

inset <- diy_sf %>%
  mutate(kota = ifelse(KABUPATEN == "Kdy. Yogyakart", TRUE, FALSE)) %>%
  tm_shape() +
  tm_fill(col = "kota", palette = c("grey", "red")) +
  tm_style("cobalt") +
  tm_legend(show = FALSE) 

plot_pemenang
plot_with_inset <- print(inset, vp = viewport(0.15, 0.2, width = 0.2, height = 0.4))


#coba facet
coba_facet <- diy_sf %>%
  tm_shape() +
  tm_borders(lwd = 0.5, col = "white") +
  tm_fill(col = "KEMATIAN", title = '', palette = "-inferno") +
  tm_facets(by = "KABUPATEN", nrow = 3, free.coords = TRUE) +
  tm_layout(
    main.title = "Angka Kematian di Setiap Kelurahan berdasarkan Provinsi",
    main.title.size = 0.5,
    main.title.position = "center",
    legend.outside.position = "right"
  )

tmap_save(coba_facet, dpi = 2000, filename = "coba facet.png")
