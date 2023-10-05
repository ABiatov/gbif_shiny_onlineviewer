# Administrative division

## Level 1 - Oblast

Old file: ***Ukr_Obl_NEW.shp***
Old layer: *Ukr_Obl*

New file: **adm_1.shp**
New layer: *adm_1*

Ukr_Obl | adm_1 | obligatory field |
-----: | :-----: | :-----
obl_name | adm_1_name | +


## Level 2 - Rayon

Old file: ***Ukr_Raion_NEW.shp***
Old layer: *Ukr_Rai*

New file: **adm_2.shp**
New layer: *adm_2*

Ukr_Rai | **adm_2** | obligatory field |
-----: | :-----: | :-----
obl_name | adm_1_name | +
raion_name | adm_2_name | +


## Level 3 - OTG

Old file: ***Ukr_OTG_NEW.shp***
Old layer: *Ukr_OTG*

New file: **adm_3.shp**
New layer: *adm_3*

Ukr_OTG | adm_3 | obligatory field |
-----: | :-----: | :-----
shapeID | shapeID | +
obl_name | adm_1_name | -
raion_name | adm_2_name | +
OTG_name | adm_3_name | +
- | type | -
