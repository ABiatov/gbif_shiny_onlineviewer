# Code Analysis for the `gbif_app_v2` Application

## Table of Contents
1. [1. Overview](#sec-1)
2. [2. Step-by-Step Breakdown](#sec-2)
3. [3. Key Concepts](#sec-3)
4. [4. Potential Issues](#sec-4)
5. [5. Usage Example](#sec-5)
6. [6. Operational Issue on shinyapps.io](#sec-6)

<a id="sec-1"></a>
## 1. Overview
This is a Shiny application for working with GBIF data. A user defines an area of interest, either by selecting an administrative unit, uploading a boundary, or drawing a polygon, optionally creates a buffer around it, retrieves a spatial subset of occurrence records, and filters the results by biodiversity conservation criteria.

The application then displays the results on maps and in tables, and generates CSV/XLSX exports and HTML/DOCX reports.

<a id="sec-2"></a>
## 2. Step-by-Step Breakdown

### 2.1. Files and Module Roles
- `app.R` - the main UI and server file, containing the core reactivity, map, filter, and report logic.
- `config.R` - constants and reference data: column lists, filters, colors, and buffer parameters.
- `custom_functions.R` - geospatial helper functions:
  - `polygon_bufferisation()` - buffers a polygon in a metric CRS and converts it back to WGS84.
  - `leaf_draw_sf_polyg()` - converts Leaflet Draw coordinates into an `sf` geometry.
- `global_reactive_value.R` - global `reactiveVal()` values for the AOI, buffer, report tables, and counters.
- `localization_ua.R` / `localization_en.R` - UI and report text strings for localization.
- `templates/report.Rmd` - the final report template.

### 2.2. Initialization (`app.R`, lines ~1-90)
- The code clears the environment with `rm(list = ls())` and runs `gc()`.
- Libraries are loaded: `shiny`, `sf`, `leaflet`, `dplyr`, `DT`, `rmarkdown`, and others.
- Configuration, reactive values, functions, and localization are sourced. Ukrainian is used by default.
- The application reads:
  - dataset metadata (`metadata.Rdata`) for the DOI and date;
  - administrative layers `adm_1/2/3.shp`.
- The OTG list is prepared for `pickerInput`.

Why it is structured this way:
- Splitting logic into `source(...)` files keeps `app.R` relatively readable.
- Keeping configuration, functions, and UI/server logic separate is a reasonable baseline modular style for Shiny.

### 2.3. UI Layer (`app.R`, lines ~94-339)
The interface is built with a `tabsetPanel` containing five tabs:
1. Map and territory selection.
2. Filtering and a map of filtered points.
3. Table preview and CSV/XLSX export.
4. Reports, summary tables, and HTML/DOCX generation.
5. About.

Additional details:
- A loading spinner is implemented through `conditionalPanel` and CSS when `shiny-busy` is active.
- All labels are taken from the localization file instead of being hard-coded in the UI.

Shiny idiom:
- This follows the classic "thin UI + heavy server" pattern: the UI describes structure, while the logic lives in the server.

### 2.4. Creating Maps and the Drawing Tool (`app.R`, lines ~345-413)
- Two maps are created:
  - `map` - the main AOI map;
  - `map2` - the map shown after filtering.
- Base layers include Visicom and ESRI imagery.
- `addDrawToolbar` is attached to `map` for drawing, editing, and deleting polygons.
- Updates use `leafletProxy` so the whole map does not need to be rebuilt.

Why this matters:
- `leafletProxy` reduces load and avoids visible full map redraws.

### 2.5. Building the AOI, or Area of Interest (`app.R`, lines ~414-586)
Four AOI sources are supported:
- region selection (`regions`);
- district selection (`raions`);
- hromada selection (`OTG`);
- a user file (`.kml/.kmz`) or a polygon drawn with the mouse.

Flow:
1. Any new action clears old layers and points.
2. Dependent selectors are updated, for example the district list after a region is selected.
3. The AOI is written into `reaktive_aoi_polygon()`.
4. `reaktive_bufered_polygon()` is synchronized. Initially, it equals the AOI before buffering.
5. The data retrieval button is enabled.

Analogy:
- The AOI works like a search frame in a map search tool: first the user defines the frame, then requests data inside it.

### 2.6. Buffering (`app.R` ~590-609 and `custom_functions.R`)
- When `buffer_radius` changes, the application calls:
  - `polygon_bufferisation(sf_input_polygon, radius)`.
- The function:
  1. transforms the geometry to the metric CRS `3537`;
  2. builds a buffer in meters;
  3. converts the result back to EPSG:4326;
  4. merges the geometry with `st_union`.

Why this is required:
- Buffering in degrees in WGS84 is not physically correct for distance-based operations, so a metric projection is necessary.

Example of the key logic with comments:
```r
sf_polygon_buffered <- st_transform(sf_input_polygon, CRS_used_in_calculations) %>%  # to meters
  st_buffer(dist = as.numeric(radius), nQuadSegs = 4) %>%                             # buffer in m
  st_transform(4326) %>%                                                               # back to lon/lat
  st_union()                                                                            # single geometry
```

### 2.7. Retrieving GBIF Data by AOI (`app.R`, lines ~612-747)
- The `sf_clipped_data` reactive is triggered by the `act_get_gbif_data` button.
- The source is the FlatGeobuf file `gbif_sf_dataset.fgb`.
- The code uses `wkt_filter`, which means the spatially filtered subset is read directly:

```r
read_sf(path_datadump_fgb, wkt_filter = reaktive_bufered_polygon() %>% st_as_text())
```

Why this is a good approach:
- The full occurrence dataset is not loaded into memory for every request.
- The spatial filter is applied during reading, which is usually faster.

After loading:
- The AOI, buffer, and points are drawn on `map`.
- The same points are initially shown on `map2`, then replaced by the filtered result.
- The apply and clear filter buttons are enabled.

### 2.8. Filtering Occurrence Records (`app.R`, lines ~792-885)
- `sf_filteredData()`:
  - takes `sf_clipped_data()`;
  - applies a large OR filter across:
    - the Red Book of Ukraine (`ЧКУ`);
    - IUCN;
    - international lists;
    - regional lists;
    - invasiveness;
  - then applies an AND filter by kingdom (`kingdom_filters`).
- The result is displayed on `map2` with popups and links.

Pattern:
- The application uses Shiny's reactive graph: input changes trigger recalculation of dependent nodes.
- `intern_filt_present` and `region_filt_present` act as masks for active filters by index.

### 2.9. Table and Exports (`app.R`, lines ~888-932)
- `df_filteredData()` removes geometry with `st_drop_geometry` and keeps the required fields.
- The Preview tab table is rendered with `DT::renderDataTable`.
- Export formats:
  - CSV via `write.csv`;
  - XLSX via `write_xlsx`.
- The UI also displays the number of records and the GBIF DOI citation.

### 2.10. Summary Tables and Report (`app.R`, lines ~939-1605 + `templates/report.Rmd`)
- When `refresh_filters` is clicked, the application builds:
  - a summary table by category;
  - a general species table;
  - separate tables for each agreement or list.
- Tables and `nrow_*` values are stored in `reactiveVal` objects so they can be used both in the UI and in R Markdown.
- `downloadReport`:
  - copies the template and logos to a temporary folder;
  - runs `rmarkdown::render` in HTML or Word format;
  - returns the file to the user.

Why this scheme works:
- R Markdown can access the already calculated reactive data within the same session.

### 2.11. Global Reactive Values (`global_reactive_value.R`)
- `reaktive_aoi_polygon`, `reaktive_bufered_polygon`, and `df_rare_lists` are initialized.
- A loop with `assign()` dynamically creates pairs of values:
  - `tab_filtred_*`;
  - `nrow_*`.

This reduces duplication when declaring variables, but makes code navigation harder because the names are not visible explicitly in one place.

<a id="sec-3"></a>
## 3. Key Concepts

### 3.1. Shiny Reactive Architecture
- `observeEvent` - performs an action when an event occurs, such as a button click or input change.
- `reactive` - defines a computed value that depends on inputs.
- `eventReactive` - computes a value only when a specific trigger fires.

In simple terms, this is a dependency graph where nodes are automatically recalculated when their inputs change.

### 3.2. Spatial Processing with `sf`
- `st_read`, `read_sf` - read spatial data.
- `st_union`, `st_bbox` - merge geometries and calculate bounding boxes.
- `st_transform` + `st_buffer` - perform correct buffering in a metric projection.

### 3.3. Patterns
- Modular configuration: parameters and strings are moved out of `app.R`.
- Proxy update pattern with `leafletProxy` for fast incremental map updates.
- Template pattern for reporting with `templates/report.Rmd` and a set of reactive tables.

<a id="sec-4"></a>
## 4. Potential Issues

### 4.1. Logical and Functional Risks
- `if(is.null(reaktive_bufered_polygon)){...}` checks the function, not its value (`app.R:618`).
  It should be `reaktive_bufered_polygon()`.
- `arrange("kingdom", "class", ...)` sorts by string literals, not by columns (`app.R:891`).
  It should be `arrange(kingdom, class, family, scientificName)`.
- `fitBounds` on an empty filter result can produce an invalid bbox (`app.R:860-865`), so the code needs `req(nrow(sf_filteredData()) > 0)` or a fallback.
- When `clear_filters` is used, almost all lists are set to `NULL` (`app.R:767-790`). The resulting OR filter may reduce the selection to zero records. This may be intentional, but the UX is ambiguous.

### 4.2. Performance
- The report section contains a lot of duplication, with dozens of similar blocks (`app.R:939-1538`), making maintenance and changes expensive.
- Frequent debug `print(...)` calls in `observe` (`app.R:1634-1667`) add log noise and can slow the app during frequent reactive events.
- `base_map(..., basemap='google-terrain')` in `renderPlot` may be heavy and network-bound (`app.R:1549-1551`).

### 4.3. Reliability and Security
- User-uploaded `.kml/.kmz` files are read without size limits or additional geometry validation (`app.R:496-513`), which can lead to errors or long processing times.
- `load(...Rdata)` depends on the binary file contents and creates objects as-is (`app.R:51`), which makes the data schema hard to control.
- Using `rm(list = ls())` at startup and session end (`app.R:10`, `app.R:1630`) can unexpectedly remove objects and makes maintenance harder.

<a id="sec-5"></a>
## 5. Usage Example

### 5.1. How to Run the Application
```r
setwd("gbif_app_v2")
shiny::runApp("app.R")
```

### 5.2. Typical User Flow
1. On the **Map** tab, select a district or hromada, or upload/draw a polygon.
2. Choose a buffer radius, or set it to `0` for no buffer.
3. Click **Get GBIF Data**.
4. On the filters tab, choose criteria and click **Apply Filters**.
5. Download CSV/XLSX or generate a report on the reports tab.

### 5.3. Example of a Complex Filter, Conceptually
```r
# Take data clipped by AOI and apply filters:
# 1) at least one conservation criterion (OR),
# 2) then a kingdom constraint (AND).
sf_filtered <- sf_clipped %>%
  dplyr::filter(
    iucnRedListCategory %in% selected_iucn |
    ЧКУ %in% selected_chku |
    (selected_berne1 & BernAppendix1 == "yes")
  ) %>%
  dplyr::filter(kingdom %in% selected_kingdoms)
```

<a id="sec-6"></a>
## 6. Operational Issue on shinyapps.io

The current production version of the application hosted on `shinyapps.io` regularly runs into RAM limits. When several users work at the same time, or when requests cover large territories and return tens of megabytes of point data, memory consumption grows sharply. This causes sessions to terminate unexpectedly and can bring the application down.

### 6.1. How It Appears
- Out-of-memory errors during query execution.
- User sessions disconnect while data is being loaded or filtered.
- The application becomes unstable when several users are active in parallel.

### 6.2. When It Happens Most Often
- Several users are working at the same time.
- A large AOI is requested.
- The resulting subset contains a large volume of point data, usually tens of MB or more.

### 6.3. Impact on Users
- Loss of progress in the current session.
- Inability to complete analysis for large territories.
- Lower trust in service stability during peak load.
