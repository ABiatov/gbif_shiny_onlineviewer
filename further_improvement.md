# Further Improvement Plan for `gbif_app_v2`

This document contains the development, migration, and scaling proposals extracted from `Code_Explanation.md`.

## Table of Contents
1. [1. Quick Improvements for the Current Code](#sec-1)
2. [2. Goals & Scope](#sec-2)
3. [3. Stakeholders & Users](#sec-3)
4. [4. User Journeys](#sec-4)
5. [5. Current Architecture (As-Is)](#sec-5)
6. [6. Module Responsibility Map](#sec-6)
7. [7. Data Contracts](#sec-7)
8. [8. Dependency Inventory](#sec-8)
9. [9. Non-Functional Requirements (NFR)](#sec-9)
10. [10. Error Handling & Failure Modes](#sec-10)
11. [11. Security & Privacy](#sec-11)
12. [12. Observability](#sec-12)
13. [13. Test Strategy](#sec-13)
14. [14. Tech Debt Register](#sec-14)
15. [15. ADR (Architecture Decision Record)](#sec-15)
16. [16. Target Stack Options for Migration](#sec-16)
17. [17. Concept Mapping (Shiny -> New Architecture)](#sec-17)
18. [18. API Boundary Plan (To-Be)](#sec-18)
19. [19. State Management Plan (To-Be)](#sec-19)
20. [20. Data Pipeline Plan (To-Be)](#sec-20)
21. [21. Backward Compatibility & Parity Checklist](#sec-21)
22. [22. Migration Strategy](#sec-22)
23. [23. Roadmap (Draft)](#sec-23)
24. [24. Open Questions for AI Brainstorming](#sec-24)
25. [25. Load Profile and Scaling Plan (30-50 Concurrent Users)](#sec-25)

<a id="sec-1"></a>
## 1. Quick Improvements for the Current Code

### 1.1. Possible Improvements
- Move repeated report blocks into a generator function driven by configuration, for example a mapping of field, label, and reactive key.
- Replace index-based filtering logic such as `intern_filt_present()[N]` with a `name -> column` dictionary to remove position-based fragility.
- Add `req(...)` and `validate(need(...))` before critical calculations such as `bbox`, `renderPlot`, and file reading.
- Move geospatial operations into a separate `R/geo.R` module and report aggregates into `R/report.R`.
- Add tests:
  - a unit test for `polygon_bufferisation`;
  - filter tests for OR/AND logic correctness;
  - an integration test for report generation.

<a id="sec-2"></a>
## 2. Goals & Scope

### 2.1. Goals of the Current Version (As-Is)
- Provide fast web access to GBIF occurrence records within a selected area of interest.
- Support filtering by biodiversity conservation criteria, including national and international lists.
- Support exporting results to table formats and reports.

### 2.2. Goals of the Next Version (To-Be)
- Improve maintainability and reduce code duplication.
- Improve resilience to invalid input data and empty results.
- Prepare the architecture for migration to another language or stack without losing functionality.

### 2.3. Out of Scope
- A full redesign of the domain model, such as taxonomy or classification, without a separate project.
- Manual editing of the source GBIF dataset from the application interface.

<a id="sec-3"></a>
## 3. Stakeholders & Users
- Primary users: biodiversity specialists, analysts, conservation NGOs, and government or governance bodies.
- Technical users: developers and analysts who maintain the code and data.
- Business interest: fast and reproducible occurrence selection for decision-making and reporting.

<a id="sec-4"></a>
## 4. User Journeys

### 4.1. Main Scenario
1. The user selects an AOI.
2. The user adds a buffer if needed.
3. The user requests a GBIF subset.
4. The user applies filters.
5. The user reviews the map and table.
6. The user exports CSV/XLSX/HTML/DOCX.

### 4.2. Alternative Scenarios
- AOI provided through a `.kml/.kmz` upload.
- AOI created by drawing and editing on the map.
- Quick filter reset and repeated analysis.

### 4.3. Failure Scenarios
- Invalid boundary file.
- Empty result after filtering.
- Error during report generation.

<a id="sec-5"></a>
## 5. Current Architecture (As-Is)

### 5.1. Component Context Diagram
- UI: Shiny tabs, inputs, and outputs.
- Reactive state: `reactiveVal`, `reactive`, and `eventReactive`.
- Spatial engine: `sf`, `leaflet`, and `leafletProxy`.
- Data layer: FlatGeobuf plus RData metadata.
- Report engine: `rmarkdown` and `report.Rmd`.

### 5.2. Data Flow
1. AOI selection or drawing -> `reaktive_aoi_polygon`.
2. Buffering -> `reaktive_bufered_polygon`.
3. Spatial query with `wkt_filter` -> `sf_clipped_data`.
4. Business filtering -> `sf_filteredData`.
5. Display, export, and report generation.

### 5.3. Architectural Constraints
- Reactive logic and business logic are heavily mixed in a single `app.R`.
- Report table blocks contain a large amount of duplication.
- The domain layer is weakly isolated from the UI layer.

<a id="sec-6"></a>
## 6. Module Responsibility Map

| Module | Responsibility | Risks |
|---|---|---|
| `app.R` | UI, reactivity, filters, maps, exports, reports | Overloaded, duplicated logic |
| `config.R` | Constants, field/filter lists, styles | Fragile when column names change |
| `custom_functions.R` | Geospatial helper functions | Input validation is needed |
| `global_reactive_value.R` | Reactive containers | Dynamic names make refactoring harder |
| `localization_*.R` | UI/report text | Localization files may drift apart |
| `templates/report.Rmd` | Report structure | Duplicated conditions and tables |

<a id="sec-7"></a>
## 7. Data Contracts

### 7.1. Input Contracts
- AOI geometry:
  - Format: polygon or multipolygon in WGS84 CRS (EPSG:4326).
  - Sources: `adm_*`, `.kml/.kmz`, draw toolbar.
- GBIF dataset fields, critical:
  - Geospatial: `Latitude`, `Longitude`, `geometry`.
  - Taxonomy: `kingdom`, `class`, `family`, `scientificName`, `nameUk`.
  - Filter attributes: `ЧКУ`, `iucnRedListCategory`, `BernAppendix*`, `ЧС_*`, `Invasive`.

### 7.2. Output Contracts
- Map: occurrence points plus AOI and buffer.
- Table exports: CSV/XLSX with columns from `colnames_set1/2`.
- Report: HTML/DOCX based on `templates/report.Rmd`.

### 7.3. Invariants
- The buffer is calculated in meters after CRS transformation.
- Every visualization should handle an empty dataset without breaking the UI.
- Column names in `config.R` must match the actual dataset schema.

<a id="sec-8"></a>
## 8. Dependency Inventory

### 8.1. Main Packages
- UI/reactivity: `shiny`, `shinyWidgets`, `shinyjs`, `DT`.
- Geospatial: `sf`, `sp`, `leaflet`, `leaflet.extras`, `leaflet.esri`, `leafem`.
- Data/export: `dplyr`, `openxlsx2`, `data.table`.
- Reports: `rmarkdown`, `knitr`, `markdown`.

### 8.2. External Dependencies
- Basemap services: Visicom, ESRI, terrain basemap.
- Local shapefiles and datasets in `gbif_data/`.

<a id="sec-9"></a>
## 9. Non-Functional Requirements (NFR)

### 9.1. Performance
- Expected response time for spatial query and filtering.
- Limits on uploaded file size and point count.

### 9.2. Reliability
- Correct behavior with empty selections, broken geometries, and missing fields.
- Predictable recovery after report errors.

### 9.3. Scalability
- Ability to work with larger datasets without loading everything into RAM.
- Ability to split the application into a backend API and frontend.

### 9.4. UX
- Clear error and loading-status messages.
- Stable map interactivity on low-powered machines.

<a id="sec-10"></a>
## 10. Error Handling & Failure Modes

| Scenario | Current Behavior | Desired Behavior |
|---|---|---|
| Empty `sf_filteredData` | Errors may occur in `fitBounds`/plot | `validate(need(...))`, fallback bbox, informative text |
| Invalid `.kml/.kmz` | Read error | try-catch plus user-facing text |
| Required dataset column is missing | dplyr/select error | Startup schema check plus fail-fast behavior |
| Report render failure | Error during download | try-catch plus logs and a user-facing message |

<a id="sec-11"></a>
## 11. Security & Privacy
- Validate MIME type, extension, and size for uploaded files.
- Validate AOI geometry with `st_is_valid` and use `st_make_valid` if needed.
- Record basemap and GBIF licenses and attribution in the report.
- Avoid executing or trusting unchecked binary inputs without schema validation.

<a id="sec-12"></a>
## 12. Observability

### 12.1. What to Log
- Spatial query execution time.
- Filtering and report render time.
- Selection size before and after filters.
- Geometry parsing and report generation errors.

### 12.2. Metrics
- `query_duration_ms`
- `filtered_rows_count`
- `report_generation_ms`
- `error_rate_by_stage`

### 12.3. Minimum Alerts
- Sharp increase in render/download errors.
- Abnormally long response time.

<a id="sec-13"></a>
## 13. Test Strategy

### 13.1. Unit Tests
- `polygon_bufferisation`: CRS correctness, geometry type, area greater than zero.
- `leaf_draw_sf_polyg`: coordinate conversion into a valid polygon.
- Filtering functions or blocks: correct OR/AND logic.

### 13.2. Integration Tests
- Scenario: AOI -> buffer -> get data -> filters -> export.
- Report generation for non-empty and empty datasets.

### 13.3. Contract Tests
- Input data schema validation, including required columns and types.

<a id="sec-14"></a>
## 14. Tech Debt Register

| Debt | Impact | Priority | Candidate Fix |
|---|---|---|---|
| Duplicated report blocks | High cost of change | High | Config-driven generator |
| Index-based filter logic | Risk of errors when order changes | High | `filter_key -> column` mapping |
| Mixed UI and domain logic | Difficult refactoring | High | Extract a service layer |
| Debug `print` calls in production code | Noise and overhead | Medium | Structured logger |

<a id="sec-15"></a>
## 15. ADR (Architecture Decision Record)

### ADR-001: Spatial Query via FlatGeobuf + `wkt_filter`
- Status: accepted in the current version.
- Reason: lower memory usage and faster selection.
- Trade-off: dependency on the format and on valid WKT AOI input.

### ADR-002: Reactive Monolithic Shiny Architecture
- Status: historically accepted.
- Reason: fast start and minimal time-to-market.
- Trade-off: limited modularity and difficult scaling.

<a id="sec-16"></a>
## 16. Target Stack Options for Migration

### Option A: Python + FastAPI + React + deck.gl/MapLibre
- Pros: strong backend ecosystem, flexible API, scalability.
- Cons: more infrastructure work.

### Option B: TypeScript Full Stack (NestJS + React)
- Pros: one language, good end-to-end typing.
- Cons: the geospatial pipeline may require more manual setup.

### Option C: R Backend API + Separate Frontend
- Pros: smoother migration, preserves part of the R expertise.
- Cons: some R/Shiny limitations may remain.

<a id="sec-17"></a>
## 17. Concept Mapping (Shiny -> New Architecture)

| Shiny Concept | Target Equivalent |
|---|---|
| `reactiveVal` | Store/State (Redux/Zustand/Signals) + server state cache |
| `observeEvent` | Event handlers + command bus/use-case services |
| `eventReactive` | Explicit query trigger (API call / mutation) |
| `render*` | Component rendering + state selectors |
| `leafletProxy` | Imperative map controller/ref |

<a id="sec-18"></a>
## 18. API Boundary Plan (To-Be)
- `POST /aoi/query` - spatial query by AOI plus buffer.
- `POST /filters/apply` - apply filters and return a table or GeoJSON.
- `GET /exports/csv|xlsx` - exports.
- `POST /reports` - report generation.

Minimum request contract:
```json
{
  "aoi_wkt": "POLYGON((...))",
  "buffer_m": 5000,
  "filters": {
    "iucn": ["CR", "EN"],
    "international": ["Bern Appendix 1"],
    "kingdom": ["Animalia", "Plantae"],
    "invasive": false
  }
}
```

<a id="sec-19"></a>
## 19. State Management Plan (To-Be)
- Split state into:
  - `map_state` (AOI, bbox, buffer),
  - `query_state` (request status, timing, errors),
  - `filter_state`,
  - `result_state` (table, geo-features, summary),
  - `report_state`.
- Introduce explicit statuses: `idle | loading | success | empty | error`.

<a id="sec-20"></a>
## 20. Data Pipeline Plan (To-Be)
1. Validate AOI.
2. Normalize CRS.
3. Run an indexed spatial query.
4. Apply business filters.
5. Build report aggregations.
6. Cache frequently repeated requests.

<a id="sec-21"></a>
## 21. Backward Compatibility & Parity Checklist

### 21.1. Compatibility
- Keep the same AOI input formats (`.kml/.kmz`, draw).
- Keep the key export formats: CSV/XLSX/DOCX/HTML.
- Keep filtering rules and the default column set.

### 21.2. Feature Parity
- [ ] AOI selection by `adm_1/2/3`.
- [ ] Custom polygon.
- [ ] Buffer in meters.
- [ ] Spatial query against the dataset.
- [ ] Filters by `ЧКУ`, IUCN, conventions, and regional lists.
- [ ] Map with popups and links.
- [ ] CSV/XLSX export.
- [ ] Report generation.

<a id="sec-22"></a>
## 22. Migration Strategy

### 22.1. Stage 1 - Stabilize As-Is
- Fix critical bugs: `is.null`, `arrange`, and empty bbox handling.
- Add contract checks for the data schema.
- Add a minimal test suite.

### 22.2. Stage 2 - Extract Core
- Move geospatial and filtering logic into an independent service layer.
- Prepare API contracts and end-to-end parity tests.

### 22.3. Stage 3 - Parallel Run
- Run the new service next to the old one.
- Compare results for identical inputs.
- Keep a discrepancy log.

### 22.4. Stage 4 - Cutover
- Switch the UI to the new backend.
- Keep fallback to the old version for a limited period.

<a id="sec-23"></a>
## 23. Roadmap (Draft)

### Milestone M1 (2-4 Weeks)
- Critical bug fixes and stabilization.
- Logs, metrics, and basic tests.

### Milestone M2 (4-8 Weeks)
- Service layer and API prototype.
- Refactoring of report aggregates.

### Milestone M3 (8-12 Weeks)
- New UI/architecture in parallel-run mode.
- Parity and performance verification.

### Milestone M4 (12+ Weeks)
- Production cutover.
- Post-migration optimization.

<a id="sec-24"></a>
## 24. Open Questions for AI Brainstorming
- What response-time SLA is acceptable for spatial queries?
- What maximum AOI or file size should be supported?
- Is a multi-user mode with state isolation and job queues needed?
- Which is more important: development speed or long-term scalability?
- What dataset versioning and report reproducibility strategy is needed?
- Is offline mode or basemap caching required?
- What level of backward compatibility is mandatory for external users?

<a id="sec-25"></a>
## 25. Load Profile and Scaling Plan (30-50 Concurrent Users)

### 25.1. Target Load Profile
- Concurrent active users: `30-50`.
- Load type: interactive spatial queries with large AOI polygons.
- Result size: up to `10-50+ MB` of points and attributes per request.
- Peak mode: several heavy requests run at the same time, including report generation.

### 25.2. SLO/SLA for VNext
- `P50` for a typical request: up to `2-4 sec`.
- `P95` for a heavy request: up to `10-20 sec`, or move it to async mode.
- API errors (`5xx`) under load: no more than `1%`.
- Large report generation time: up to `30-90 sec` as a background job.

### 25.3. Query Classes and Execution Modes
- `Small/Medium query`, with results up to about 10 MB: synchronous response.
- `Large query`, with results above about 10 MB: asynchronous queueing; the user receives a `job_id`.
- `Very large export/report`: async only, with notification when the file is ready.

### 25.4. Architectural Measures for Resilience
- Introduce a background task queue for heavy operations such as `query`, `report`, and `export`.
- Limit heavy-task parallelism in the worker pool, for example `2-4` tasks per instance.
- Add backpressure: limits on active heavy jobs per user and per system.
- Separate API instances for fast responses from worker instances for heavy tasks.
- Support horizontal scaling of the stateless layer through a load balancer.

### 25.5. Working with Large Geospatial Data
- Use a spatial index and bbox pre-filtering before exact intersections.
- Store and read data in a format optimized for spatial scans, such as FlatGeobuf, GeoParquet, or PostGIS.
- Return geospatial data in compressed form with `gzip`/`br`, and support chunked or streaming responses.
- Add a server-side limit on the maximum number of points shown on the interactive map and use progressive aggregation.
- For large map volumes, degrade detail gracefully with clustering, tiling, or sampling.

### 25.6. Limits and Protective Mechanisms
- Limit uploaded AOI file size and geometry complexity, including vertex count.
- Set per-stage timeouts for spatial query, filtering, export, and report rendering.
- Use retry policies only for idempotent steps.
- Apply rate limiting per user/IP for heavy endpoints.
- Add circuit breakers for external dependencies, such as basemap or remote services.

### 25.7. Caching
- Cache repeated results by key: `hash(AOI + buffer + filters + dataset_version)`.
- Use separate TTL values for interactive previews and exports.
- Cache intermediate report aggregations.
- Invalidate cache when the dataset version changes.

### 25.8. Observability Under Load
- Metrics: `concurrent_users`, `queue_depth`, `job_wait_ms`, `job_run_ms`, `p95_latency`, `memory_rss`, `cpu`, `error_rate`.
- Logs: dimensions such as `query_class`, `aoi_area`, `result_size_mb`, and `rows_count`.
- Alerts: queue growth, rising `p95`, OOM events, and increasing timeout share.

### 25.9. Load Testing Plan, Required
1. Prepare three scenario sets: `small`, `medium`, and `large` AOI.
2. Run tests with `30`, `40`, and `50` concurrent users.
3. Run a separate stress test for simultaneous large requests with `10-50+ MB` responses.
4. Run a soak test for `1-2 hours` to check for memory leaks and degradation.
5. Record pass thresholds for SLO and resilience.

### 25.10. Acceptance Criteria for Performance
- The system stably serves `30-50` concurrent users without critical UX degradation.
- Heavy requests do not block interactive operations for other users.
- Memory and queue size do not grow uncontrollably under sustained load.
- All long-running tasks are executed through an observable async pipeline with clear user-facing status.
