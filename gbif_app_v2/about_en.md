**Biodiversity Viewer** is an online tool that provides quick and convenient access to data on registration of species subject to special protection in accordance with Ukrainian legislation and published on the [GBIF](https://www.gbif.org/). It is open source software distributed under the [*Creative Commons Attribution (CCA-BY) 4.0*](https://creativecommons.org/licenses/by/4.0/deed.en) license, so users can deploy it on their own server or local computer, modify or adapt for any other country.

# What is GBIF?
GBIF is an intergovernmental, supranational organization whose purpose is to aggregate data on biological diversity and ensure free and open access to it. It is managed by representatives of the participating countries and is filled with by "data publishers" - scientific institutions, educational institutions, public organizations, community science projects, etc. You can learn more about the vision, organization and structure of GBIF using the following [link](https://www.gbif.org/uk/what-is-gbif).

# What GBIF is not?
GBIF is not an entire digital footprint of all human knowledge, but only reflects what the data publishers have published. Therefore, GBIF, like any other database, should not be considered as comprehensive source of information about the biological diversity of a certain terrestrial or water area. The absence of records of a certain species for a certain site in the GBIF database cannot be considered evidence of the lack of information about this species at this site, and even more so as evidence that "this species does not exist here".

# How to use the tool?
Biodiversity Viewer interface consists of 4 main tabs that guide the user through the stages of forming a query to the GBIF database, obtaining data, their initial processing and creating reports. You should switch to the next tab only after you have performed an action on the current one.

Start operation - selection of the area of interest. The user can choose an administrative unit (up to the level of an amalgamated territorial community), draw an area on a map or use his own polygonal geospatial file in .kml or .kmz formats, containing a polygon with the boundaries of the target area of interest. If needed, the user can set a buffer around the selected area of interest. Only after defining the search an area will the "Get GBIF data" button become available, clicking on which will download a sample of data for the selected area. The results of all actions are immediately displayed on the map in the viewer.

After loading the data, the user can go to the next tab - "Filter data" - which allows you to configure the set of protected lists, species of which are of interest to the user. When setting filters, the logic "all by default" is used. That is, to get only a part of the data (for example, only species from Resolution 6 of the Bern Convention), the user needs to clear all filters using the "Deselect all" button, and then put the filter sign opposite the name of the needed convention or list. Regional red lists are an exception (none is selected by default).

After clicking the "Apply filters" button, only those points that indicate records that meet all the specified conditions remain in the map area. The user can go to the following tab "Preview of the data table", where you can view the data in the form of an interactive table (divided into pages for easier display), or download the data table to the PC in the form of .csv or .xlsx files.

The last tab - "Generating of reports" - provides summarized (automatically created) information on GBIF data in the selected area of ​​interest, namely the total number of species and their status according to chosen conservation lists. On this page, the user can also download summary information in .html (for viewing in web browsers) or .docx (for further processing in word editors such as Microsoft Word) formats.

# How NOT to use the tool?
The tool is primarily focused on solving the problems of practical nature conservation and wildlife management. For scientific research, we recommend contacting the portal [gbif.org](https://www.gbif.org/) directly, or use such tools as [rgbif](https://www.gbif.org/uk/tool/81747/rgbif) or [pygbif](https://www.gbif.org/uk/tool/OlyoYyRbKCSCkMKIi4oIT/pygbif-gbif-python-client), in addition to the information from other databases, biological collections and scientific publications.

It is recommended to refrain from trying to download data for very large areas (for example, multiple areas) - this will slow down the tool and may cause it to crash due to exhausting system resources.

It is worth noting that people make mistakes. Observations published on GBIF and available through our tool may have incorrect identification, geolocation, date or other associated information. Biodiversity Viewer is in no way responsible for the quality and reliability of data published on GBIF. If you find an error in the data, you should follow the link to the dataset opposite the corresponding record and contact the authors of the dataset directly through the contacts indicated in the metadata (description) of the specific dataset.

Most importantly, Biodiversity Viewer is only an automated tool that saves the user time and effort spent on searching, downloading and organizing data. In no case should it be taken as a substitute for a specialist. Data obtained with the Biodiversity Viewer must be interpreted and summarized with full awareness of one's own responsibility for the final result.

# Frequently asked questions and solutions to problems
> **I can't access the tool from the link.**

There can be two reasons: server overload due to a large number of users connecting at the same time, and a technical failure. Try to check in later. If that doesn't help, [inform us](https://github.com/ABiatov/gbif_shiny_onlineviewer/issues), making sure to indicate your web browser, the date and time of the failed attempt, and describe the error signs or messages you see on the screen.

> **At a certain stage, the tool "hangs" and does not respond to commands.**

The selection of records by spatial query requires time for processing, which is proportional to the size of the site and the number of records falling on it. For areas the size of an administrative district, it usually takes a few seconds, but for large samples it can be significantly longer. Try to reload the page, select a smaller area, and retry the request.

> **The tool refuses to work with my polygon.**

To form a correct sample of records by polygonal area (especially in situations when the user loads his/her geospatial file), the polygon must not contain any geometry errors. Most often, such errors occur when manually creating complex contours, when the boundary line of the polygon crosses itself. If you have problems with your polygon, we recommend that you first check it with a topology checker, for example, in a free GIS tool [QGIS](https://qgis.org/uk/site/).

> **I don't see some of the records I know for the selected area.**

The tool does not reflect all of humanity's knowledge, but only those findings that are published on GBIF. Moreover, registrations of the same biological species could be published under different names (synonymous). Although GBIF has a built-in synonym resolution tool, for some taxa we had to resolve nomenclatural issues manually, so it may happen that an individual record "didn't catch on". If you noticed that the records of a certain species, which is included in the conservation lists valid on the territory of Ukraine, are available for the selected region on the gbif.org portal, but are not displayed in the Tool,[inform us](https://github.com/ABiatov/gbif_shiny_onlineviewer/issues).

Not every species record available through GBIF has precise coordinates. Among others, GBIF includes data points from atlases that were positioned according to the regular grid. Some iNaturalist-derived observations have obscured coordinates due to licencing or threatened-species policy. Such low-precision occurrences are excluded from the Biodiversity Viewer.

It's also worth noting that Biodiversity Viewer doesn't directly access GBIF every time a user makes a request (in such case each request would take a very long time), instead periodically indexing GBIF and doing a temporary download. If a find was published after the tool's data was last updated, it will be temporarily unavailable through the tool unless the updated download is done.

> **Wrong species name (out of date, synonym, etc.)**

We used those species names that are used in the selected conservation lists. Higher taxonomy (class, kingdom) is also indicated as presented in the relevant documents.

> **For my area of interest, a lot of records of "common" and not so rare species are shown.**

We form loading for the species specified in the relevant conservation lists valid on the territory of Ukraine, and are in no way responsible for the formation of these lists.

> **How to cite Biodiversity Viewer in a correct manner?**

Please use the following form to cite to the tool:
Biodiversity Viewer: an open web-based biodiversity conservation decision-making tool for policy and governance. Joint project of The Habitat Foundation and Ukrainian Nature Conservation Group, supported by NLBIF: The Netherlands Biodiversity Information Facility, nlbif2022.014 - Access date <your-date-of-accessing-the-tool>.


# Information for developers
Biodiversity Viewer is a Shiny-app, that interacts with the GBIF database through its [API](https://techdocs.gbif.org/en/openapi/), with the help of tools from the pack [rgbif](https://www.gbif.org/uk/tool/81747/rgbif). The source code is posted on [Github](https://github.com/ABiatov/gbif_shiny_onlineviewer/), where you can also find technical documentation.

# Contact us
If you want to report a bug or suggest an improvement, leave a message in the section [Issues](https://github.com/ABiatov/gbif_shiny_onlineviewer/issues) (registration on Github is required). 

**Sincerely, the development team of Biodiversity Viewer**
