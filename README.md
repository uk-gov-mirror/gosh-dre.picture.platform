# picture.platform - R package for core platform functionality of PICTURE

[`PICTURE (Personalised Paediatric Informatics ConsulTation using Real-world Evidence)`](https://adc.bmj.com/content/108/Suppl_1/A45.1) is a set of R packages that can be used to analyse data.

For more information or to engage with us, please email [DRESupport@gosh.nhs.uk](mailto:DRESupport@gosh.nhs.uk).

The development was based on datasets that follow the Research Data View (RDV) structure, a GOSH DRE data model.

Currently, it is required that all datasets that are loaded into PICTURE contain an identifier column 'project_id' and event datetime columns '*_datetime'.

The PICTURE platform allows the cohort-based analysis of 2-dimensional tabular data.
The platform is defined to be configurable and flexible so that one can run a range of analyses that address diverse clinical and operational questions.
Each analysis is considered as an 'app' that runs on the PICTURE platform.
To create a PICTURE app, it is only necessary to create a yaml file that pieces together existing PICTURE platform components.

In general, PICTURE analyses follow a standard flow:

1. Specify the data to be loaded on PICTURE and analysed.
2. Data are pre-processed by the [`picture_preprocessing`](https://https://github.com/gosh-dre/picture_preprocessing) library to convert CSVs to Parquet files so that they are quicker to load
3. PICTURE loads the base dataset
4. PICTURE splits the base dataset into one or more cohorts based on filters defined in the PICTURE app
5. PICTURE applies analytical methods to each cohort, as listed in the PICTURE app
6. PICTURE presents analytical results to the user as either an interactive dashboard or output report

### Installing PICTURE

PICTURE requires 3 repos to run [picture.platform](https://github.com/gosh-dre/picture.platform),
[driveanalytics](https://github.com/gosh-dre/driveanalytics), and [prepressr](https://github.com/gosh-dre/prepressr).
[picture_preprocessing](https://github.com/gosh-dre/picture_preprocessing) is used to pre-process CSV data into the right format for PICTURE.

Packages can be installed directly from GitHub
```
devtools::install_git("https://https://github.com/gosh-dre/picture.platform")
devtools::install_git("https://github.com/gosh-dre/driveanalytics")
devtools::install_git("https://github.com/gosh-dre/prepressr")
devtools::install_git("https://github.com/gosh-dre/picture_preprocessing")
```
Or if you clone the repo, enter the folder you can install it as a package.

```
devtools::load_all;devtools::install()
```


### Running PICTURE

To run the PICTURE platform with the default configuration and demo applications, use the following helper function:

```r
picture.platform::app_picture_run()
```


## Platform Developer Instructions

### Setup dependencies
As the package MUST include all of the dependencies in the DESCRIPTION file, we
are not using renv for this repository.
To install all of the dependencies, you
just need to run the first installation of it locally and it will automatically
install all of the missing required imports.

```r
devtools::install()
```

### Before running

Some externally available data is needed to run preprocessing:
- [The index of multiple deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)
- [ICD-10 codes](https://ftp.cdc.gov/pub/health_statistics/nchs/publications/ICD10CM/2019/)
- [Postcode Coordinates](https://geoportal.statistics.gov.uk/datasets/e832e833fe5f45e19096800af4ac800c/about) this will need to be summarised to disctict level
- [ Local Authority districts shape files](https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2020-boundaries-uk-buc/about)

### Running on Cloud Workspace

To run on some cloud workspaces the IP address and port may need to be specified in app_picture.R. This is done in `app_picture_server() options` with the following line:
```R
options = list(port = 8080, launch.browser = FALSE, host = '0.0.0.0')
```

Large apps with thousands of patients or a lots of analysis tabs, e.g. 20 tabs, may not load properly on a cloud system due to memory limitations. If this happens add `amount_of_data_to_load: limited` to the app yaml and specify the columns needed for each tab under `analysis_cols`. For example:

```yaml
- tab: Diagnoses
    methodList:
      - fn: gen_frequency_analysis
        rpkg: driveanalytics
        tab_lbl: "Diagnosis Frequency"
        params:
          df_rdv_server: df_dia_conditions
          df_pde_server: df_pde
          event_col:
            "Diagnoses": "diag_name"
          cohort_col: "cohort"
          analysis_cols:
          - project_id
          - diag_name
```

This means that instead of loading all data in the rdvs only the columns specifically needed for the app.

### App Building
#### App YAMLs
Instructions on writing an app YAML is [here](inst/apps/README.md).

### Dataflow

Before RDVs are loaded into PICTURE they are pre-processed with a separate [application](https://github.com/gosh-dre/picture_preprocessing).
Key variables are added, most importantly the ICD-10 hierachy and they are then converted into Parquet format which can then be loaded into PICTURE.

### Data
A dataset of dummy data has been included. The CSV files can be processed through `picture_preprocessing` and the parquet files can be run through `picture.plattorm`.

**NOTE:** all dummy data have been generated using an internally validated dummy data generation process that leverages real data in order to provide suitable contents and self-consistency for individual rows of data, but has no biologically meaningful information, nor any reference / link to individual specific patients.

### Latex for Reporting
```r
# Install latex compiler
tinytex::install_tinytex()
# Install packages
tinytex::tlmgr_install("tabu")
tinytex::tlmgr_install("montserrat")
tinytex::tlmgr_install("fancyhdr")
tinytex::tlmgr_install("ltablex")
tinytex::tlmgr_install("environ")
tinytex::tlmgr_install("wrapfig")
```

### Useful tools
```r
# Test load the package
devtools::load_all()
# Manually cause roxygen2 to run to populate `man/`
devtools::document()
# Add a new package to the DESCRIPTION file
usethis::use_package("ggplot2")
# Check lint free
lintr::lint_package(".")
# Run testthat tests
devtools::test()
# Package validity tests (includes testthat tests)
devtools::check()
# Test specific file
devtools::test_active_file()
# Automatically run tests when source files change:
devtools::auto_test()
```

### Important Notes
* Do not use `library()` (see [this](https://r-pkgs.org/namespace.html#search-path) for why)


## References
* [Packages in R book](https://r-pkgs.org/)
* [Shiny book](https://mastering-shiny.org/)
* [Bookdown book](https://bookdown.org/)

## Disclaimer
DISCLAIMER – this openly published repository represents a “major version” snapshot of a project that is not actively developed on Github.  Therefore, anyone using all or part of this repository must take responsibility for ensuring the use of up-to-date secure versions of relevant packages and code interpreters when running the code.  As stated in the license, the software is provided “as is” without any kind of warranty.   For more information, please refer to the license file in the repository.

## Licenses

All code is released under the [MIT License][mit]. All documentation is [© Crown copyright][copyright] (2025) and available under the terms
of the [Open Government License 3.0][ogl].

[mit]: LICENSE
[copyright]: http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/
[ogl]: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
