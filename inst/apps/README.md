# YAML-based PICTURE App Creation
This README explains how to assemble PICTURE applications as YAML files.

## Top Level Structure
| Parameter | Description | Example |
| --------- | ----------- | ------- |
| title | A short sentence title for the application. This is used to label the application in the PICTURE homepage. | Informatics consult for patients newly diagnosed with a muscular dystrophy |
| description | A description of the application's purpose/use case. This description is shown in the PICTURE homepage. | A set of analytics information to inform a clinician when interacting with a patient diagnosed with muscular dystrophy |
| creator | Email address for the person who created the analysis | mytrust@gosh.nhs.uk |
| img | A file description for an image to be shown with the application on the PICTURE homepage. File paths are relative to the R package root. | www/images/Neuron Full Colour-01.png |
| dataset | The base dataset to use for the analysis. This should be the name of the folder with the data to load | study01 |
| initialCohorts | YAML description for the initial/default set of patient cohorts used to build the analysis | [See section](#initialcohorts) |
| offerCohortBuilder | Flag to define if the application should allow the user to modify and define cohorts with the interactive cohort builder | true |
| analysis | YAML description for the analytical methods that PICTURE should apply to the cohort datasets | [See section](#analysis) |
| outputs | YAML description for the output format that PICTURE should use to present the results | [See section](#outputs) |

## initialCohorts
The initialCohorts element describes a list of cohorts that can be derived from the base dataset. For each cohort in the list, they must have the following two parameters:

| Parameter | Description | Example |
| --------- | ----------- | ------- |
| label | A label for the cohort, used throughout the analytics to identify the results for this specific cohort. | Females |
| config | YAML description for the cohort. | [See section](#initialcohortsconfig) |

### initialCohorts.config
The configuration of each cohort is defined by a list of selection filters applied to the EPR data. Each filter in the sequence is applied in order, such that each removes/splits the patients.

| Parameter | Description | Example |
| --------- | ----------- | ------- |
| type | The type of processing to apply. Currently, only `filter` is supported. | filter |
| rdv | The `rdv_code` for the RDV to use for filtering. The list of available RDVs and their code is [here](../configs/rdv_code_lookup.csv). | pde |
| column | The column from `rdv` that should be queried against for the filter. To identify available columns you will need to look at the RDVs themselves. | sex_name |
| query_type | This defines how to query the selected RDV column. The available query types are described [here](#query-types). The code implementing the queries is [here](../../R/utils_segment_cohorts.R). | str_matches |
| val | This is the value that the column is queried against. The input value depends on the `query_type`. The required values for each query type are described [here](#query-types). | Female |
| inclusion | This defines how the inclusion criteria are applied to the patients based on the filter. | ever |
| window | (optional) This defines a window either side of the inclusion criteria to select data from, in days. | [-7, 7] (e.g. 7 days before till 7 days after) |

In the above table, the example configuration can be read as filtering the patient list so that only patients who `ever` have a `sex_name` in the `pde` RDV that `str_matches` the value `Female` will be kept.

### Query types
| `query_type` | Description | `val` |
| ---------- | ----------- | --- |
| str_matches | The string value of the selected column must exactly match the string given in `val` | String value to match in the query column |
| str_starts | The string value of the selected column must start with the substring given in `val` | String value to start the query column |
| str_contains | The string value of the selected column must contain the substring given in `val` | String value to occur query column |
| date_between | The numerical value of the selected column (possibly a date or datetime) must be between the two input `val` | A list of two numerical values the query column value must be between. (Note, if you are using datetimes, you will need a `!datelist` tag in the YAML, see [here](./gosh_start_well.yaml) for an example.) |
| numeric_between | The numerical value of the selected column must be between the two input `val` | A list of two numerical values the query column value must be between.|
| age_between | The patient must be aged between the to input `val`. This is a special case and uses additional logic to filter so that only patient events that happened when the patient was within the required age range are kept. | A list of two numerical values the age must be between. |

### Inclusion types
| `inclusion` | Description |
| ---------- | ----------- |
| ever | If a patient ever has an event that matches the supplied filter, all of their data will be kept in the dataset. <br>e.g., if a filter says ever diagnosis of asthma, all data for any patient who has ever had a diagnosis of asthma (even if years ago and only once) will be retained. |
| never | If a patient never has an event that matches the supplied filter, all of their data will be kept in the dataset. <br>e.g., if a filter says never diagnosis of asthma, all data for any patient who has ever had a diagnosis of asthma (even if only once) will be dropped. |
| fully_concurrent | Only data that occur concurrently with the filter event will be kept in the dataset. <br>e.g., if a filter says fully_concurrent with a PICU admission, only data occurring between the start and finish of a patient's PICU admission will be retained. If a patient had no PICU admissions, all data will be dropped. |
| after_first | Only data that occur after the first occurrence of a filter event will be kept in the dataset. <br>e.g., if a filter says after_first a PICU admission, only data occurring after the start of a patient's PICU admission will be retained. If a patient had no PICU admissions, all data will be dropped. |
| on_first | Only data that occur exactly at the same time as the first event. <br>e.g., if a filter says on_first a PICU admission, only data occurring during the patient's first PICU admission will be retained. If a patient had no PICU admissions, all data will be dropped. |

## analysis
The analytics methods used to provide a PICTURE analysis are grouped into tabs. Each tab given an entry in the navigation bar on the left-hand side of PICTURE. Each tab in the list has two parameters:

| Parameter | Description | Example |
| --------- | ----------- | ------- |
| tab | A name for the tab, shown in the navigation bar. You should keep these short to avoid overruns and odd formatting. | Demographics |
| methodList | YAML description for the cohort. | [See section](#analysismethodlist) |

### analysis.methodList
The method list contains a list of analytics methods that are each shown in sub-tabs. PICTURE dynamically generates function calls based on the two inputs defined here. The two inputs are:

| Parameter | Description | Example |
| --------- | ----------- | ------- |
| fn | The `r` function name for the analytics method to apply to the input cohorts. Details are given [here](#fn). | gen_frequency_analysis |
| params | YAML defining a named list of parameters that are passed to `fn` to apply an analysis. Details are given [here](#params). | params:<br> $~~~$ df_rdv: df_pde |
| tab_lbl | (Optional) Label for the tab that wraps the module | Common diagnoses |
| output | (Optional) Output reactive name to store any returned reactive against. This is useful for pipelines | df_lab_wrangled |

### fn
The available analytics functions are in the `r` directory, e.g., `gen_frequency_analysis`, `gen_event_time_analysis`, etc. PICTURE dynamically calls the functions to run/display the analysis by combining `fn` with with either:
 - `_ui()` to run a shiny ui function (e.g., `gen_frequency_analysis_ui()`)
 - `_server()` to run a shiny server function (e.g., `gen_frequency_analysis_server()`)
 - `_report()` to generate a rmarkdown/pdf report (e.g., `gen_frequency_analysis_report()`)

 Some template analyses (i.e., groups of low-level analyses commonly used together) are available in the `r` directory with the prefix `tpl_`. These can use used in the same way as the individual analyses.

### params
For the `ui` functions, no parameters are used.

For the `server` and `report` functions, the function calls are populated with the parameters included in the `params` field.

For the direct value parameters (e.g., `event_col: "proc_name`) these parameters are directly assigned based on the value in the YAML.

For the RDVs (e.g., `df_rdv: df_pro_opcs`), these are dynamically populated  at runtime to supply the reactive to the `server` and the RDV value to the `report`.

Note: PICTURE will automatically create `id` values to setup the namespaces for the shiny modules. You do not need to include these specifically.

## outputs
The PICTURE output can be presented to the user as both an interactive webapp and a PDF report. The output parameters control this:

| Parameter | Description | Example |
| --------- | ----------- | ------- |
| interactive | Whether to show the interactive UI webapp. Currently, this is not supported and is always `true`. | true |
| pdf | Whether to generate and show the output PDF report. | true |
