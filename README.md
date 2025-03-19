# rave-pipelines

RAVE 2.0 interactive modules and data pipelines for reproducible analysis and visualization of intracranial electroencephalography. This repository consists of built-in RAVE (https://rave.wiki/) modules and pipelines; currently contain the entire preprocessing pipelines. All presented as interactive modules. 

### Why RAVE 2.0 instead of 1.0

RAVE 1.0 has the following limitations:

* Type of data to load is limited to power, phase, voltage. Custom data loading is complicated.
* The analysis code is often entangled with UI code, making it hard to just run the pipeline scripts offline (without GUI)
* The default input types are limited, the inputs UI can become very complicated when non-default arrangement is needed (which is often the case)
* The modules are designed to run simple analysis code. Users often need to run the whole pipeline in order to get results, even though partial pipeline code is needed.

The motivation of RAVE 2.0 is to overcome these limitations. A RAVE 2.0 pipeline module consists of two parts: 

1. Data pipeline: well structured scripts that runs pipeline without graphical user interface;
2. UI module: `R-Shiny` application providing user interface. Users set the inputs from the UI, the module drives data pipeline, and visualize the pipeline results back to users.

Features include:

* Separate pipeline scripts and interactive UI modules. Users can run reproducible pipeline code without UI. This can speed up and scale up the pipeline when no UI is needed
* Module authors have 100% control over the UI code. They can choose to load any type of data, even for data not defined in RAVE
* The pipeline can run partially. 
* All the intermediate key variables will be saved and read easily
* Supports `Python`

## Selected Screenshots

| Notch Filter | Wavelet | Epoch Generator |
|:------------:|:-------:|:---------------:|
|<img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225415783-5a16f3d8-44a1-4add-a0ab-05805be71893.png">  |  <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225415828-8f3cacec-1080-4ea1-b106-fe0d0933c904.png">  | <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225416445-cac2d4dd-53ae-4603-bea7-43be155050ca.png"> |



| Channel Reference | CT & MRI Co-registration | Electrode Localization |
|:-----------------:|:------------------------:|:----------------------:|
| <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225416639-c140d3c1-e1ac-490b-93a0-4e876af66340.png"> | <img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225417282-936787a1-9030-4152-bb83-b98401ac5688.png">|<img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/225418490-dc6caa68-c477-4c5d-a90c-159a35ad815d.png">|

## How to use

Check the [installation guide](https://openwetware.org/wiki/RAVE:Install) to install `RAVE`. The pipeline will be downloaded automatically during installation/upgrade.

## Make contribution

This repository forbids directly push. All changes must be made via pull-requests. Please fork this project and edit on your own before creating pull-requests. "At" our team members or join our [slack channel](mailto:slack@rave.wiki) to request for supports.

### 1. A taste of RAVE data pipeline


Before digging into each file, let's take a look on how to run RAVE [notch filter](modules/notch_filter) without GUI:

```r
# Load pipeline
pipeline <- ravepipeline::pipeline("notch_filter")

# Set inputs
pipeline$set_settings(project_name = "devel", subject_code = "DemoSubject")

# Run pipeline
pipeline$run("apply_notch")
#> • start target settings_path
#> • built target settings_path [0 seconds]
#> • start target settings
#> • built target settings [0 seconds]
#> • start target notch_filter_lowerbound
#> • built target notch_filter_lowerbound [0 seconds]
#> • start target notch_filter_upperbound
#> • built target notch_filter_upperbound [0 seconds]
#> • start target subject_code
#> • built target subject_code [0.001 seconds]
#> • start target project_name
#> • built target project_name [0.001 seconds]
#> • start target filter_settings
#> • built target filter_settings [0 seconds]
#> • start target subject
#> • built target subject [0.003 seconds]
#> • start target imported_electrodes
#> • built target imported_electrodes [0 seconds]
#> • start target apply_notch
#> [Applying Notch filters]: Results collected                                                   
#> • built target apply_notch [1.888 seconds]                                                    
#> • end pipeline [1.967 seconds]

# Visualize pipeline
ravepipeline::pipeline_visualize(pipeline$pipeline_path)
```

<img width="80%" alt="image" src="https://user-images.githubusercontent.com/8163576/226499317-0e674b33-c854-4c1b-98b6-799302451094.png">


In 4 lines of R code, we load the pipeline, set inputs, apply the notch filters to subject `devel/DemoSubject`, and visualize the pipeline structure! Besides, all the intermediate data, such as `imported_electrodes`, `subject`, `filter_settings`, ... are stored and ready to be loaded:

```r
pipeline$read(c("subject", "filter_settings"))
#> $subject
#> RAVE subject <devel/DemoSubject>
#> 
#> $filter_settings
#> $filter_settings$lb
#> [1]  59 118 178
#> 
#> $filter_settings$ub
#> [1]  61 122 182
#> 
#> $filter_settings$domain
#> [1] 1
```

### 2. How does RAVE pipeline work

Most data analysis scripts contain the following componets:

- Code to load external libraries, utility functions
- Set inputs, global data
- Run analyses and generate outputs, for example, data loading & cleaning, intermediate analysis, data & graphs export

RAVE data pipeline manages these three componets separately (see figure below). 

<img width="100%" alt="image" src="https://user-images.githubusercontent.com/8163576/226494805-f7004824-9392-441b-97e4-a3c70835fe1e.png">
<caption>Structures of RAVE 2.0 pipeline (right) and how RAVE uses the pipeline files to generate analysis script (left)</caption>

* `R/shared-xxx.R` loads external libraries and defines reusable functions (optional)
* `settings.yaml` defines the input variables
* `main.Rmd` defines the analysis code

The RAVE internal workflow can be roughly described as: `Load shared functions -> load settings.yaml as inputs -> run main.Rmd`.

Using [`modules/notch_filter`](modules/notch_filter) as an example,

* [`R/shared-diagnose_plot.R`](modules/notch_filter/R/shared-diagnose_plot.R) defines a reusable function `diagnose_notch_filters`
* [`settings.yaml`](modules/notch_filter/settings.yaml) specifies which RAVE subject to load and notch filter parameters (`notch_filter_lowerbound` and `notch_filter_upperbound`)
* [`main.Rmd`](modules/notch_filter/main.Rmd) contains R code blocks. Each code block starts with ` ```{rave ...} `

In the previous example script, 

1. When we run `pipeline <- ravepipeline::pipeline("notch_filter")`, RAVE collects the pipeline directory information and wrap it in the variable `pipeline`.

2. `pipeline$set_settings(project_name = "devel", subject_code = "DemoSubject")` alters `project_name` and `subject_code` in the `settings.yaml`.

3. `pipeline$run("apply_notch")` analyzes code dependence within `main.Rmd`, and ONLY runs the code blocks needed to generate variable `apply_notch`. (if you check the dependence graph, `diagnostic_plot` is not needed, hence not evaluated.

4. `ravepipeline::pipeline_visualize(pipeline$pipeline_path)` displays the dependency graph. Each node stands for an input in `settings.yaml` (e.g. `project_name`) or a code block in `main.Rmd` (e.g. `apply_notch`). 

### 3. Write your own pipeline

_*(This sub-section might be ambiguous. Please ask the RAVE team to schedule a walk-through.)_

To start, open `rave-pipeline` project in `RStudio`, and enter the R command (please change `module_id` and `module_label` accordingly)

`ravepipeline::module_add(module_id = "my_module_id", module_label = "My Module Label")`

A sample module pipeline folder will be created under `modules/` named after the specified `module_id`.

#### `settings.yaml`

A `settings.yaml` is just a collection of key-value pairs. Module users are only allowed to set those inputs, hence consider carefully which inputs you want the users to alter. I recommend the RAVE function `raveio::save_yaml` to programmically write to this file to avoid `yaml` syntax errors. 

#### `main.Rmd`

`main.Rmd` is an [R-markdown file](https://rmarkdown.rstudio.com/lesson-2.html) that interleaves code (starting with ` ``` `) and descriptive paragraphs. RAVE does not care about descriptive paragraphs. You can remove them for now. 

The first block should look like this:

````r
```{r setup, include = FALSE}
# This code block sets up the engine environment
# Please do not remove me
ravepipeline::pipeline_setup_rmd("my_module_id")
```
````

This is a special block that loads the inputs from `settings.yaml` to the memory for debug use. For example, if you set `project_name` to `"demo"` in `settings.yaml`, you can type `project_name` in the R console and it should return value `"demo"`. 

The subsequential code blocks start with ` ```{rave ...} `. Each code block defines a pipeline "target" (a key intermediate variable). For example the following block defines a target variable `subject`.

````r
```{rave load_subject, language = "R", export = "subject", format = "rave-subject", cue = "always"}
subject <- raveio::RAVESubject$new(
  project_name = project_name, 
  subject_code = subject_code,
  strict = FALSE
)
```
````

* ` ```{rave ` means this block is part of pipeline (all RAVE pipeline targets might start with it
* `load_subject` describes the goal of this block. Use `_` to connect words
* `language = "R"` use R to run this block (alternative is `Python`, ask the RAVE team for more information. We will add instructions in the future)
* `export = "subject"` tells RAVE that this block will generate a variable `subject`
* `format = "rave-subject"` this is a special case, will talk about it later. You can ignore for now
* `cue = "always"` tells RAVE do not skip this code block, will talk about it later. 

Within the ` ``` `, the script asks RAVE to generate a variable `subject` using `project_name` and `subject_code`. These variables are "undeclared" for this block (if you directly run this block, you will get variable not found errors). 

Where do these undeclared vairables come from? The answer is they come from previous "targets" or from `settings.yaml`. In this case, `project_name` and `subject_code` are defined in `settings.yaml`.

(TODO: add more)

The last two code blocks should look like

````r
```{r build, echo=FALSE, results='hide'}
build_pipeline(make_file = "make-electrode_localization.R")
```

```{r visualize, echo=FALSE}
Sys.setenv("RAVE_PIPELINE" = normalizePath("."))
ravepipeline::pipeline_visualize()
```
````

Please leave them AS-IS. Now let's compile the pipeline script by pressing the following button:

<img width="100" alt="image" src="https://user-images.githubusercontent.com/8163576/226508232-33db252d-d61c-45e5-b604-9e8d5bdec551.png">

RAVE will compile and translate the script into `module/my_module_id/make-electrode_localization.R`. You should be ready to run the pipeline now.

(TODO: add instruction)

**Caveats**

1. Each code block generates only one variable. RAVE pipeline will drop all the other temporary variables when exiting current block. Therefore, all the undefined variables must come from previous targets or pipline inputs only
2. A code block does not need to end with the target variable. However, the target variable must be declared within the block.

Take a look at the following case (containing 3 blocks). Suppose `project_name` and `subject_code` are defined in `settings.yaml`. Two of three contain errors.

````r
```{rave load_subject, language = "R", export = "subject", format = "rave-subject", cue = "always"}
subject <- raveio::RAVESubject$new(
  project_name = project_name, 
  subject_code = subject_code,
  strict = FALSE
)
subject_id <- subject$subject_id
message("Loading subject: ", subject_id)
```

```{rave load_subject_power_data, language = "R", export = "repository", cue = "always"}
repository <- raveio::prepare_subject_power(subject_id)
```

```{rave calculate_baseline, language = "R", export = "baseline", cue = "always"}
raveio::power_baseline(repository, c(0,0.5))
```
````

* The first block does not end with variable `subject`, but it generates a variable `subject` within the code. There are two undefined variables for this block: `project_name` and `subject_code`, but they are defined in `settings.yaml`. Therefore, this block is valid.
* The second block contains `repository`. However the undeclared variable `subject_id` is temporary (generated in previous target `subject`). Since RAVE drops this variable when exiting previous block, this variable is unavailable, hence error will be raised. A valid modification will be:

```{rave load_subject_power_data, language = "R", export = "repository", cue = "always"}
repository <- raveio::prepare_subject_power(subject)

# or repository <- raveio::prepare_subject_power(subject$subject_id)
```

* The third block has an undeclared variable `repository`, which is generated by the second block. However, RAVE does not know which variable is `baseline`. You need to explicitly specify it. A revision will be

````r
```{rave calculate_baseline, language = "R", export = "baseline", cue = "always"}
raveio::power_baseline(repository, c(0,0.5))
baseline <- repository$power$baselined
```
````













