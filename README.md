# surveyUtils

A comprehensive R package for streamlined survey research data processing, from SurveyMonkey API integration to cleaned datasets ready for analysis.

🚧 MVP. More to come!

## Overview

surveyUtils provides a complete workflow for survey research data processing. The package centers around an **intelligent data codebook system** that serves dual roles as both documentation and processing configuration, enabling automated data cleaning, scoring of standardized psychological instruments with subscales, and comprehensive quality control checks.

Built on production-level R practices, surveyUtils demonstrates data pipeline architecture with institutional knowledge management, making it particularly valuable for teams conducting repeated survey research studies.

## Key Benefits

- ⚡ **Faster project setup** - From hours to minutes
- 🎯 **Consistent workflows** - Standardized processes across team members, with easy reference parameters files 
- 🔄 **End-to-end automation** - Complete pipeline from raw SurveyMonkey extracts to analysis-ready datasets  
- 📋 **Intelligent codebook system** - Self-documenting data processing with institutional memory through maximal codebook library  
- ⚡ **Standardized instrument scoring** - Automated scoring for scales, with subscale support  
- ✅ **Quality control built-in** - Attention checks, straightlining detection, duration filtering, and response validation  
- 🔗 **API integration** - Direct SurveyMonkey downloads with caching
- 🏗️ **Modular architecture** - Reusable functions following tidyverse conventions and DRY principles  
- 📊 **Analysis-ready outputs** - Wrangled data with comprehensive codebook documentation
- 🔒 **Secure data handling** - Token-based authentication (and future Box integration for sensitive research data)

## System Architecture

```
surveyUtils/
├── external/
│   └── surveymonkey_utils.R       # SurveyMonkey API communication & data download
│   └── box_utils.R                # Secure cloud storage integration [Coming Soon]
├── data/
│   ├── data_codebook_utils.R      # Intelligent codebook generation & management
│   ├── data_wrangling_utils.R     # Data cleaning & processing
│   ├── survey_scoring_utils.R     # Standardized instrument scoring with subscales
├── visualization/
│   ├── plot_utils.R               # Presentation-ready visualizations
│   └── table_utils.R              # Presentation-ready tables [Coming Soon]
└── core/
│   └──  workflow_utils.R           # High-level workflow orchestration [WIP]
└── config/
│   └── parameters_template.R      # Parameters file for one-stop configuration
```

## Table of Contents

- [Overview](#overview)
- [Key Benefits](#key-benefits) 
- [System Architecture](#system-architecture)
- [The Data Codebook System](#the-data-codebook-system)
- [Core Modules](#core-modules)
  - [SurveyMonkey API Integration](#surveymonkey-api-integration)
  - [Data Codebook Management](#data-codebook-management)
  - [Data Wrangling Pipeline](#data-wrangling-pipeline)
  - [Survey Scoring Engine](#survey-scoring-engine)
  - [Workflow Orchestration](#workflow-orchestration)
  - [Cloud Storage Integration](#cloud-storage-integration)
  - [Visualization Tools](#visualization-tools)
- [Getting Started ](#getting-started)


## The Data Codebook System

The data codebook is the central organizing principle of surveyUtils, serving two critical functions:

### Dual Role Codebook
1. **Documentation**: Complete metadata for all variables including question text, response scales, and survey instrument information
2. **Processing Configuration**: Machine-readable instructions for data cleaning, reverse coding, instrument scoring, and response format conversion

Captures survey metadata via SurveyMonkey API + integrates information from past studies to minimize manual completion.

### Codebook Structure

| Column | Description |
|--------|-------------|
| `col_num`  | Column number for ordering |
| `question_text` | Full question text as presented to participants |
| `variable_name` | Original SurveyMonkey variable name (snake_case cleaned) |
| `short_variable_name` | Concise analysis-friendly variable name |
| `variable_name_R` | Variable name with _R suffix for reverse-coded items |
| `cat` | Variable category (demo, attention_check, survey_items, computed_scores) |
| `scale_abbrev` | Short scale name (phq, gad, pss, etc.) |
| `scale_full` | Complete standardized instrument name |
| `subscale` | Subscale designation for multi-factor instruments |
| `coding_direction` | 1 for forward coding, -1 for reverse coding |
| `min` / `max` | Valid response range for validation and scoring |
| `response_choices` | Available response options (from SurveyMonkey API) |
| `response_coding` | Numeric coding for response choices |
| `response_format` | Format for conversion ("numeric", "text", etc.) |
| `correct_response` | Correct answer for attention check items |
| `required` | Whether question was required in survey |
| `question_format` | Question type (single_select, multi_select, matrix, etc.) |
| `question_id` | SurveyMonkey question ID |
| `page_number` | Survey page number |
| `family` / `subtype` | SurveyMonkey question family and subtype |
| `matrix_row_id` / `matrix_row_text` | Matrix question row identifiers |

### Enhanced Features
- **Maximal Codebook Library**: Institutional knowledge system that automatically matches questions to known instruments using exact variable name matching and fuzzy text matching
- **Intelligent Question Recognition**: Two-stage matching process - exact matches first, then fuzzy matching for remaining questions
- **Template Generation**: Automated codebook creation from SurveyMonkey metadata
- **Response Format Specification**: Supports text-to-numeric conversion for standardized response scales
- **Computed Score Tracking**: Automatically adds total and subscale score variables to codebook after scoring

---

## Core Modules

### SurveyMonkey API Integration
**File:** `surveymonkey_utils.R`

Handles secure communication with SurveyMonkey API for data retrieval and survey structure analysis.

**Key Functions:**
- `load_sm_token()` - Secure OAuth token management
- `fetch_surveys_sm()` - Retrieve survey list with intelligent caching
- `download_responses_sm()` - Download survey responses with pagination handling
- `fetch_survey_structure_sm()` - Extract detailed survey metadata
- `flatten_survey_responses()` - Convert nested API data to tabular format
- `create_double_headers()` - Mirror SurveyMonkey export format
- `process_survey_responses()` - Complete response processing pipeline


### Data Codebook Management
**File:** `data_codebook_utils.R`

Generates and maintains intelligent codebooks that combine API metadata with institutional knowledge.

**Key Functions:**
- `generate_basic_codebook_template()` - Create codebook from SurveyMonkey metadata
- `generate_enhanced_codebook_template()` - AI-enhanced codebook with automatic scale matching
- `load_maximal_codebook()` - Access institutional knowledge library
- `match_questions_to_codebook()` - Fuzzy text matching for question identification
- `update_maximal_codebook()` - Continuously improve knowledge base
- `setup_new_survey_project()` - Complete project initialization workflow


### Data Wrangling Pipeline
**File:** `data_wrangling_utils.R`

Comprehensive data processing pipeline with quality control and validation.

**Key Functions:**
- `process_double_headers()` - Handle SurveyMonkey's dual header format
- `apply_short_names()` - Apply concise variable names from codebook
- `score_attention_checks()` - Automated attention check validation
- `filter_by_date_range()` - Remove test/QA responses
- `calculate_survey_duration()` - Compute completion times with validation
- `reverse_code_from_codebook()` - Automated reverse coding
- `create_multiselect_summary_cols()` - Generate readable multi-select summaries
- `process_survey_data()` - Complete processing pipeline orchestration


### Survey Scoring Engine
**File:** `survey_scoring_utils.R`

Automated scoring of standardized psychological instruments with validation.

**Key Functions:**
- `var_score()` - Core scoring function with prorating and validation
- `load_data_dictionary()` - Validation and loading of scoring configurations
- `get_survey_config()` - Extract instrument-specific scoring parameters
- `score_survey()` - Score individual instruments
- `score_surveys()` - Batch scoring with error handling
- `generate_scoring_report()` - Comprehensive scoring validation report


### Workflow Orchestration
**File:** `workflow_utils.R`

High-level functions that coordinate complex multi-step research workflows.

**Key Functions:**
- `process_sm_survey_complete()` - Complete SurveyMonkey download and setup
- `setup_new_research_project()` - Create standardized project structure
- `complete_survey_processing_pipeline()` - End-to-end processing coordination


### Cloud Storage Integration
**File:** `box_utils.R`

Secure integration with Box for sensitive research data management.

**Key Functions:**
- `upload_survey_to_box()` - Secure upload of processed datasets with versioning


### Visualization Tools
**File:** `plot_utils.R`

Research-focused plotting functions with consistent styling for publications.

**Key Functions:**
- `make_hist()` - Publication-ready histograms
- `make_scatter()` - Scatterplots with regression lines
- `make_group_scatter()` - Group comparison visualizations
- `make_group_time_plot()` - Longitudinal data visualization


## Getting Started
1. Set up an app within your Survey Monkey account: https://developer.surveymonkey.com
- Enable Scopes:
    - View Surveys
    - View Collectors
    - View Responses
    - View Response Details
    - View Webhooks
- Save Access Token to a csv file under a single column titled 'access_token'. Keep this file and token secure. Paste the path to the csv into creds_path in the parameters file.
```r
# Load your survey-specific parameters *this should be the only line you have to edit*
source("config/parameters.R")

# Load utilities (sources all utils scripts)
source(params$utils_path)

# 1. Setup authentication
token <- load_sm_token(params$creds_path)

# 2. Pull responses data from SurveyMonkey via API
df_raw <- process_survey_responses(
  token, 
  survey_name = params$survey_name,
  save_csv = params$save_raw_csv, 
  output_path = params$raw_dir
)

# 3. Generate enhanced codebook with institutional knowledge
codebook <- generate_enhanced_codebook_template(
  token,
  survey_name = params$survey_name,
  output_path = params$config_dir,
  maximal_codebook_path = params$maximal_codebook_path,
  fetch_new_metadata = params$fetch_new_metadata
)

# 4. Fill in any missing scale information in generated codebook CSV
# Edit: config/[survey_name]_generated_enhanced_codebook.csv

# 5. Complete data processing pipeline
df_processed <- process_survey_data(
  df_raw, 
  codebook, 
  survey_name = params$survey_name,
  score_surveys = params$score_surveys,
  output_path = params$processed_dir,
  start_date_col = params$start_date_col,
  end_date_col = params$end_date_col,
  min_date = params$min_date,
  export_attention_failures = params$export_attention_failures,
  remove_attention_fails = params$remove_attention_fails,
  straightline_threshold = params$straightline_threshold,
  remove_straightliners = params$remove_straightliners,
  update_codebook_with_scored = params$update_codebook_with_scored
)
```

### Processing Pipeline Steps

The `process_survey_data()` function orchestrates these steps automatically:

1. **Column name standardization** - Convert to snake_case, strip HTML
2. **Short name application** - Apply concise variable names from codebook
3. **Double header processing** - Combine question text and response choices
4. **Multi-select summaries** - Create readable summary columns
5. **Date filtering** - Remove test/QA responses before launch date
6. **Duration calculation** - Compute and validate survey completion times
7. **Attention check scoring** - Validate and optionally remove inattentive responders
8. **Response text mapping** - Convert text responses to numeric codes
9. **Numeric conversion** - Convert to numeric data type with validation
10. **Straightlining detection** - Identify and optionally remove straightliners
11. **Reverse coding** - Apply reverse coding based on codebook
12. **Survey scoring** - Score all standardized instruments with subscales
13. **Codebook updating** - Add computed score variables to codebook

This package demonstrates production-level R development practices including modular architecture, comprehensive documentation, intelligent caching, institutional knowledge management, and secure credential handling suitable for sensitive data.
