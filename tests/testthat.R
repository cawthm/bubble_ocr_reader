# Test runner for bubble_test_ocr_reader
# Run this file to execute all tests

library(testthat)

# Source all R modules
source("R/config.R")
source("R/data_io.R")
source("R/image_processing.R")
source("R/bubble_detection.R")
source("R/template_matching.R")
source("R/response_extraction.R")
source("R/grading.R")
source("R/pipeline.R")

# Load required packages
library(magick)
library(tidyverse)
library(data.table)
library(yaml)

# Run tests
test_dir("tests/testthat")
