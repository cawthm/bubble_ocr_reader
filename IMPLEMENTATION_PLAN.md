# Implementation Plan - Bubble Test OCR Reader

## Phase 1: Project Setup & Cleanup

- [ ] **1.1** Create clean directory structure:
  ```
  bubble_test_ocr_reader/
  ├── R/                    # Core R modules
  ├── tests/                # Unit tests
  ├── config/               # Configuration files
  ├── keys/                 # Answer key files
  ├── data/                 # Reference data (bubble_map, roster)
  ├── input/                # Scanned PDFs (gitignored)
  ├── output/               # Results (gitignored)
  └── scripts/              # CLI entry points
  ```

- [ ] **1.2** Create `.gitignore` for sensitive/large files:
  - `input/`, `output/`, `*.pdf` (scans), `*.docx`, credentials

- [ ] **1.3** Remove extraneous files from repository:
  - `bob_and_alice.R`, `bob_n_alice.png`
  - `~$Quiz_2.docx` (temp file)
  - `.DS_Store`
  - Experimental/unused code files

- [ ] **1.4** Create `config.yaml` with all tunable parameters

---

## Phase 2: Core Module Refactoring

### 2.1 Image Processing Module (`R/image_processing.R`)

- [ ] **2.1.1** Extract and document `read_pdf_as_image(path)`:
  - Input: path to PDF file
  - Output: magick image object
  - Add input validation and error handling

- [ ] **2.1.2** Extract and document `preprocess_image(image, config)`:
  - Combines: scale, quantize, median filter, threshold
  - Make parameters configurable via config object
  - Return processed image with metadata

- [ ] **2.1.3** Extract and document `image_to_matrix(image)`:
  - Convert magick image to binary numeric matrix
  - Document the 0/1 convention (1 = filled)

### 2.2 Bubble Detection Module (`R/bubble_detection.R`)

- [ ] **2.2.1** Refactor `find_peaks(matrix)`:
  - Clean up `centralizer()` helper (rename to `weight_run()`)
  - Document the RLE-based peak detection algorithm
  - Add configurable threshold parameter

- [ ] **2.2.2** Refactor `find_bubbles(peak_matrix, threshold)`:
  - Rename from `find_interesting_points()`
  - Return clean data.frame with: id, x, y, intensity

- [ ] **2.2.3** Refactor `deduplicate_bubbles(bubbles, radius)`:
  - Simplify the complex while-loop logic
  - Add early termination conditions
  - Document the deduplication algorithm

- [ ] **2.2.4** Extract `euclidean_distance(x1, y1, x2, y2)` as utility

### 2.3 Template Matching Module (`R/template_matching.R`)

- [ ] **2.3.1** Refactor `find_nearest_template_match(bubbles, template)`:
  - Rename from `nearest_neighbors()`
  - Vectorize distance calculations for performance
  - Return structured match results

- [ ] **2.3.2** Refactor `calculate_perspective_correction(bubbles, template)`:
  - Extract from `run_corrections()`
  - Document the 4-point calibration marker approach
  - Return transformation parameters

- [ ] **2.3.3** Create `apply_perspective_correction(image, params)`:
  - Wrapper around `image_distort()`
  - Validate transformation parameters

### 2.4 Response Extraction Module (`R/response_extraction.R`)

- [ ] **2.4.1** Create `extract_student_id(matches)`:
  - Parse ID bubble matches
  - Return 10-digit string (or NA with warning)

- [ ] **2.4.2** Create `extract_test_version(matches)`:
  - Parse TV bubble matches
  - Return single character (w/x/y/z)

- [ ] **2.4.3** Create `extract_responses(matches, n_questions)`:
  - Parse QU bubble matches
  - Return vector of responses (A-E or NA)
  - Include confidence scores

- [ ] **2.4.4** Create unified `extract_all(matches)`:
  - Combines ID, version, and responses
  - Returns structured result object

### 2.5 Grading Module (`R/grading.R`)

- [ ] **2.5.1** Create `load_answer_key(path_or_name, config)`:
  - Support CSV files and inline definitions
  - Validate key format and length

- [ ] **2.5.2** Create `grade_responses(responses, key)`:
  - Compare responses to key
  - Return: correct/incorrect per question, total score

- [ ] **2.5.3** Create `calculate_statistics(graded_results)`:
  - Per-student: score, percentage
  - Per-question: % correct, point-biserial correlation
  - Aggregate: mean, median, std dev, distribution

### 2.6 Main Pipeline (`R/pipeline.R`)

- [ ] **2.6.1** Create `process_single_scan(path, config)`:
  - Orchestrates full pipeline for one file
  - Returns structured result or error

- [ ] **2.6.2** Create `process_batch(paths, config, parallel = FALSE)`:
  - Process multiple files
  - Aggregate results into single data frame
  - Optional parallel processing

- [ ] **2.6.3** Create `run_grading_session(scan_dir, quiz_name, config)`:
  - High-level entry point
  - Handles key loading, batch processing, statistics
  - Writes output files

---

## Phase 3: Configuration & Data Management

- [ ] **3.1** Create `R/config.R`:
  - `load_config(path)` - read YAML config
  - `validate_config(config)` - check required fields
  - `get_default_config()` - sensible defaults

- [ ] **3.2** Create `R/data_io.R`:
  - `load_bubble_map(path)` - read and validate template CSV
  - `load_roster(path)` - read and validate student roster
  - `save_results(results, path)` - write graded results
  - `save_statistics(stats, path)` - write summary stats

- [ ] **3.3** Migrate `bubble_map2.csv` to `data/bubble_map.csv`:
  - Clean up column names
  - Remove unused columns
  - Document coordinate system

---

## Phase 4: Testing

### Unit Tests (`tests/`)

- [ ] **4.1** `test_image_processing.R`:
  - Test image loading with valid/invalid paths
  - Test preprocessing produces expected dimensions
  - Test matrix conversion produces 0/1 values

- [ ] **4.2** `test_bubble_detection.R`:
  - Test peak finding on synthetic matrices
  - Test deduplication removes nearby points
  - Test threshold filtering

- [ ] **4.3** `test_template_matching.R`:
  - Test distance calculations
  - Test matching accuracy on known inputs
  - Test perspective correction parameters

- [ ] **4.4** `test_grading.R`:
  - Test score calculation (all correct, all wrong, mixed)
  - Test statistics calculations
  - Test edge cases (empty responses, extra responses)

### Integration Tests

- [ ] **4.5** Create test fixtures:
  - Sample scanned PDF with known responses
  - Expected output for that scan
  - Edge case scans (rotated, low quality, partial)

- [ ] **4.6** `test_pipeline.R`:
  - End-to-end test with fixture scan
  - Verify output matches expected results

---

## Phase 5: Performance Optimization

- [ ] **5.1** Profile current implementation:
  - Identify bottlenecks (likely: `nearest_neighbors`, `deduplicator_fn`)
  - Measure baseline processing time per scan

- [ ] **5.2** Vectorize distance calculations:
  - Replace for-loop in `nearest_neighbors()` with matrix operations
  - Use `outer()` or `dist()` for pairwise distances

- [ ] **5.3** Optimize deduplication:
  - Use spatial indexing or k-d tree for neighbor lookup
  - Consider `RANN` package for fast nearest neighbors

- [ ] **5.4** Add optional parallel processing:
  - Use `future` / `furrr` for batch processing
  - Make parallelism configurable

- [ ] **5.5** Benchmark and document:
  - Target: < 5 sec per scan, < 2 min for 30 scans
  - Document actual performance

---

## Phase 6: CLI & Documentation

- [ ] **6.1** Create CLI entry point (`scripts/grade.R`):
  ```
  Rscript scripts/grade.R --scan-dir quiz_scans/quiz_1 \
                          --quiz quiz_1 \
                          --config config/config.yaml
  ```

- [ ] **6.2** Add command-line argument parsing:
  - Use `argparse` or `optparse` package
  - Support: `--scan-dir`, `--quiz`, `--config`, `--output-dir`, `--verbose`

- [ ] **6.3** Write `README.md`:
  - Project overview
  - Installation instructions
  - Quick start guide
  - Configuration reference

- [ ] **6.4** Add inline documentation:
  - Roxygen2 comments for all exported functions
  - Document parameters, return values, examples

---

## Phase 7: Git & Deployment

- [ ] **7.1** Initialize clean git history:
  - Commit project structure
  - Commit each module separately with descriptive messages

- [ ] **7.2** Create GitHub repository:
  - Push to remote
  - Add appropriate license (MIT recommended)

- [ ] **7.3** Set up GitHub Actions (optional):
  - Run tests on push
  - Check code style

---

## Dependency Summary

```r
# Core dependencies
library(magick)      # Image processing
library(tidyverse)   # Data manipulation
library(data.table)  # Fast data operations
library(yaml)        # Config parsing

# Testing
library(testthat)    # Unit testing

# CLI
library(argparse)    # Command-line parsing

# Performance (optional)
library(future)      # Parallel processing
library(furrr)       # Parallel purrr
library(RANN)        # Fast nearest neighbors
```

---

## Milestones

| Milestone | Phases | Definition of Done |
|-----------|--------|-------------------|
| **M1: Clean Slate** | 1 | Directory organized, git initialized, config created |
| **M2: Refactored Core** | 2 | All modules extracted, documented, working |
| **M3: Tested** | 3, 4 | Tests pass, >80% coverage on core functions |
| **M4: Fast** | 5 | Meets performance targets |
| **M5: Shipped** | 6, 7 | CLI works, README complete, on GitHub |
