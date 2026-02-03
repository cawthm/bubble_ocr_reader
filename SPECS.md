# Bubble Test OCR Reader - Specification

## Overview

A command-line application that processes scanned bubble test answer sheets, extracts student responses via image analysis, grades them against answer keys, and produces summary statistics.

## Input

### 1. Scanned Answer Sheets
- **Format**: PDF files (single-page scans from phone camera or flatbed scanner)
- **Source**: `bubble_template.pdf` forms filled in by students
- **Organization**: Grouped by quiz/exam in subdirectories (e.g., `quiz_scans/quiz_1/`)

### 2. Bubble Template Reference
- **File**: `bubble_map.csv`
- **Contents**: Pixel coordinates for each bubble on the standardized form
- **Bubble Types**:
  - `MK` (Markers): 4 calibration circles for perspective correction
  - `TV` (Test Version): 4 options (w, x, y, z)
  - `ID` (Student ID): 10-digit grid (digits 0-9)
  - `QU` (Questions): 48 questions × 5 choices (A-E)

### 3. Answer Keys
- **Format**: CSV or programmatic definition
- **Structure**: `question_number`, `correct_answer`, `chapter` (optional)
- **Versioning**: Support multiple test versions (w, x, y, z) with different answer orders

### 4. Student Roster
- **File**: `roster.csv`
- **Fields**: `student_id`, `last_name`, `first_name`, `email`

## Processing Pipeline

### Stage 1: Image Preprocessing
1. Load PDF and convert to image
2. Scale to standard dimensions (1260×1530 pixels)
3. Convert to 2-color grayscale (black/white)
4. Apply median filter to reduce noise
5. Apply threshold to create binary image

### Stage 2: Bubble Detection
1. Convert image to binary matrix (0=white, 1=black)
2. Find peak concentrations using run-length encoding
3. Identify "interesting points" (filled bubbles) above threshold
4. Deduplicate nearby points (keep highest intensity)

### Stage 3: Perspective Correction
1. Locate 4 calibration markers on scanned image
2. Match to expected marker positions on template
3. Calculate perspective transformation matrix
4. Apply distortion correction to align with template

### Stage 4: Response Extraction
1. Re-process corrected image through bubble detection
2. Match detected points to template bubble positions
3. Calculate confidence metric: `intensity / (distance + 1)`
4. Extract:
   - Student ID (10 digits)
   - Test Version (w/x/y/z)
   - Question responses (A-E for each question)

### Stage 5: Grading
1. Load answer key for specified quiz/version
2. Compare student responses to correct answers
3. Calculate score (correct / total)
4. Flag low-confidence or missing responses for review

### Stage 6: Output Generation
1. Individual student results (ID, responses, score)
2. Aggregate statistics (mean, median, distribution)
3. Flagged items requiring manual review
4. Export to CSV

## Output

### 1. Graded Results
- **File**: `results_{quiz_name}_{timestamp}.csv`
- **Fields**: `student_id`, `name`, `test_version`, `score`, `percentage`, `q1`...`qN`

### 2. Summary Statistics
- **File**: `summary_{quiz_name}_{timestamp}.csv`
- **Contents**:
  - Total students processed
  - Score distribution (mean, median, std dev, min, max)
  - Per-question statistics (% correct, discrimination index)
  - Flagged scans requiring review

### 3. Audit Trail
- **File**: `audit_{quiz_name}_{timestamp}.log`
- **Contents**: Processing log with warnings, corrections, and confidence scores

## Configuration

### `config.yaml`
```yaml
# Image processing parameters
image:
  target_width: 1260
  target_height: 1530
  median_radius: 14
  threshold_percent: 40

# Detection parameters
detection:
  peak_threshold: 10
  dedup_radius: 10
  confidence_min: 40

# Paths
paths:
  bubble_map: "bubble_map.csv"
  roster: "roster.csv"
  scans_dir: "quiz_scans/"
  output_dir: "output/"

# Answer keys
answer_keys:
  quiz_1: "keys/quiz_1.csv"
  quiz_2: "keys/quiz_2.csv"
  # ...
```

## Error Handling

| Error Type | Detection | Action |
|------------|-----------|--------|
| Unreadable scan | No calibration markers found | Flag for rescan |
| Low confidence ID | Fuzzy match distance > threshold | Flag for manual review |
| Missing responses | Fewer responses than expected | Flag, include partial results |
| Unknown student | ID not in roster | Flag, include in results |
| Multiple marks | >1 bubble filled per question | Use highest confidence or flag |

## Performance Requirements

- Process single scan: < 5 seconds
- Batch of 30 scans: < 2 minutes
- Memory usage: < 500MB for batch processing

## Non-Functional Requirements

1. **Modularity**: Each pipeline stage is an independent, testable function
2. **Logging**: All operations logged with timestamps and metrics
3. **Reproducibility**: Same input always produces same output
4. **Graceful degradation**: Partial results returned on errors
