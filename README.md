# Bubble Test OCR Reader

Automatically grade scanned bubble sheet answer forms. Extracts student IDs, test versions, and responses via image processing, then grades against answer keys.

## Clone

```bash
git clone https://github.com/yourusername/bubble_test_ocr_reader.git
cd bubble_test_ocr_reader
```

## Dependencies

**R 4.0+** with packages:

```r
install.packages(c("magick", "tidyverse", "data.table", "yaml", "argparse"))
```

**ImageMagick** (required by the `magick` R package):

| OS | Command |
|----|---------|
| macOS | `brew install imagemagick` |
| Ubuntu/Debian | `sudo apt-get install libmagick++-dev` |
| Windows | Download installer from [imagemagick.org/script/download.php](https://imagemagick.org/script/download.php) |

## Directory Structure

```
bubble_test_ocr_reader/
├── R/                  # Core modules
├── scripts/            # CLI entry point
│   └── grade.R
├── config/             # Configuration
│   └── config.yaml
├── data/               # Bubble template coordinates
│   └── bubble_map.csv
├── keys/               # Answer keys
├── input/              # Place scanned PDFs here
└── output/             # Graded results appear here
```

## Input

### Scanned Answer Sheets

PDF scans of completed bubble forms (phone camera or flatbed scanner).

<img src="sample_scans/sample_scan_preview.png" alt="Sample scan" width="400">

### Answer Key

CSV with `resp_no` column plus version columns. Versions are 4-character codes from {w,x,y,z}:

```csv
resp_no,wxyz,zzyw
1,a,a
2,b,d
3,a,b
4,d,b
5,e,c
...
```

Students mark their test version in the 4-column grid at the top of the form, and the system matches their responses to the correct key column.

## Usage

```bash
Rscript scripts/grade.R \
  --scan-dir input/quiz_1 \
  --quiz quiz_1 \
  --key keys/quiz_1_key.csv
```

**Options:**
| Flag | Description |
|------|-------------|
| `--scan-dir`, `-s` | Directory containing PDF scans (required) |
| `--quiz`, `-q` | Quiz name for output files (required) |
| `--key`, `-k` | Path to answer key CSV (required) |
| `--output-dir`, `-o` | Output directory (default: `output/`) |
| `--config`, `-c` | Config file (default: `config/config.yaml`) |
| `--quiet` | Suppress progress messages |

## Output

Results are saved to `output/`:

**results_\<quiz\>_\<timestamp\>.csv**
```csv
student_id,test_version,quiz,resp_no,ans,metric,file,key,correct
301245678,wxyz,quiz_1,1,a,133.12,input/scan1.pdf,a,TRUE
301245678,wxyz,quiz_1,2,b,156.40,input/scan1.pdf,b,TRUE
...
```

**summary_\<quiz\>_\<timestamp\>.txt**
```
===== GRADING SUMMARY =====

Students graded: 25

Score Distribution:
  Mean:   82.5%
  Median: 85.0%
  Std Dev: 12.3%
  Range:  45.0% - 100.0%

Most Missed Questions:
  Q7: 32.0% correct
  Q12: 48.0% correct
  ...
```

## License

MIT
