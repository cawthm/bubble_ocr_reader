# Bubble Test OCR Reader

Automatically grade scanned bubble sheet answer forms. Extracts student IDs, test versions, and responses via image processing, then grades against answer keys.

## Clone

```bash
git clone https://github.com/cawthm/bubble_ocr_reader.git
cd bubble_ocr_reader
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
bubble_ocr_reader/
├── R/                  # Core modules
├── scripts/            # CLI entry point
│   └── grade.R
├── config/             # Configuration
│   └── config.yaml
├── data/               # Bubble template coordinates
│   └── bubble_map.csv
├── keys/               # Answer keys
│   └── sample_versioned_key.csv
├── sample_scans/       # Example scans for testing
└── output/             # Graded results appear here
```

## Input

### Scanned Answer Sheets

PDF scans of completed bubble forms (phone camera or flatbed scanner).

![Sample scan](./sample_scans/sample_scan_preview.png)

### Answer Key

CSV with `resp_no` column plus version columns. Versions are 4-character codes from {w,x,y,z}:

```csv
resp_no,wxxy,wxxz,zyzx,yyyy,wxyz,zzyw
1,a,c,b,d,a,a
2,b,a,d,c,b,d
3,c,b,a,b,a,b
4,d,d,c,a,d,b
5,e,e,e,e,e,c
...
```

Students mark their test version in the 4-column grid at the top of the form, and the system matches their responses to the correct key column.

## Usage

Try it with the included sample scans:

```bash
Rscript scripts/grade.R \
  --scan-dir sample_scans \
  --quiz sample_test \
  --key keys/sample_versioned_key.csv
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
301245678,wxyz,sample_test,1,a,133.12,sample_scans/sample_scan1.pdf,a,TRUE
301245678,wxyz,sample_test,2,b,156.40,sample_scans/sample_scan1.pdf,b,TRUE
...
```

**summary_\<quiz\>_\<timestamp\>.txt**
```
===== GRADING SUMMARY =====

Students graded: 2

Score Distribution:
  Mean:   95.0%
  Median: 95.0%
  Std Dev: 7.1%
  Range:  90.0% - 100.0%

Most Missed Questions:
  Q5: 50.0% correct
  Q1: 100.0% correct
  ...
```

## License

MIT
