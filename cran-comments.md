## Test environments

- Local: Windows 11, R 4.4.0
- GitHub Actions: windows-latest, macOS-latest, ubuntu-latest (R release, devel)
- win-builder: R devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes

- The package depends on an external Power BI dashboard maintained by the
  Brazilian Ministry of Health. If the dashboard URL or schema changes,
  the package may need updates.
- Internet access is required for data download functions.
- The API has a ~30K row limit per request. The package implements
  partitioned download (ASC + DESC) for larger tables when focused on
  Rio de Janeiro.
