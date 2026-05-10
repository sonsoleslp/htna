## Test environments

* Local: macOS 26.3.1, R 4.5.2
* GitHub Actions: ubuntu-latest, macos-latest, windows-latest (R-release)
* GitHub Actions: ubuntu-latest (R-devel)

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTEs (other than the install-time warning
described below).

## Notes for reviewers

### Upstream NAMESPACE warning

`R CMD check` surfaces an install-time warning:

> S3 methods 'print.wtna_perm_mixed', 'summary.wtna_perm_mixed' were
> declared in NAMESPACE but not found

This warning originates in `Nestimate` (one of htna's `Imports`):
`Nestimate`'s NAMESPACE declares those S3 methods but does not export
the implementations. The warning is repeated in several of `R CMD
check`'s checks (`code/documentation mismatches`, `S3 generic/method
consistency`, etc.) but always reflects the same upstream issue, not
anything in htna itself. We have notified the `Nestimate` maintainers
and the warning will disappear once they cut a fix release.

### Method references

The package implements original wrapper logic on top of `Nestimate` and
`cograph`. There are no published references describing the methods
themselves; the underlying transition-network methodology is well
established in the literature on stochastic process mining and
sequence analysis.
