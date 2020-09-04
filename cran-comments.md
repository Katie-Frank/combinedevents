## Test environments
* local R installation, R 4.0.2, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.0.2, R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

In addition to the new release note, R-devel on Windows Server generated two notes:

* "Possibly mis-spelled words in DESCRIPTION: IAAF (11:72)", but the spelling is okay as IAAF is an initialism for International Association of Athletics Federation.
* "checking for future file timestamps ... NOTE unable to verify current time", but the external clock used for this check appears to be down (https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time).

## Downstream dependencies

There are currently no downstream dependencies for this package
