## Test environments
* local mac OS R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

On my local R installation,

0 errors | 0 warnings | 2 notes

* This is a new release.
* checking for future file timestamps ... NOTE
  unable to verify current time
  * For this note, the external clock used for this check appears to be down (https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time).

In addition to the new release note, **R-devel on Windows Server** and **R-release on Ubuntu Linux** generated two notes:

* "Possibly mis-spelled words in DESCRIPTION: IAAF (11:72)", but the spelling is okay as IAAF is an initialism for International Association of Athletics Federation.
* "checking for future file timestamps ... NOTE unable to verify current time"

Other than the new release note, **R-devel on Fedora Linux** generated the same note on the spelling of IAAF.

## Downstream dependencies

There are currently no downstream dependencies for this package
