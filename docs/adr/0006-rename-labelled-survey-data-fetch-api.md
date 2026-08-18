# Rename Labelled Survey Data Fetch API

Status: accepted

qualtdict will rename the public `get_survey_data()` API to
`fetch_labelled_survey_data()` and remove the old exported name. The old name
collides with another package in `pkgcheck`, sounds like a general survey-data
getter, and conflicts with qualtdict's boundary as a metadata and
labelled-export companion to qualtRics. The new name keeps the network-fetch
signal while naming the package-owned result, Labelled Survey Data.

This deliberately supersedes the part of ADR 0003 that kept
`get_survey_data()` stable. A deprecated exported alias would preserve user
compatibility, but it would also preserve the duplicate exported-function
finding this decision is meant to resolve.
