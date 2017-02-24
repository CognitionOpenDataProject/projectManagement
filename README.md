This repo contains a number of R functions to be used by project leads to manage the project. The functions mainly involve updating the "Articles" Google Sheet in response to Coder and Pilot activities.
Below is a list of events and activities (in roughly chronological order of use) and their corresponding functions:

#
* **EVENT**: Coder requests batch of articles
* **ACTION**: coderRequest.R

#
* **EVENT**: Coder returns article(s) uncompleted
* **ACTION**: coderReturn.R

#
* **ACTIVITY**: Update triage pool
* **ACTION**: updateTriage.R

#
* **ACTIVITY**: Do some triage
* **ACTION**: doTriage.R

#
* **EVENT**: A second project lead verifies triaged repo
* **ACTION**: verifyTriage.R

#
* **EVENT**: Pilot request article
* **ACTION**: pilotRequest.R

#
* **EVENT**: Pilot returns article uncompleted
* **ACTION**: pilotReturn.R

#
* **EVENT**: Pilot report complete
* **ACTION**: pilotComplete.R

#
* **EVENT**: Joint (pilot & copilot) report complete
* **ACTION**: reportComplete.R
