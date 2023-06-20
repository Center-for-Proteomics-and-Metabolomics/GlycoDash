
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Glycodash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

Glycodash is an R Shiny dashboard for processing IgG and IgA
glycosylation data obtained from LaCyTools
(<https://pubs.acs.org/doi/10.1021/acs.jproteome.6b00171>).

## Installation

### Option 1: Run Glycodash using Docker

1. Download and install Docker desktop (https://www.docker.com/products/docker-desktop/)
2. Start Docker desktop on your computer.
3. Open Command Prompt.
4. Look for the most recent Glycodash package here: https://github.com/orgs/Center-for-Proteomics-and-Metabolomics/packages?repo_name=glycodash

    Then pull the Docker image by pasting the listed command in your command prompt, i.e.:
   
    ```console
    docker pull ghcr.io/center-for-proteomics-and-metabolomics/glycodash-v1.0.0:latest
    ```
   Docker will now build an image which can take some time.
5. The image should now be loaded in Docker. Click the Run button. Then click optional settings, and for "host port" fill in 80. Then click "Run".

   <img width="640" alt="docker" src="https://github.com/Center-for-Proteomics-and-Metabolomics/glycodash/assets/105744767/a3ec7f36-2261-4af8-a3ba-597480cde490">
   <br />
   <img width="276" alt="docker2" src="https://github.com/Center-for-Proteomics-and-Metabolomics/glycodash/assets/105744767/8f0ef8bd-1fee-440d-92b5-316438fe8685">

6. Under the "Containers" tab in Docker, you should now see something like this:
   
   <img width="449" alt="docker3" src="https://github.com/Center-for-Proteomics-and-Metabolomics/glycodash/assets/105744767/282e7c79-739f-489a-8ecb-644a2531a595">

7. To use the dashboard: open your browser, type "localhost" in the search bar and hit enter. You should now see this:

   <img width="814" alt="glycodash" src="https://github.com/Center-for-Proteomics-and-Metabolomics/glycodash/assets/105744767/c2840219-ab01-42f4-8d1b-59a6b95cec05">

8. When you are done using Glycodash, simply stop the container in Docker by clicking the stop button.


### Option 2: Run the Glycodash code in RStudio.

1.  Install R and Rstudio: <https://posit.co/download/rstudio-desktop/>
2.  Download the source code of the most recent release as a zip file, then unzip and
    store the folder somewhere.
3.  Double click “glycodash.Rproj”, this will launch RStudio.
4.  In Rstudio: click file –\> open. Then in the glycodash-master
    folder, open “run_dev.R” in the dev folder.
5.  In Rstudio, click the “source” button, or use Ctrl+Shift+S, to start
    the Dashboard. A new Rstudio window should now open with the
    dashboard running. *(Rstudio might first ask you to install required
    packages. Accept this. For package requirements: see the DESCRIPTION
    file.*)
7.  Optional: in the Rstudio dashboard window, click “open in browser”
    (this should look better). The dashboard was tested in Google Chrome
    and Microsoft Edge.


## Branches

The ```master``` branch will be used for “official” releases.

The ```alfa``` branch is the development branch.

The ```beta``` branch contains the version of the dashboard that is currently
running on the cpmtools server.
