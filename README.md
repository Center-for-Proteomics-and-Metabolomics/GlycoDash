# GlycoDash <img src="man/figures/glycodash_logo.png" align="right" height="138" /></a>

GlycoDash is an R Shiny dashboard for processing glycomics data obtained from [LaCyTools](<https://pubs.acs.org/doi/10.1021/acs.jproteome.6b00171>) or [Skyline](<https://skyline.ms>).

## Installation

### Option 1: Run GlycoDash using Docker (recommended)
_Note: it is recommended that you have at least 16 GB of RAM._
1. Download and install [Docker desktop](https://www.docker.com/products/docker-desktop/).
2. Start Docker desktop on your computer.
3. Open Command Prompt.
4. Get most recent GlycoDash package [here](https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash/pkgs/container/glycodash).

   Then pull the Docker image by pasting the listed command in your command prompt.
   
   Docker will now build an image which can take some time.
6. The image should now be loaded in Docker. Click the Run button. Then click optional settings, and for "host port" fill in 80. Then click "Run".

![afbeelding](https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash/assets/105744767/1f626535-66c9-419e-9ca9-447213bef07d)

![afbeelding](https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash/assets/105744767/439a415e-5d35-4180-a9c1-4457853e3f42)

7. Under the "Containers" tab in Docker, you should now see something like this:
   
![afbeelding](https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash/assets/105744767/30c51e52-6e99-429c-bc14-edd9687dd25f)

8. To use the dashboard: open your browser, type "localhost" in the search bar and hit enter. You should now be able to use the dashboard:

![afbeelding](https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash/assets/105744767/7c838f22-c035-4d7c-bd06-20f92174df1d)


9. When you are done using GlycoDash, simply stop the container in Docker by clicking the stop button.


### Option 2: Run the GlycoDash code in RStudio.

1.  Install R and Rstudio: <https://posit.co/download/rstudio-desktop/>
2.  Download the source code of the most recent release as a zip file, then unzip and
    store the folder somewhere.
3.  Double click “glycodash.Rproj”, this will launch RStudio.
4.  In Rstudio: click file –\> open. Then in the glycodash-master
    folder, open “run_dev.R” in the dev folder.
5.  In Rstudio, click the “source” button, or use Ctrl+Shift+S, to start
    the Dashboard. A new Rstudio window should now open with the
    dashboard running. *(Rstudio might first ask you to install required
    packages. Accept this. For package version requirements: see the DESCRIPTION
    file. See the Dockerfile for the package versions that were used in development.*)
7.  Optional: in the Rstudio dashboard window, click “open in browser”
    (this should look better). 

## How to use GlycoDash
_To be filled in..._

## Branches

The ```master``` branch will be used for “official” releases.

The ```alfa``` branch is the development branch.

The ```beta``` branch contains the version of the dashboard that is currently
running on the cpmtools server.
