Models, analysis, and reproducible results come from ["Modeling the effects of crime type and evidence on judgments about guilt"](https://www.nature.com/articles/s41562-018-0451-z.epdf?author_access_token=gW_gZL0F4bNCBdSfJdfHqtRgN0jAjWel9jnR3ZoTv0OPcExbUXFEBLmRIJVwmtiNjh9IEH2pkC2Nh_cBrWPkHuJj4keS7hpDBQvmnU20N9jF3OGevYkvLVEkxopzUvo61hticf34wy0yLHXrWmQ-AA%3D%3D).
- [Github Repo](https://github.com/pearsonlab/legal)

# What you need (dependencies):
We used R via [RStudio](https://www.rstudio.com/). RStudio is not strictly necessary, but it may make building some aspects of the project (e.g., the supplement) easier. We also make heavy use of the [Stan](http://mc-stan.org/) probabilistic programming language and the [tidyverse](https://tidyverse.tidyverse.org/index.html). A few other dependencies are used for particular plots or tables.

In particular, we use:
- `rmarkdown` (plus dependencies)
- `rstan` (plus dependencies)
- `tidyverse`
- `kableExtra`
- `magick`
- `gridBase`
- `Hmisc`
- `jsonlite`

# About the data
The data are recorded in a single file, `combined_data.csv` in the `data` folder. It is the output of `make_public_dataset.R`, which uses files derived from the experiment. 

The file is a single table, one line per rating given, with the following columns:
- `uid`: Unique id for each participant.
- `experiment`: Version of the experiment run. Cf. Supplementary Table 2.
- `scenario`: Integer indicating which crime was presented on a given trial: (1 - 33).
- `physical`: Which physical evidence was presented? (`No Physical`, `Non-DNA`, `DNA`)
- `history`: What criminal history information was presented? (`No History`, `Unrelated`, `Related`)
- `witness`: What eyewitness information was presented? (`No Witness`, `Yes Witness`)
- `rating_type`: Which type of rating does the datum represent:
  - `rating`: Most common. "How strong is the case that the accused committed this crime?"
  - `rate_punishment`: Next most common when participants gave two or more ratings. "How severe should the punishment be for someone who commits a crime like this one?"
- `rating`: Numerical rating for the relevant question (1 - 100).
- `group`: Which experimental group the participant belonged to:
  - `mturk`: Amazon Mechanical Turk sample. (Previously reported in ["Modeling the effects of crime type and evidence on judgments about guilt"](https://www.nature.com/articles/s41562-018-0451-z.epdf?author_access_token=gW_gZL0F4bNCBdSfJdfHqtRgN0jAjWel9jnR3ZoTv0OPcExbUXFEBLmRIJVwmtiNjh9IEH2pkC2Nh_cBrWPkHuJj4keS7hpDBQvmnU20N9jF3OGevYkvLVEkxopzUvo61hticf34wy0yLHXrWmQ-AA%3D%3D) )
  - `mri`: fMRI sample

All together, the fMRI sample data comprise more than 2,140 ratings from 33 unique individuals. Demographic information were collected from all participants but only included in the combined data for the mTurk sample.

# Building the project
The main directory contains a Makefile. Mac and Linux users should have `make` installed. Windows users will need to get `make` ([Rtools](https://cran.r-project.org/bin/windows/Rtools/) has it). See also this [StackOverflow Answer](https://stackoverflow.com/questions/33608345/how-to-execute-a-makefile-from-r).

Once you have `make` installed, you can open a terminal, navigate to the project directory, and then type
```shell
$ make models
```
to run all the models and postprocess their outputs,
```shell
$ make figs
```
to make all the figures,
or
```shell
$ make supplement
```
to generate the supplement (from an RMarkdown file). The latter two require that the postprocessed model outputs exist. So the models will be run in any case.

Finally, if you want to make everything, just do
```shell
$ make
```

**Warning:** The Stan models can be time-consuming to run. On a four-core machine with a decent processor, the entire set of models produces a couple hundred MB of outputs and requires over **14 hours** to finish (and that's not including the sensitivity analysis models, which double the time). If you're on a laptop that's put to sleep, Stan should resume when the laptop wakes up, but the whole process is best done on a desktop that can be left alone for the better part of a day.
