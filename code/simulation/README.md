# README

I used the `batchtools` package and the abstract experiment description (which is described in the package documentation), to structure our code in a convenient way. In this regard, I separate the following parts: the problems, algorithms, experiments and batch system specific parts.

A so-called registry object is created, which defines a directory where all relevant information, files and results of the computational jobs are stored with `makeExperimentRegistry`. Thus, the results are reproducible:

<ul>
with the ``registry`` object everybody can reproduce the results on their own batch system by simply exchanging the cluster functions back end. In our experiment, each problem has one  unique seed   (i.e. synchronized problems). The problem seed is incremented only depending on the experiment replication so that all the algorithms retrieve the same problem instances for each distinct replication.
</ul>

## Scripts

The main script is `rerun-batchtools.R`. It runs the whole simulation study:

- `create-static-part.R`<br/>
- `problems-wce-ranef-survival.R`<br/>
- `algorithms-wce-ranef-survival.R`<br/>
- `setup-batch-wce-ranef-survival.R`<br/>
- `submit-jobs-wce-ranef-survival.R`<br/>

`eval-utils.R` contains the helper functions that are used in scripts that are elsewhere to evaluate simulation results and arrange them either in a visual or a tabular format.



