# Combined noun semantics visualisations

This standalone Shiny app combines:

- `quantifiers_combined.csv`: `many` versus `much`
- `number_combined.csv`: plural versus singular
- `determiners_combined.csv`: `a` versus `the`, restricted to rows where `noun_pos == "N"`

The app aggregates observations by noun lemma and period, harmonizes detailed periods
from `period_simple`, and retains raw denominators alongside raw and smoothed
proportions.

The semantic-category controls can exclude individual `semantics` levels or merge
levels by assigning them to the same group. The downloaded aggregate retains the
source label in `semantics_original` and places the selected or merged label in
`semantics`.

The Animated PCA tab fits one PCA coordinate system across all selected periods
and displays cumulative lemma trajectories. Lemmas can be restricted to those
observed in a minimum number of selected periods. When a lemma is absent from an
intermediate period, its last observed position remains visible until its next
observation.

The Clustered Heatmap tab displays one narrow heatmap per selected period. Lemma
labels omit the period name, semantic groups are boxed as separate panels, and
the color scale is shared across the displayed periods.

Run the app from the repository root:

```r
shiny::runApp("combined_visualisations")
```

Regenerate cached CSV and RDS aggregates:

```sh
Rscript data_processing/prepare_combined_visualisation_data.R
```

The primary plots require all three primary features. Quantifier `many`/`much`
cells are often sparse, so use the minimum-observation filters and inspect the
denominators before interpreting extreme values.
