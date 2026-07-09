# How to publish this repo + mint the Zenodo DOI

This bundle is ready to push. The DOI is minted **automatically by Zenodo** the first time a GitHub
**release** is published, exactly as was done for `ebrahim.gof`.

## Your steps (one-time)
1. Create an empty GitHub repo named **`logistic-gof-benchmark`** (public) under `ebrahimkhaled`.
2. Sign in at **https://zenodo.org** with GitHub, open **Settings → GitHub**, and **flip the toggle ON**
   for `ebrahimkhaled/logistic-gof-benchmark` (this installs the webhook). Do this *before* the release.
3. Tell me it's enabled.

## My steps (once you confirm)
```bash
cd "logistic-gof-benchmark"
git init && git add -A && git commit -m "Reproducibility materials for the logistic-regression GOF benchmark"
git branch -M main
git remote add origin https://github.com/ebrahimkhaled/logistic-gof-benchmark.git
git push -u origin main
git tag -a v1.0.0 -m "v1.0.0" && git push origin v1.0.0
gh release create v1.0.0 --title "v1.0.0" --notes "Initial archived release accompanying the SLADS submission."
```
Publishing the release triggers Zenodo to archive the tag and mint a **versioned DOI** plus a **concept DOI**
(always points to the latest). 

## After the DOI exists
- Replace `10.5281/zenodo.XXXXXXX` in `README.md`, `CITATION.cff`, and `.zenodo.json` with the **concept DOI**.
- Add one line to the paper's *Code and Data Availability* section:
  > "Simulation drivers, aggregated results, and figure code are archived at
  > https://doi.org/10.5281/zenodo.XXXXXXX (concept DOI)."
  (In the double-anonymized copy this becomes "[archive DOI withheld for review]".)
- Optionally upload the large per-replication `pvalues_*.csv` (~180 MB) directly to the Zenodo record as an
  extra data file.
