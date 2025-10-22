# Replication Files for "Reaching for the Threshold: How Minimum Participation Rules Facilitate Multilateral Treaty Ratification"

**Authors:** Christian Arnold (University of Birmingham) and Carsten Schulz (University of Cambridge)
**Journal:** Journal of Conflict Resolution
**Corresponding Author:** 
* Paper: Carsten (cas245@cam.ac.uk) / Code: Chris (c.arnold.2@bham.ac.uk)

---

## Overview

This Dataverse deposit contains all materials required to reproduce the analyses.

## Repository Contents

### 1. Data
* `dat_mtdsg_18april2019.dta`: Our main data that contains all the coding work of thresholds. For variables, see codebook codebook_mtdsg_18april2019.docx. Note: this does not live on github. Check dataverse or get in touch with Chris. 
* `dat_public_common_annotation.csv`: Our annotation of public and common goods
* `dat_mprdsm.rdata`: The LLM-annotated dispute settlement mechanisms
* `dat_treaty_corpus.rdata`: The corpus of full treaty texts in case you want to run the LLM annotation of the dispute settlement mechanisms on your own.
* `dat_IdealpointestimatesAll_Jun2024.csv`: The TSCS idealpoint estimates from Bailey et al. 2017

### 2. Code

#### 2.1 Software 
The replication files were developed on aarch64-apple-darwin20 running under: macOS 15.6.1 using R version 4.4.1 (2024-06-14). For more details, see `mpr_master.r`


#### 2.2 Running the Replication
* Open the main script: `mpr_master.r`. Things should all run from there.
* *Expected runtime: ~20 minutes on a standard laptop.

### 3. Figures
* Will populate through running the code 

### 4. Codebook 
* `codebook_mtdsg_18april2019.docx` contains the codebook with the variable names of the core data set