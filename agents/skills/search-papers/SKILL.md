---
name: search-papers
description: Search academic paper databases (PubMed, arXiv, bioRxiv) and retrieve paper metadata including DOIs
---

## Instructions

Use this skill to search for academic papers across three databases. Each script accepts a search query and an optional maximum number of results.

### Scripts

#### `search_pubmed.R`

Search PubMed (NCBI) for biomedical and life science papers.

```bash
Rscript scripts/search_pubmed.R "<query>" [max_results]
```

- `query` (required): Search terms (e.g., `"intracranial EEG epilepsy"`)
- `max_results` (optional): Maximum number of results to return (default: 5)

Returns: Title, authors, journal, publication year, DOI, PMID.

#### `search_arxiv.R`

Search arXiv for preprints in physics, math, CS, and related fields.

```bash
Rscript scripts/search_arxiv.R "<query>" [max_results]
```

- `query` (required): Search terms (e.g., `"neural signal processing"`)
- `max_results` (optional): Maximum number of results to return (default: 5)

Returns: Title, authors, published date, arXiv ID, DOI (if available).

#### `search_biorxiv.R`

Search bioRxiv for biology preprints.

```bash
Rscript scripts/search_biorxiv.R "<query>" [max_results]
```

- `query` (required): Search terms (e.g., `"electrophysiology cortex"`)
- `max_results` (optional): Maximum number of results to return (default: 5)

Returns: Title, authors, date, DOI, category.

### Usage Tips

- Use specific search terms for better results
- Combine terms with spaces for AND-style queries
- PubMed is best for published biomedical literature
- arXiv is best for physics, math, computer science, and quantitative biology preprints
- bioRxiv is best for biology preprints
