# COVID Weekly

Python utility to retrieve weekly COVID data organized by zip code and persist for use by the
GeoDa team in the Center for Spatial Data Science at the University of Chicago.

---

1. [Description](#Description)
1. [Secrets](#Secrets)
1. [Installation](#Installation)
1. [Using](#Using)
1. [Developer Notes](#Developer-Notes)

## Description

This library offers utilities for downloading, parsing, and persisting weekly COVID-19 data from the
city of Chicago via Socratica. We use the [COVID-19 Cases, Tests, and Deaths by ZIP Code](https://dev.socrata.com/foundry/data.cityofchicago.org/yhhz-zm2v) API endpoint.

The primary parsing that this library currently provides is to limit the amount of columns and to
transition from a tall table to a wide one.

See below for links to documentation used while developing this library.

- https://data.cityofchicago.org/resource/yhhz-zm2v.json
- https://data.cityofchicago.org/Health-Human-Services/COVID-19-Cases-Tests-and-Deaths-by-ZIP-Code/yhhz-zm2v
- https://dev.socrata.com/foundry/data.cityofchicago.org/yhhz-zm2v
- https://data.cityofchicago.org/profile/edit/developer_settings
- https://support.socrata.com/hc/en-us/articles/210138558-Generating-an-App-Token
- https://spatial.uchicago.edu/geoda
- https://drive.google.com/file/d/1snGYoIPVmmHIogYGUmxJ2ZldCB1VIl_S/view

## Secrets

To manage secrets, this library uses environment variables. A sample environment can be found at `.sample_env`

The secrets currenly used are:

- SOCRATICA_APP_TOKEN
  - The APP token for authenticating with Socratica's API.
  - The API endpoint used is free to use, but to get past some rate limit throttling, it is advised to create an account and an APP token (both still free).
  - See [here](https://support.socrata.com/hc/en-us/articles/210138558-Generating-an-App-Token) for more information.

## Installation

1. `scripts/install` for production installation or `scripts/install-dev` for development
1. Make sure all variables at `.sample_env` are exported to your current environment

## Using

- `run --filepath={LOCAL_FILEPATH}` to run as a script
- `from covid_weekly import run` to run as a Python package

## Developer Notes

- The author created this package not knowing how it would be used in the future, and so tried to write it to be as flexible as possible
- See [docs/data.ipynb](docs/data.ipynb) for documentation on the data used by the package
- See [docs/library_usage.ipynb](docs/library_usage.ipynb) for examples on how to use the package as a library
