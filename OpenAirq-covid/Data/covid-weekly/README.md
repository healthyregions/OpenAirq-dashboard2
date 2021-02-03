# COVID Weekly

Python utility to retrieve weekly COVID-19 data organized by zip code and persist for use by the
GeoDa team in the Center for Spatial Data Science at the University of Chicago.

---

1. [Description](#Description)
1. [Secrets](#Secrets)
1. [Installation](#Installation)
1. [Production](#Production)
1. [Development](#Development)
1. [Testing](#Testing)
1. [Developer Notes](#Developer-Notes)

## Description

This library offers utilities for downloading, parsing, and persisting weekly COVID-19 data from the
city of Chicago via Socratica. We use the [COVID-19 Cases, Tests, and Deaths by ZIP Code](https://dev.socrata.com/foundry/data.cityofchicago.org/yhhz-zm2v) API endpoint.

The primary parsing that this library currently provides is to limit the amount of columns and to
transition from a tall table to a wide one.

See below for links to documentation used while developing this library.

- [Socratica's enpoint](https://data.cityofchicago.org/resource/yhhz-zm2v.json)
- [City of Chicago source data documentation](https://data.cityofchicago.org/Health-Human-Services/COVID-19-Cases-Tests-and-Deaths-by-ZIP-Code/yhhz-zm2v)
- [Socratica's Documentation on the API endpoint](https://dev.socrata.com/foundry/data.cityofchicago.org/yhhz-zm2v)
- [Generating a Socratica APP token](https://support.socrata.com/hc/en-us/articles/210138558-Generating-an-App-Token)

## Documentation

- See [docs/data.ipynb](docs/data.ipynb) for documentation on the data used by the package
- See [docs/library_usage.ipynb](docs/library_usage.ipynb) for examples on how to use the package as a library
- This package uses [pydocstyle](http://www.pydocstyle.org/en/stable/) with numpy styling for docstring formatting

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

## Production

- `scripts/install` to install the package and bash script
- `export SOCRATICA_APP_TOKEN={secret}` to provide your app token
- `scripts/test-it` to verify installation
- run
  - `run --filepath={LOCAL_FILEPATH}` to run as a script
  - `from covid_weekly import run` to run as a Python package

## Development

- Activate your virtual environment of choice
- `scripts/install-dev` to install the package, bash script, and developer tooling
  - As a side effect this creates a jupyter kernel which adds this package to its path which allows
    for importing this package without re-running `python setup.py install` after every change
- `export SOCRATICA_APP_TOKEN={secret}` to provide your app token
- `scripts/test` to run unit tests or `scripts/test-it` to run integration tests

### Testing

This package use [pytest](https://docs.pytest.org/en/stable/), [black](https://black.readthedocs.io/en/stable/), [pydocstyle](http://www.pydocstyle.org/en/stable/), and [pylint](https://www.pylint.org/) for testing.

1. [pytest](https://docs.pytest.org/en/stable/)
   1. Standard testing platform
   1. Also uses the coverage plugin
   1. Configuration can be found at `setup.cfg`
   1. Example: `python -m pytest -v`
1. [black](https://black.readthedocs.io/en/stable/)
   1. An opinionated autoformatter
   1. Example: `python -m black --line-length 90 --check covid_weekly`
   1. See `scripts/lint` for the exact command run
1. [pydocstyle](http://www.pydocstyle.org/en/stable/)
   1. Linter for docstring documentation
   1. This package uses [NumPy style](https://sphinxcontrib-napoleon.readthedocs.io/en/latest/example_numpy.html)
   1. Configuration can be found at `setup.cfg`
   1. Example: `python -m pydocstyle covid_weekly
   1. See `scripts/lint` for the exact command run`
1. [pylint](https://www.pylint.org/)
   1. The main linter used
   1. Configuration can be found at `.pylintrc`
   1. See `scripts/lint` for the exact command run`

## Developer Notes

- The author created this package not knowing how it would be used in the future, and so it has been written to be as flexible as possible
