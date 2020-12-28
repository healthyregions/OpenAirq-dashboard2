"""Parse weekly COVID-19 data for Chicago."""
import pandas as pd

from . import logger

_logger = logger.create(__name__)


def _collapse_hierarchical_columns(columns):
    return ["_".join(col) for col in columns]


def parse(file_object):
    df = pd.read_csv(file_object, parse_dates=["week_start", "week_end"])
    df["week_start"] = df["week_start"].dt.date.astype(str)
    df["week_end"] = df["week_end"].dt.date.astype(str)

    _logger.info(
        "Parsing %d rows from %s to %s...",
        len(df),
        df["week_start"].min(),
        df["week_end"].max(),
    )
    parsed = df.pivot(
        index="zip_code",
        columns="week_start",
        values=["cases_weekly", "cases_cumulative", "case_rate_weekly"],
    )
    parsed.columns = _collapse_hierarchical_columns(columns=parsed.columns)
    return parsed
