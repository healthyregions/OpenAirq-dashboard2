"""Parse weekly COVID-19 data for Chicago."""
import pandas as pd

from . import logger

_logger = logger.create(__name__)


def _collapse_hierarchical_columns(columns):
    """Collapse hierarchical columns by concatenating them together.

    Parameters
    ----------
    columns : pd.core.indexes.base.Index
        Hierarchical column index of a pandas dataframe.

    Examples
    --------
    {"cases_weekly": ["2020-12-01", "2020-12-02", ...], ...} ->
        ["cases_weekly_2020-12-01", "cases_weekly_2020-12-02", ...]

    Returns
    -------
    list of str
    """
    return ["_".join(col) for col in columns]


def parse(file_object):
    """Parse data at `file_object` from tall to wide.

    Parameters
    ----------
    file_object : file-like object

    Returns
    -------
    pd.core.frame.DataFrame
        rows : ZIP codes
        columns : [
            "cases_weekly_2020-03-01",
            "cases_weekly_2020-03-08",
            ...,
            "cases_cumulative_2020-03-01",
            "cases_cumulative_2020-03-08",
            ...,
            "case_rate_weekly_2020-03-01",
            "case_rate_weekly_2020-03-08",
        ]
    """
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
