"""Persist weekly COVID-19 data for Chicago."""
from . import logger

_logger = logger.create(__name__)


def to_local(df, to_filepath):
    """Persist to local."""
    _logger.info("Persisting %d zip codes to %s...", len(df), to_filepath)
    df.to_csv(to_filepath)
    return to_filepath


def to_s3():
    """Persist to AWS S3."""
    return


def to_gcs():
    """Persist to Google Cloud Storage."""
    return
