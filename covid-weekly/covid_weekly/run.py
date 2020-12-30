"""Download and persist weekly COVID-19 data for Chicago."""
import io

from . import get, logger, parse, persist

_logger = logger.create(__name__)


def run(app_token, persist_filepath):
    """Download and persist to local file weekly COVID-19 data for the city of Chicago.

    Parameters
    ----------
    app_token : str
        Secret token for Socratica applications. See README.md#Secrets for more info.
    persist_filepath : str
        Local filepath to which to persist data.

    Returns
    -------
    str
        Local filepath pointing to persisted data.
    """
    response = get(app_token=app_token)
    with io.BytesIO(response) as buffer:
        df = parse(buffer)
    persist.to_local(df=df, to_filepath=persist_filepath)
    return persist_filepath
