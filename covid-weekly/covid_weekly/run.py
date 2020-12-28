"""Download and persist weekly COVID-19 data for Chicago."""
import io

from . import get, logger, parse, persist

_logger = logger.create(__name__)


def run(app_token, persist_filepath):
    response = get(app_token=app_token)
    with io.BytesIO(response) as buffer:
        df = parse(buffer)
    persist.to_local(df=df, to_filepath=persist_filepath)
