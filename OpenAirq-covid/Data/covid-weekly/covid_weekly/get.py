"""Download weekly COVID-19 data for Chicago from Socratica."""
import requests

from . import logger

RESPONSE_LIMIT = int(1e9)
API_ENDPOINT = "https://data.cityofchicago.org/resource/yhhz-zm2v.csv"

_logger = logger.create(__name__)


def get(app_token, response_limit=RESPONSE_LIMIT):
    """Retrieve weekly COVID-19 data for the city of Chicago.

    Data is published by the City of Chicago via Socratica. The data can be found here
     https://dev.socrata.com/foundry/data.cityofchicago.org/yhhz-zm2v.

    Parameters
    ----------
    app_token : str
        Secret token for Socratica applications. See README.md#Secrets for more info.
    response_limit : int
        Max number of rows to allow in the response.

    Returns
    -------
    bytes
        Byte-encoded string containing weekly COVID-19 data for the city of Chicago.
    """
    _logger.info("Requesing %s...", API_ENDPOINT)
    response = requests.get(
        API_ENDPOINT,
        headers={"X-App-Token": app_token},
        params={"$limit": response_limit},
    )
    response.raise_for_status()
    return response.content
