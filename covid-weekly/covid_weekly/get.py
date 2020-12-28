"""Download weekly COVID-19 data for Chicago from Socratica."""
import requests

from . import logger

RESPONSE_LIMIT = int(1e9)
API_ENDPOINT = "https://data.cityofchicago.org/resource/yhhz-zm2v.csv"

_logger = logger.create(__name__)


def get(app_token):
    _logger.info("Requesing %s...", API_ENDPOINT)
    response = requests.get(
        API_ENDPOINT,
        headers={"X-App-Token": app_token},
        params={"$limit": RESPONSE_LIMIT}
    )
    response.raise_for_status()
    return response.content
