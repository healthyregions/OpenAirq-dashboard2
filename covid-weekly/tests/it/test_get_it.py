import io
import os

import pandas as pd

from covid_weekly import get


SOCRATICA_APP_TOKEN = os.environ["SOCRATICA_APP_TOKEN"]


def test_get():
    response = get(app_token=SOCRATICA_APP_TOKEN, response_limit=5)
    actual = pd.read_csv(io.BytesIO(response))
    assert len(actual) == 5
