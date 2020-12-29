import os
import tempfile

import pandas as pd

from covid_weekly import run


SOCRATICA_APP_TOKEN = os.environ["SOCRATICA_APP_TOKEN"]


def test_run():
    with tempfile.TemporaryDirectory() as temp_dir:
        filepath = os.path.join(temp_dir, "test.csv")
        run(app_token=SOCRATICA_APP_TOKEN, persist_filepath=filepath)
        actual = pd.read_csv(filepath, index_col="zip_code")
        assert len(actual) >= 60  # 60 zip codes
