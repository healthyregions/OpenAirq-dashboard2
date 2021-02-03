import os
import subprocess
import tempfile

import pandas as pd


def test_run_script():
    with tempfile.TemporaryDirectory() as temp_dir:
        filepath = os.path.join(temp_dir, "test.csv")
        subprocess.check_call(["run", "--filepath", filepath])
        assert os.path.exists(filepath)

        actual = pd.read_csv(filepath, index_col="zip_code")
        assert len(actual) >= 60  # zip codes
