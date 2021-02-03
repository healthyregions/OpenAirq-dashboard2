import os
import tempfile

import pandas as pd

from covid_weekly import persist


def test_to_local():
    filename = "test.csv"
    original_df = pd.DataFrame({"a": [1, 2, 3]})
    original_df.index.name = "index"
    with tempfile.TemporaryDirectory() as temp_dir:
        filepath = os.path.join(temp_dir, filename)
        persist.to_local(df=original_df, to_filepath=filepath)
        assert os.path.exists(filepath)
        actual = pd.read_csv(filepath, index_col="index")
        assert actual.equals(original_df)
