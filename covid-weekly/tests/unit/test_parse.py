import pandas as pd

from covid_weekly.parse import _collapse_hierarchical_columns, parse


def test__collapse_hierarchical_columns():
    hierachical_columns = pd.MultiIndex.from_tuples(
        tuples=[("1", "A"), ("1", "B"), ("2", "A"), ("2", "B")]
    )
    actual = _collapse_hierarchical_columns(hierachical_columns)
    expected = ["1_A", "1_B", "2_A", "2_B"]
    assert set(actual) - set(expected) == set()


def test_parse():
    filepath = "tests/resources/test_api_response.csv"
    actual = parse(filepath)
    assert isinstance(actual, pd.core.frame.DataFrame)
    assert len(actual) == 1
    assert actual.index[0] == 60603
    assert (
        set(actual.columns)
        - set(
            [
                "cases_weekly_2020-03-22",
                "cases_weekly_2020-03-29",
                "cases_weekly_2020-04-05",
                "cases_weekly_2020-05-24",
                "cases_weekly_2020-05-31",
                "cases_cumulative_2020-03-22",
                "cases_cumulative_2020-03-29",
                "cases_cumulative_2020-04-05",
                "cases_cumulative_2020-05-24",
                "cases_cumulative_2020-05-31",
                "case_rate_weekly_2020-03-22",
                "case_rate_weekly_2020-03-29",
                "case_rate_weekly_2020-04-05",
                "case_rate_weekly_2020-05-24",
                "case_rate_weekly_2020-05-31",
            ]
        )
        == set()
    )
