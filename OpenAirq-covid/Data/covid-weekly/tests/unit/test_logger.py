import io
import re

from covid_weekly import logger


def test_create():
    stream = io.StringIO()
    _logger = logger.create(name="tests.test_logger", stream=stream)
    _logger.info("this is a test message...")
    actual = stream.getvalue()
    assert bool(
        re.match(
            r"[A-Z]{4} \([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\) --"
            r" .+ -- .+",
            actual,
        )
    )
