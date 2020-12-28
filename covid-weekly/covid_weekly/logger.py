"""Custom logger for services."""
import logging
import sys


def create(name, level=logging.INFO, stream=None):
    """Create logger.

    Parameters
    ----------
    name : str
        Name of logger. Most often this is `__name__`
    level : int, optional
        Level of the logger, by default logging.INFO. Possible values are logging.
        CRITICAL (50), logging.ERROR (40), logging.WARNING (30), logging.INFO (20),
        logging.DEBUG (10), or logging.NOTSET (0).

    Returns
    -------
    logging.RootLogger
        Logging object.
    """
    if stream is None:
        stream = sys.stdout

    formatter = logging.Formatter(
        fmt="%(levelname)s (%(asctime)s) -- %(name)s -- %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )
    handler = logging.StreamHandler(stream=stream)
    handler.setFormatter(formatter)
    logger = logging.getLogger(name)
    logger.setLevel(level)
    logger.addHandler(handler)
    return logger
