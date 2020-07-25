import importlib
import sys
from pathlib import Path

from urls import UrlPattern


# Make sure we're able to import dependencies in 'pyre-check' repo, since they
# are not currently in the PyPI package for pyre-check
current_file = Path(__file__).absolute()
sys.path.append(str(current_file.parents[3]))

# Work around '-' in the name of 'pyre-check'
generate_taint_models = importlib.import_module(
    "pyre-check.tools.generate_taint_models"
)
view_generator = importlib.import_module(
    "pyre-check.tools.generate_taint_models.view_generator"
)


class Ignore:
    pass


def main() -> None:
    # Here, specify all the generators that you might want to call.
    generators = {
        "get_REST_api_sources": generate_taint_models.RESTApiSourceGenerator(
            django_urls=view_generator.DjangoUrls(
                urls_module="urls",
                url_pattern_type=UrlPattern,
                url_resolver_type=Ignore,
            )
        )
    }
    # The `run_generators` function will take care of parsing command-line arguments, as
    # well as executing the generators specified in `default_modes` unless you pass in a
    # specific set from the command line.
    generate_taint_models.run_generators(
        generators, default_modes=["get_REST_api_sources"]
    )


if __name__ == "__main__":
    main()
