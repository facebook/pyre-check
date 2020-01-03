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


class Ignore:
    pass


def main() -> None:
    configuration_arguments = generate_taint_models.ConfigurationArguments(
        # These shouldn't need to be changed
        root=str(current_file.parent),
        stub_root=f"{current_file.parent.parent.parent}/stubs",
        # Set up your generators here
        urls_module="urls",
        url_pattern_type=UrlPattern,
        # These are just mandatory defaults; ignore them
        url_resolver_type=Ignore,
        annotation_specs=[],
        whitelisted_views=[],
        whitelisted_classes=[],
        graphql_object_type=Ignore,
        graphql_module=[],
        blacklisted_global_directories={},
        blacklisted_globals={},
    )

    generation_arguments = generate_taint_models.GenerationArguments(
        mode=["get_REST_api_sources"],
        verbose=True,
        output_directory=str(current_file.parent),
    )

    generate_taint_models.run_from_global_state(
        generation_arguments, configuration_arguments
    )


if __name__ == "__main__":
    main()
