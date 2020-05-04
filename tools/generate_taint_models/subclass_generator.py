from typing import Dict, Iterable, List, Optional

from ...api import query
from ...api.connection import PyreConnection


def get_all_subclasses_from_pyre(
    targets: Iterable[str], pyre_connection: PyreConnection
) -> Optional[Dict[str, List[str]]]:
    class_hierarchy = query.get_class_hierarchy(pyre_connection)
    if class_hierarchy is not None:
        subclass_generator = (
            (base_class, class_hierarchy.subclasses(base_class))
            for base_class in targets
        )
        return {
            base_class: subclasses
            for base_class, subclasses in subclass_generator
            if subclasses is not None
        }
    else:
        return None
