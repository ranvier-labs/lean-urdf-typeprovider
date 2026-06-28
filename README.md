# LeanUrdfTypeProvider

Small URDF type-provider experiment for Lean.

The first implementation uses Lean's parser-extension infrastructure directly:
`declare_syntax_cat`, `syntax`, and command elaborators parse a URDF-relevant XML
subset into typed Lean declarations.

```lean
import LeanUrdfTypeProvider

urdf_type_provider "path/to/robot.urdf" as RobotModel

#check RobotModel.model
#check RobotModel.linkNames
#check RobotModel.jointNames
```

Supported URDF subset:

- `robot`, `link`, `joint`
- `origin`, `axis`, `limit`
- `inertial`, `mass`, `inertia`
- `visual`, `collision`, `geometry`
- `box`, `cylinder`, `sphere`, `mesh`

The file parser accepts XML declarations, nested elements, empty element tags,
and string-valued attributes. It intentionally rejects mismatched tags and
invalid joint parent/child references during elaboration.

Tests use the local `~/dev/lean_test` package through Lake and keep fixtures in
`testdata/`.
