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

The same robots can be read at run time, from any URDF file:

```lean
let robot ← LeanUrdfTypeProvider.Robot.load "go2_description.urdf"
-- or Robot.parse : String → Except String Robot
```

The run-time XML reader (`LeanUrdfTypeProvider.Xml`) accepts what real descriptions
contain: comments, the XML declaration, `xmlns:` attributes, entities, single-quoted values
and exponents such as `3.12E-05`. `urdf_type_provider "file"` reads files with it too; the
inline `urdf_model` form keeps the syntax-category grammar. Generated declarations carry
floats bit-exactly. It reads the Unitree Go2, H1 and G1 descriptions of `unitree_ros`.

Supported URDF subset:

- `robot`, `link`, `joint`
- `origin`, `axis`, `limit`
- `inertial`, `mass`, `inertia`
- `visual`, `collision`, `geometry`
- `box`, `cylinder`, `sphere`, `mesh`
- `material` (robot level and inline, `color`, `texture`), joint `dynamics` and `mimic`
- sensors (`Robot.sensors`, `LeanUrdfTypeProvider.Sensor`), in both spellings: URDF's own
  `<sensor name update_rate>` with `<parent link>`, `<origin>`,
  `<camera><image width height hfov format near far/></camera>` and
  `<ray><horizontal …/><vertical …/></ray>` (urdfdom), and Gazebo's
  `<gazebo reference="link"><sensor type="camera|depth|rgbd_camera|ray|gpu_ray|gpu_lidar|imu|contact">`
  with `<update_rate>`, `<pose>`, `<camera>`, `<ray>`/`<lidar>` (`<scan>`, `<range>`),
  `<imu>`, `<contact>` and Gaussian `<noise>`

Sensors often live apart from a robot's description. An overlay is a `<robot>` file with
only sensors (or other parts) that name the base file's links:

```lean
let robot ← Robot.loadWithOverlays "go2_description.urdf" #["go2.sensors.urdf"]
-- or Robot.parseOverlay, Robot.merge (an overlay sensor replaces a base one of its name)
```

The file parser accepts XML declarations, nested elements, empty element tags,
and string-valued attributes. It intentionally rejects mismatched tags and
invalid joint parent/child references during elaboration.

Dependencies are pinned through Lake. Run the tests with:

```bash
lake test
```

Test fixtures live in `testdata/`.

## License

Licensed under the [Apache License, Version 2.0](LICENSE).
