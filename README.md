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

Two extensions beyond URDF and SDF: a sensor's `self_filter` (attribute or element: `mount`,
`robot` or `none`, what of its own robot it ignores; empty by default), and a stereo depth
camera's error model inside its `<camera>`:

```xml
<stereo baseline="0.05" disparity_noise="0.08" subpixel="0.08" min_depth="0.105"
        edge_threshold="0.05" edge_dropout="0.5" dropout="0" real_width="848" real_height="480"/>
```

(`StereoSpec`; values as attributes or child elements, absent ones at these defaults, an
Intel RealSense D435 at 848×480): depth from disparity `f·b/z` at the real imager's focal
length, Gaussian disparity noise and subpixel quantization (px), minimum depth (m), the
left imager's invalid band, and dropout at depth edges and anywhere.

A third: event cameras (dynamic vision sensors), `<sensor type="dvs">` (or `event_camera`),
with their image geometry in a `<camera>` block and their pixel model in `<dvs>` (or
`<event_camera>`, inside the sensor or its camera):

```xml
<dvs contrast_on="0.2" contrast_off="0.2" sigma_contrast="0.03" refractory="0.0005"
     cutoff_hz="300" leak_rate="0.1" leak_decades="0.1" shot_rate="0" shot_bright="0.25"
     jitter="0" log_eps="0.02" min_rate="50" adaptive="true" adapt_step="0.5" capacity="0"/>
```

(`EventCameraSpec`, `Sensor.events?`; absent values at these defaults, an iniVation DAVIS346
as v2e models it). `SensorKind` has no constructor for them, so that existing matches stay
exhaustive: they parse as `.other "dvs"`, and `SensorKind.isEventCamera` recognizes them.

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
