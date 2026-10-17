namespace LeanUrdfTypeProvider

structure Vector3 where
  x : Float := 0.0
  y : Float := 0.0
  z : Float := 0.0
  deriving Repr, BEq, Inhabited

instance : ToString Vector3 where
  toString v := "{x := " ++ toString v.x ++ ", y := " ++ toString v.y ++ ", z := " ++ toString v.z ++ "}"

structure Origin where
  xyz : Vector3 := {}
  rpy : Vector3 := {}
  deriving Repr, BEq, Inhabited

structure Inertia where
  ixx : Float := 0.0
  ixy : Float := 0.0
  ixz : Float := 0.0
  iyy : Float := 0.0
  iyz : Float := 0.0
  izz : Float := 0.0
  deriving Repr, BEq, Inhabited

structure Inertial where
  origin : Origin := {}
  mass : Float := 0.0
  inertia : Inertia := {}
  deriving Repr, BEq, Inhabited

inductive Geometry where
  | box (size : Vector3)
  | cylinder (radius length : Float)
  | sphere (radius : Float)
  | mesh (filename : String) (scale : Option Vector3 := none)
  | unsupported (tag : String)
  deriving Repr, BEq, Inhabited

/-- A colour and/or texture, defined at robot level and referenced by name, or inline. -/
structure Material where
  name : String := ""
  rgba? : Option (Float × Float × Float × Float) := none
  texture? : Option String := none
  deriving Repr, BEq, Inhabited

structure Visual where
  name : Option String := none
  origin : Origin := {}
  geometry : Geometry := .unsupported "missing"
  material? : Option Material := none
  deriving Repr, BEq, Inhabited

structure Collision where
  name : Option String := none
  origin : Origin := {}
  geometry : Geometry := .unsupported "missing"
  deriving Repr, BEq, Inhabited

structure Link where
  name : String
  inertial? : Option Inertial := none
  visuals : Array Visual := #[]
  collisions : Array Collision := #[]
  deriving Repr, BEq, Inhabited

inductive JointType where
  | revolute
  | continuous
  | prismatic
  | fixed
  | floating
  | planar
  | unknown (raw : String)
  deriving Repr, BEq, Inhabited

structure JointLimit where
  lower? : Option Float := none
  upper? : Option Float := none
  effort? : Option Float := none
  velocity? : Option Float := none
  deriving Repr, BEq, Inhabited

structure JointDynamics where
  damping : Float := 0.0
  friction : Float := 0.0
  deriving Repr, BEq, Inhabited

structure Joint where
  name : String
  jointType : JointType
  parent : String
  child : String
  origin : Origin := {}
  axis : Vector3 := { x := 1.0, y := 0.0, z := 0.0 }
  limit? : Option JointLimit := none
  dynamics? : Option JointDynamics := none
  /-- The joint this one mimics, if any. -/
  mimic? : Option String := none
  deriving Repr, BEq, Inhabited

/-- What a sensor measures. `camera`: an image (`depth`: its depth image too, or only it);
`lidar`: ranges along rays; `imu`; `contact`. Gazebo's types map onto these: `camera`,
`depth`/`depthcamera`/`rgbd_camera`, `ray`/`gpu_ray`/`lidar`/`gpu_lidar`, `imu`, `contact`. -/
inductive SensorKind where
  | camera
  | depthCamera
  | rgbdCamera
  | lidar
  | imu
  | contact
  | other (raw : String)
  deriving Repr, BEq, Inhabited

def SensorKind.ofString : String → SensorKind
  | "camera" | "multicamera" | "wideanglecamera" => .camera
  | "depth" | "depthcamera" | "depth_camera" => .depthCamera
  | "rgbd" | "rgbd_camera" | "rgbdcamera" => .rgbdCamera
  | "ray" | "gpu_ray" | "lidar" | "gpu_lidar" => .lidar
  | "imu" => .imu
  | "contact" => .contact
  | raw => .other raw

/-- Event cameras (dynamic vision sensors) have no constructor of their own, so that matches
over `SensorKind` stay exhaustive: Gazebo has no standard type for them, and they are read as
`other` with a raw type of `dvs`, `event_camera`, `eventcamera` or `event` (any case). Their
image geometry is a `<camera>` block, their pixel model `Sensor.events?`. -/
def SensorKind.isEventCamera : SensorKind → Bool
  | .other raw => ["dvs", "event_camera", "eventcamera", "event", "event-camera"].contains raw.toLower
  | _ => false

/-- Additive Gaussian noise (Gazebo's `<noise type="gaussian">`). -/
structure SensorNoise where
  mean : Float := 0.0
  stddev : Float := 0.0
  deriving Repr, BEq, Inhabited

/--
A stereo depth camera's error model (an extension, `<camera><stereo …/></camera>`): depth
comes from disparity `d = f·b/z` at the real imager's focal length `f` (pixels, from
`realWidth` and the field of view) and `baseline` `b` (m), with Gaussian disparity noise
`disparityNoise` (px) and quantization to `subpixel` (px); no return below `minDepth` (m);
the left imager's band that the right one does not see (`x < f·b/z`) is invalid; pixels at
depth edges (relative step to a neighbour above `edgeThreshold`) drop out with probability
`edgeDropout`, any pixel with `dropout`. Defaults: an Intel RealSense D435 at 848×480.
-/
structure StereoSpec where
  baseline : Float := 0.05
  disparityNoise : Float := 0.08
  subpixel : Float := 0.08
  minDepth : Float := 0.105
  edgeThreshold : Float := 0.05
  edgeDropout : Float := 0.5
  dropout : Float := 0.0
  /-- The real imager's size (pixels); 0: the simulated image's. -/
  realWidth : Nat := 848
  realHeight : Nat := 480
  deriving Repr, BEq, Inhabited

/--
An event camera's pixel model (an extension, `<dvs …/>` or `<event_camera …/>` inside the
`<sensor>` or its `<camera>`), after ESIM (Rebecq et al. 2018) and v2e (Hu, Liu, Delbruck 2021):
a pixel compares its log intensity `L = log(I + logEps)` (low-passed at a cutoff that grows
with intensity) to a memorized reference and emits an ON (OFF) event when it rises by
`contrastOn` (falls by `contrastOff`), per-pixel thresholds drawn from a Gaussian of standard
deviation `sigmaContrast`; then it holds for `refractory` (s). Noise: leak events at
`leakRate` (Hz per pixel, spread lognormally by `leakDecades`), shot-noise events at
`shotRate` (Hz per pixel in the dark, times `shotBright` at full intensity), timestamp jitter
`jitter` (s). The renderer samples the scene at most at the sensor's update rate and at least
at `minRate` (Hz), adaptively (`adaptive`: the largest per-pixel log change per sample kept
near `adaptStep`·contrast); `capacity`: events stored per sample (0: 4 per pixel), more are
counted as dropped. Defaults: an iniVation DAVIS346 as v2e models it (thresholds 0.2, σ 0.03,
cutoff 300 Hz, leak 0.1 Hz).
-/
structure EventCameraSpec where
  contrastOn : Float := 0.2
  contrastOff : Float := 0.2
  sigmaContrast : Float := 0.03
  refractory : Float := 0.0005
  cutoffHz : Float := 300
  leakRate : Float := 0.1
  leakDecades : Float := 0.1
  shotRate : Float := 0.0
  shotBright : Float := 0.25
  jitter : Float := 0.0
  logEps : Float := 0.02
  minRate : Float := 50
  adaptive : Bool := true
  adaptStep : Float := 0.5
  capacity : Nat := 0
  deriving Repr, BEq, Inhabited

/-- A pinhole camera: image size, horizontal field of view (rad), pixel format, clip planes. -/
structure CameraSpec where
  width : Nat := 320
  height : Nat := 240
  hfov : Float := 1.047
  format : String := ""
  near : Float := 0.05
  far : Float := 100.0
  noise? : Option SensorNoise := none
  stereo? : Option StereoSpec := none
  deriving Repr, BEq, Inhabited

/-- One scan axis of a ray sensor: `samples` rays from `minAngle` to `maxAngle` (rad). -/
structure RayAxis where
  samples : Nat := 1
  resolution : Float := 1.0
  minAngle : Float := 0.0
  maxAngle : Float := 0.0
  deriving Repr, BEq, Inhabited

/-- A ray sensor (lidar): horizontal and vertical scan, range limits (m). -/
structure RaySpec where
  horizontal : RayAxis := {}
  vertical : RayAxis := {}
  minRange : Float := 0.0
  maxRange : Float := 0.0
  rangeResolution : Float := 0.0
  noise? : Option SensorNoise := none
  deriving Repr, BEq, Inhabited

/-- IMU noise: angular velocity (rad/s) and linear acceleration (m/s²). -/
structure ImuSpec where
  gyroNoise? : Option SensorNoise := none
  accelNoise? : Option SensorNoise := none
  deriving Repr, BEq, Inhabited

/--
A sensor attached to a link: URDF's `<sensor>` (urdfdom: `<parent link>`, `<origin>`,
`<camera><image …/></camera>`, `<ray><horizontal …/><vertical …/></ray>`) or Gazebo's
`<gazebo reference="link"><sensor type=…>` (`<pose>`, `<update_rate>`, `<camera>`, `<ray>`
or `<lidar>`, `<imu>`, `<contact>`). `origin` is the sensor frame in the parent link's frame.
-/
structure Sensor where
  name : String
  kind : SensorKind
  /-- The type as written (`type` attribute, or `camera`/`ray` from the child element). -/
  rawType : String := ""
  parent : String
  origin : Origin := {}
  /-- Updates per second (0: as often as possible). -/
  updateRate : Float := 0.0
  camera? : Option CameraSpec := none
  ray? : Option RaySpec := none
  imu? : Option ImuSpec := none
  /-- Contact sensors: the collision elements they report (Gazebo `<contact><collision>`). -/
  collisions : Array String := #[]
  /-- What of its own robot the sensor ignores (an extension: `self_filter` attribute or
  element): `mount` (the body it is mounted in, default when empty), `robot`, `none`. -/
  selfFilter : String := ""
  /-- Event cameras (`SensorKind.isEventCamera`): the pixel model, from `<dvs>` or
  `<event_camera>` in the sensor or its camera; the defaults when absent. -/
  events? : Option EventCameraSpec := none
  deriving Repr, BEq, Inhabited

structure Robot where
  name : String
  links : Array Link := #[]
  joints : Array Joint := #[]
  materials : Array Material := #[]
  sensors : Array Sensor := #[]
  deriving Repr, BEq, Inhabited

def JointType.ofString : String → JointType
  | "revolute" => .revolute
  | "continuous" => .continuous
  | "prismatic" => .prismatic
  | "fixed" => .fixed
  | "floating" => .floating
  | "planar" => .planar
  | raw => .unknown raw

/-- A visual's colour: its own, else the robot-level material of that name. -/
def Robot.visualColor? (robot : Robot) (v : Visual) : Option (Float × Float × Float × Float) :=
  v.material?.bind fun m => m.rgba?.orElse fun _ =>
    (robot.materials.find? (·.name == m.name)).bind (·.rgba?)

def Robot.linkNames (robot : Robot) : Array String :=
  robot.links.map (fun link => link.name)

def Robot.jointNames (robot : Robot) : Array String :=
  robot.joints.map (fun joint => joint.name)

def Robot.findLink? (robot : Robot) (name : String) : Option Link :=
  robot.links.find? (fun link => link.name == name)

def Robot.findJoint? (robot : Robot) (name : String) : Option Joint :=
  robot.joints.find? (fun joint => joint.name == name)

def Robot.rootLinks (robot : Robot) : Array String :=
  robot.links.foldl
    (fun acc link =>
      if robot.joints.any (fun joint => joint.child == link.name) then acc else acc.push link.name)
    #[]

def Robot.findSensor? (robot : Robot) (name : String) : Option Sensor :=
  robot.sensors.find? (fun sensor => sensor.name == name)

def Robot.validateTree (robot : Robot) : Except String Unit := do
  for joint in robot.joints do
    if (robot.findLink? joint.parent).isNone then
      .error s!"joint '{joint.name}' references missing parent link '{joint.parent}'"
    if (robot.findLink? joint.child).isNone then
      .error s!"joint '{joint.name}' references missing child link '{joint.child}'"
  for sensor in robot.sensors do
    if (robot.findLink? sensor.parent).isNone then
      .error s!"sensor '{sensor.name}' references missing parent link '{sensor.parent}'"
  let roots := robot.rootLinks
  if roots.size == 0 then
    .error "robot has no root link"
  else
    pure ()

end LeanUrdfTypeProvider
