import LeanUrdfTypeProvider.Runtime
import LeanUrdfTypeProvider.Xml

/-!
# From XML elements to a `Robot`

Shared by the compile-time provider (`urdf_type_provider`, `urdf_model`) and the run-time
loader (`Robot.parse`, `Robot.load`).
-/

namespace LeanUrdfTypeProvider

def attrValue? (elem : XmlElement) (name : String) : Option String := elem.attr? name
def requiredAttr (elem : XmlElement) (name : String) : Except String String := elem.requiredAttr name
def childElems (elem : XmlElement) (name : String) : Array XmlElement := elem.childrenNamed name
def firstChild? (elem : XmlElement) (name : String) : Option XmlElement := elem.child? name

/-- A decimal floating-point literal: optional sign, digits with an optional fraction
(`1.`, `.5`), and an optional exponent (`3.12E-05`). -/
def parseFloatLit? (s : String) : Option Float :=
  let cs := s.trimAscii.toString.toList
  let (negative, cs) := match cs with
    | '-' :: r => (true, r) | '+' :: r => (false, r) | r => (false, r)
  let digits (cs : List Char) : List Char × List Char := (cs.takeWhile Char.isDigit, cs.dropWhile Char.isDigit)
  let (whole, cs) := digits cs
  let (frac, cs) := match cs with
    | '.' :: r => digits r
    | r => ([], r)
  if whole.isEmpty && frac.isEmpty then none else
  let exp? : Option Int := match cs with
    | [] => some 0
    | c :: r =>
      if c == 'e' || c == 'E' then
        let (eneg, r) := match r with
          | '-' :: r => (true, r) | '+' :: r => (false, r) | r => (false, r)
        let (ds, rest) := digits r
        if ds.isEmpty || !rest.isEmpty then none
        else
          let e : Int := (String.ofList ds).toNat!
          some (if eneg then -e else e)
      else none
  exp?.map fun e =>
    let mantissa := (String.ofList (whole ++ frac)).toNat!
    let e : Int := e - (frac.length : Int)
    let x := if e < 0 then Float.ofScientific mantissa true e.natAbs else Float.ofScientific mantissa false e.natAbs
    if negative then -x else x

def parseFloat (ctx raw : String) : Except String Float :=
  match parseFloatLit? raw with
  | some value => pure value
  | none => .error s!"{ctx}: expected Float, got '{raw}'"

def fields (raw : String) : Array String :=
  ((raw.split Char.isWhitespace).toList.map (·.toString) |>.filter (!·.isEmpty)).toArray

def parseVector3 (ctx raw : String) : Except String Vector3 := do
  let parts := fields raw
  if parts.size != 3 then
    .error s!"{ctx}: expected three floats, got '{raw}'"
  else
    pure {
      x := ← parseFloat s!"{ctx}.x" parts[0]!
      y := ← parseFloat s!"{ctx}.y" parts[1]!
      z := ← parseFloat s!"{ctx}.z" parts[2]!
    }

def parseOptionalVector3 (ctx : String) (raw? : Option String) : Except String Vector3 := do
  match raw? with
  | none => pure {}
  | some raw => parseVector3 ctx raw

def parseOrigin (elem : Option XmlElement) : Except String Origin := do
  match elem with
  | none => pure {}
  | some origin =>
      if origin.name != "origin" then
        .error s!"expected <origin>, got <{origin.name}>"
      pure {
        xyz := ← parseOptionalVector3 "origin.xyz" (attrValue? origin "xyz")
        rpy := ← parseOptionalVector3 "origin.rpy" (attrValue? origin "rpy")
      }

def parseGeometry (elem : XmlElement) : Except String Geometry := do
  if elem.name != "geometry" then
    .error s!"expected <geometry>, got <{elem.name}>"
  match elem.children.toList with
  | [child] =>
      match child.name with
      | "box" =>
          pure (.box (← parseVector3 "box.size" (← requiredAttr child "size")))
      | "cylinder" =>
          pure (.cylinder
            (← parseFloat "cylinder.radius" (← requiredAttr child "radius"))
            (← parseFloat "cylinder.length" (← requiredAttr child "length")))
      | "sphere" =>
          pure (.sphere (← parseFloat "sphere.radius" (← requiredAttr child "radius")))
      | "mesh" =>
          let scale? ←
            match attrValue? child "scale" with
            | none => pure none
            | some raw => some <$> parseVector3 "mesh.scale" raw
          pure (.mesh (← requiredAttr child "filename") scale?)
      | tag =>
          pure (.unsupported tag)
  | [] => .error "<geometry> requires exactly one child"
  | _ => .error "<geometry> supports exactly one child in this provider"

def parseMaterial (elem : XmlElement) : Except String Material := do
  let rgba? ← match (elem.child? "color").bind (·.attr? "rgba") with
    | none => pure none
    | some raw =>
      let parts := fields raw
      if parts.size != 4 then .error s!"material.color: expected four floats, got '{raw}'"
      else some <$> parts.mapM (parseFloat "material.color")
  pure {
    name := (elem.attr? "name").getD ""
    rgba? := rgba?.map fun c => (c[0]!, c[1]!, c[2]!, c[3]!)
    texture? := (elem.child? "texture").bind (·.attr? "filename")
  }

def parseVisual (elem : XmlElement) : Except String Visual := do
  let geometry ←
    match firstChild? elem "geometry" with
    | some geometry => parseGeometry geometry
    | none => pure (.unsupported "missing")
  pure {
    name := attrValue? elem "name"
    origin := ← parseOrigin (firstChild? elem "origin")
    geometry := geometry
    material? := ← (firstChild? elem "material").mapM parseMaterial
  }

def parseCollision (elem : XmlElement) : Except String Collision := do
  let geometry ←
    match firstChild? elem "geometry" with
    | some geometry => parseGeometry geometry
    | none => pure (.unsupported "missing")
  pure {
    name := attrValue? elem "name"
    origin := ← parseOrigin (firstChild? elem "origin")
    geometry := geometry
  }

def parseInertia (elem : XmlElement) : Except String Inertia := do
  pure {
    ixx := ← parseFloat "inertia.ixx" (← requiredAttr elem "ixx")
    ixy := ← parseFloat "inertia.ixy" (← requiredAttr elem "ixy")
    ixz := ← parseFloat "inertia.ixz" (← requiredAttr elem "ixz")
    iyy := ← parseFloat "inertia.iyy" (← requiredAttr elem "iyy")
    iyz := ← parseFloat "inertia.iyz" (← requiredAttr elem "iyz")
    izz := ← parseFloat "inertia.izz" (← requiredAttr elem "izz")
  }

def parseInertial (elem : XmlElement) : Except String Inertial := do
  let mass ←
    match firstChild? elem "mass" with
    | some mass => parseFloat "mass.value" (← requiredAttr mass "value")
    | none => pure 0.0
  let inertia ←
    match firstChild? elem "inertia" with
    | some inertia => parseInertia inertia
    | none => pure {}
  pure {
    origin := ← parseOrigin (firstChild? elem "origin")
    mass := mass
    inertia := inertia
  }

def parseLink (elem : XmlElement) : Except String Link := do
  let inertial? ←
    match firstChild? elem "inertial" with
    | none => pure none
    | some inertial => some <$> parseInertial inertial
  pure {
    name := ← requiredAttr elem "name"
    inertial? := inertial?
    visuals := ← (childElems elem "visual").mapM parseVisual
    collisions := ← (childElems elem "collision").mapM parseCollision
  }

def parseLimit (elem : XmlElement) : Except String JointLimit := do
  let parse? (field : String) : Except String (Option Float) :=
    match attrValue? elem field with
    | none => pure none
    | some raw => some <$> parseFloat s!"limit.{field}" raw
  pure {
    lower? := ← parse? "lower"
    upper? := ← parse? "upper"
    effort? := ← parse? "effort"
    velocity? := ← parse? "velocity"
  }

def parseJoint (elem : XmlElement) : Except String Joint := do
  let parentElem ←
    match firstChild? elem "parent" with
    | some parent => pure parent
    | none => .error s!"joint '{(attrValue? elem "name").getD "?"}' is missing <parent>"
  let childElem ←
    match firstChild? elem "child" with
    | some child => pure child
    | none => .error s!"joint '{(attrValue? elem "name").getD "?"}' is missing <child>"
  let limit? ←
    match firstChild? elem "limit" with
    | none => pure none
    | some limit => some <$> parseLimit limit
  pure {
    name := ← requiredAttr elem "name"
    jointType := JointType.ofString (← requiredAttr elem "type")
    parent := ← requiredAttr parentElem "link"
    child := ← requiredAttr childElem "link"
    origin := ← parseOrigin (firstChild? elem "origin")
    axis := ← parseOptionalVector3 "axis.xyz" ((firstChild? elem "axis").bind (attrValue? · "xyz"))
    limit? := limit?
    dynamics? := ← (firstChild? elem "dynamics").mapM fun d => do
      let opt (field : String) : Except String Float :=
        match d.attr? field with
        | none => pure 0.0
        | some raw => parseFloat s!"dynamics.{field}" raw
      pure { damping := ← opt "damping", friction := ← opt "friction" }
    mimic? := (firstChild? elem "mimic").bind (·.attr? "joint")
  }

/-! ## Sensors

Both spellings in use: URDF's own `<sensor>` (urdfdom; values in attributes) and Gazebo's
SDF sensors inside `<gazebo reference="link">` (values in child elements' text). A value is
read from an attribute if present, else from the text of a child element of that name. -/

/-- A number from the attribute `name`, else from the text of the child `name`. -/
def numberOf? (ctx : String) (elem : XmlElement) (name : String) : Except String (Option Float) :=
  match (elem.attr? name).orElse fun _ => elem.textAt? [name] with
  | none => pure none
  | some raw => some <$> parseFloat s!"{ctx}.{name}" raw

def numberOr (ctx : String) (elem : XmlElement) (name : String) (d : Float) : Except String Float :=
  return (← numberOf? ctx elem name).getD d

def natOr (ctx : String) (elem : XmlElement) (name : String) (d : Nat) : Except String Nat := do
  match ← numberOf? ctx elem name with
  | none => pure d
  | some x =>
    if x < 0 || x != x.round then .error s!"{ctx}.{name}: expected a natural number, got {x}"
    else pure x.toUInt64.toNat

/-- Gaussian noise: `<noise type="gaussian"><mean/><stddev/></noise>` (or `mean`/`stddev`
attributes). Noise of other types is read for its mean and standard deviation alike. -/
def parseNoise (elem : XmlElement) : Except String SensorNoise := do
  pure { mean := ← numberOr "noise" elem "mean" 0, stddev := ← numberOr "noise" elem "stddev" 0 }

def parseNoiseIn? (elem : XmlElement) : Except String (Option SensorNoise) :=
  (elem.child? "noise").mapM parseNoise

/-- A camera: urdfdom's `<camera><image width height hfov format near far/></camera>` or
SDF's `<camera><horizontal_fov/><image><width/><height/><format/></image><clip><near/><far/></clip></camera>`. -/
def parseCamera (elem : XmlElement) : Except String CameraSpec := do
  let d : CameraSpec := {}
  let image := (elem.child? "image").getD {name := "image"}
  let clip := (elem.child? "clip").getD {name := "clip"}
  let hfov ← match ← numberOf? "camera.image" image "hfov" with
    | some h => pure h
    | none => numberOr "camera" elem "horizontal_fov" d.hfov
  pure {
    width := ← natOr "camera.image" image "width" d.width
    height := ← natOr "camera.image" image "height" d.height
    hfov
    format := ((image.attr? "format").orElse fun _ => image.textAt? ["format"]).getD d.format
    near := ← match ← numberOf? "camera.image" image "near" with
      | some n => pure n | none => numberOr "camera.clip" clip "near" d.near
    far := ← match ← numberOf? "camera.image" image "far" with
      | some f => pure f | none => numberOr "camera.clip" clip "far" d.far
    noise? := ← parseNoiseIn? elem
  }

def parseRayAxis (ctx : String) (elem : XmlElement) : Except String RayAxis := do
  pure {
    samples := ← natOr ctx elem "samples" 1
    resolution := ← numberOr ctx elem "resolution" 1
    minAngle := ← numberOr ctx elem "min_angle" 0
    maxAngle := ← numberOr ctx elem "max_angle" 0
  }

/-- A ray sensor: urdfdom's `<ray><horizontal …/><vertical …/></ray>` or SDF's
`<ray>`/`<lidar>` with `<scan><horizontal/><vertical/></scan>`, `<range><min/><max/>
<resolution/></range>` and `<noise>`. -/
def parseRay (elem : XmlElement) : Except String RaySpec := do
  let scan := (elem.child? "scan").getD elem
  let range := (elem.child? "range").getD {name := "range"}
  let axis (n : String) : Except String RayAxis :=
    match scan.child? n with
    | some a => parseRayAxis s!"ray.{n}" a
    | none => pure {}
  pure {
    horizontal := ← axis "horizontal"
    vertical := ← axis "vertical"
    minRange := ← numberOr "ray.range" range "min" 0
    maxRange := ← numberOr "ray.range" range "max" 0
    rangeResolution := ← numberOr "ray.range" range "resolution" 0
    noise? := ← parseNoiseIn? elem
  }

/-- The noise of the first axis (`<x>`) that has one, under `<angular_velocity>` or
`<linear_acceleration>`. -/
def axisNoise? (elem : XmlElement) (group : String) : Except String (Option SensorNoise) := do
  let some g := elem.child? group | return none
  for a in ["x", "y", "z"] do
    if let some n := (g.child? a).bind (·.child? "noise") then return some (← parseNoise n)
  parseNoiseIn? g

def parseImu (elem : XmlElement) : Except String ImuSpec := do
  pure { gyroNoise? := ← axisNoise? elem "angular_velocity", accelNoise? := ← axisNoise? elem "linear_acceleration" }

/-- SDF's `<pose>x y z roll pitch yaw</pose>`. -/
def parsePose (raw : String) : Except String Origin := do
  let parts := fields raw
  if parts.size != 6 then .error s!"pose: expected six floats, got '{raw}'"
  let v ← parts.mapM (parseFloat "pose")
  pure { xyz := ⟨v[0]!, v[1]!, v[2]!⟩, rpy := ⟨v[3]!, v[4]!, v[5]!⟩ }

/-- A `<sensor>` element. `reference`: the link of an enclosing `<gazebo reference>` block
(else `<parent link>` names it). -/
def parseSensor (elem : XmlElement) (reference : Option String := none) : Except String Sensor := do
  let name := (elem.attr? "name").getD ""
  let ctx := s!"sensor '{name}'"
  let parent ← match reference, (elem.child? "parent").bind (·.attr? "link") with
    | _, some p => pure p
    | some r, none => pure r
    | none, none => .error s!"{ctx}: no <parent link> and not in a <gazebo reference> block"
  let camera := elem.child? "camera"
  let ray := (elem.child? "ray").orElse fun _ => elem.child? "lidar"
  let rawType := (elem.attr? "type").getD
    (if camera.isSome then "camera" else if ray.isSome then "ray" else "")
  if rawType.isEmpty then .error s!"{ctx}: no type, <camera> or <ray>"
  let origin ← match elem.child? "origin", elem.textAt? ["pose"] with
    | some o, _ => parseOrigin (some o)
    | none, some p => parsePose p
    | none, none => pure {}
  let rate ← match elem.attr? "update_rate" with
    | some r => parseFloat s!"{ctx}.update_rate" r
    | none => numberOr ctx elem "update_rate" 0
  pure {
    name, parent, origin, rawType
    kind := SensorKind.ofString rawType
    updateRate := rate
    camera? := ← camera.mapM fun c => do
      -- A depth camera's own noise or clip may sit under <depth_camera>; the image is shared.
      parseCamera c
    ray? := ← ray.mapM parseRay
    imu? := ← (elem.child? "imu").mapM parseImu
    collisions := ((elem.child? "contact").map fun c => (c.childrenNamed "collision").map (·.text)).getD #[]
  }

/-- Every sensor of a `<robot>`: its `<sensor>` children and the `<sensor>`s of its
`<gazebo reference="link">` blocks. -/
def parseSensors (elem : XmlElement) : Except String (Array Sensor) := do
  let own ← (childElems elem "sensor").mapM (parseSensor ·)
  let mut gz := #[]
  for g in childElems elem "gazebo" do
    for sensor in childElems g "sensor" do
      gz := gz.push (← parseSensor sensor (g.attr? "reference"))
  return own ++ gz

/-- A `<robot>`'s parts without checking that they form a tree (an overlay may hold only
sensors that refer to links of another file). -/
def parseRobotParts (elem : XmlElement) : Except String Robot := do
  if elem.name != "robot" then
    .error s!"expected root <robot>, got <{elem.name}>"
  pure {
    name := (elem.attr? "name").getD ""
    links := ← (childElems elem "link").mapM parseLink
    joints := ← (childElems elem "joint").mapM parseJoint
    materials := ← (childElems elem "material").mapM parseMaterial
    sensors := ← parseSensors elem
  }

def parseRobot (elem : XmlElement) : Except String Robot := do
  let _ ← requiredAttr elem "name"
  let robot ← parseRobotParts elem
  robot.validateTree
  pure robot

/-- A robot from URDF text, read at run time. -/
def Robot.parse (source : String) : Except String Robot := do
  parseRobot (← Xml.parse source)

/-- A robot from a URDF file, read at run time. -/
def Robot.load (path : System.FilePath) : IO Robot := do
  match Robot.parse (← IO.FS.readFile path) with
  | .ok robot => pure robot
  | .error msg => throw (IO.userError s!"{path}: {msg}")

/-- An overlay: a `<robot>` whose links, joints, materials and sensors are added to another
robot's (typically only `<sensor>`s and `<gazebo>` sensor blocks naming its links). -/
def Robot.parseOverlay (source : String) : Except String Robot := do
  parseRobotParts (← Xml.parse source)

/-- `overlay`'s parts added to `base`'s; an overlay sensor replaces a base sensor of the same
name. Checked as a tree, so overlay sensors must name links of either. -/
def Robot.merge (base overlay : Robot) : Except String Robot := do
  let replaced := base.sensors.filter fun s => !overlay.sensors.any (·.name == s.name)
  let robot : Robot := { base with
    links := base.links ++ overlay.links, joints := base.joints ++ overlay.joints,
    materials := base.materials ++ overlay.materials, sensors := replaced ++ overlay.sensors }
  robot.validateTree
  pure robot

/-- A robot from a URDF file with overlay files merged in order (`Robot.merge`). -/
def Robot.loadWithOverlays (path : System.FilePath) (overlays : Array System.FilePath) : IO Robot := do
  let mut robot ← Robot.load path
  for o in overlays do
    let overlay ← match Robot.parseOverlay (← IO.FS.readFile o) with
      | .ok r => pure r
      | .error msg => throw (IO.userError s!"{o}: {msg}")
    robot ← match robot.merge overlay with
      | .ok r => pure r
      | .error msg => throw (IO.userError s!"{o} on {path}: {msg}")
  return robot

end LeanUrdfTypeProvider
