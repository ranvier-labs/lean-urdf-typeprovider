import Lean
import LeanUrdfTypeProvider.Runtime
import LeanUrdfTypeProvider.Parse
import LeanUrdfTypeProvider.XmlSyntax

open Lean Elab Command

namespace LeanUrdfTypeProvider

open LeanUrdfTypeProvider.XmlSyntax

syntax (name := urdfInlineCmd)
  "urdf_model " ident " := " urdfXmlDocument : command

syntax (name := urdfFileCmd)
  "urdf_type_provider " str " as " ident : command

private partial def stxText (stx : Syntax) : String :=
  match stx with
  | .atom _ value => value
  | .ident _ _ value _ => value.toString
  | .node _ _ args => String.intercalate " " (args.toList.map stxText)
  | .missing => ""

private def attrOfSyntax (stx : Syntax) : CommandElabM XmlAttr := do
  match stx with
  | `(urdfXmlAttr| $name:ident = $value:str) =>
      pure { name := name.getId.toString, value := value.getString }
  | _ => throwError "invalid XML attribute syntax: {stx}"

private partial def elementOfSyntax (stx : Syntax) : CommandElabM XmlElement := do
  match stx with
  | `(urdfXmlElement| < $name:ident $attrs:urdfXmlAttr* />) =>
      let attrs ← attrs.mapM (fun attr => attrOfSyntax attr.raw)
      pure { name := name.getId.toString, attrs := attrs }
  | `(urdfXmlElement| < $name:ident $attrs:urdfXmlAttr* > $children:urdfXmlElement* </ $endName:ident >) =>
      let start := name.getId.toString
      let finish := endName.getId.toString
      if start != finish then
        throwError "XML start tag <{start}> does not match end tag </{finish}>"
      let attrs ← attrs.mapM (fun attr => attrOfSyntax attr.raw)
      let children ← children.mapM (fun child => elementOfSyntax child.raw)
      pure { name := start, attrs := attrs, children := children }
  | _ => throwError "invalid XML element syntax: {stxText stx}"

private def documentElementOfSyntax (stx : Syntax) : CommandElabM XmlElement := do
  match stx with
  | `(urdfXmlDocument| $elem:urdfXmlElement) =>
      elementOfSyntax elem.raw
  | `(urdfXmlDocument| <? xml $attrs:urdfXmlAttr* ?> $elem:urdfXmlElement) =>
      let _ ← attrs.mapM (fun attr => attrOfSyntax attr.raw)
      elementOfSyntax elem.raw
  | _ => throwError "invalid URDF XML document syntax: {stxText stx}"

private def leanKeywords : List String :=
  [ "abbrev", "axiom", "class", "def", "deriving", "do", "else", "end", "example", "forall"
  , "fun", "if", "import", "in", "inductive", "instance", "let", "macro", "match", "mutual"
  , "namespace", "noncomputable", "opaque", "open", "private", "protected", "section", "set_option"
  , "syntax", "term", "then", "theorem", "true", "false", "universe", "unsafe", "where", "with"
  ]

private def replaceInvalidChars (raw : String) : String :=
  String.ofList <| raw.toList.map fun c => if c.isAlphanum || c == '_' then c else '_'

private def ensureValidStart (raw : String) : String :=
  match raw.toList with
  | [] => "x"
  | c :: _ => if c.isAlpha || c == '_' then raw else s!"x_{raw}"

private def sanitizeIdent (raw fallback : String) : String :=
  let cleaned := replaceInvalidChars raw
  let cleaned := if cleaned.isEmpty then fallback else cleaned
  let cleaned := ensureValidStart cleaned
  if leanKeywords.contains cleaned then cleaned ++ "_" else cleaned

private partial def freshNameAux (used : List String) (base : String) (idx : Nat) : String :=
  let candidate := if idx == 0 then base else s!"{base}_{idx}"
  if used.contains candidate then freshNameAux used base (idx + 1) else candidate

private def freshName (used : List String) (base : String) : String :=
  freshNameAux used base 0

private def escapeLeanStringChars : List Char → List Char
  | [] => []
  | c :: cs =>
      let escaped :=
        match c with
        | '\"' => ['\\', '\"']
        | '\\' => ['\\', '\\']
        | '\n' => ['\\', 'n']
        | '\r' => ['\\', 'r']
        | '\t' => ['\\', 't']
        | other => [other]
      escaped ++ escapeLeanStringChars cs

private def leanStringLit (raw : String) : String :=
  "\"" ++ String.ofList (escapeLeanStringChars raw.toList) ++ "\""

private def optionStringExpr : Option String → String
  | none => "none"
  | some value => s!"some {leanStringLit value}"

/-- A float as Lean source, bit-exact: `toString` keeps six decimals, which turns a
`1e-7` inertia into zero. -/
private def floatExpr (value : Float) : String :=
  s!"(Float.ofBits {value.toBits})"

private def optionFloatExpr : Option Float → String
  | none => "none"
  | some value => s!"(some {floatExpr value} : Option Float)"

private def typedRecordExpr (typeName fields : String) : String :=
  "({ " ++ fields ++ " } : _root_.LeanUrdfTypeProvider." ++ typeName ++ ")"

private def vector3Expr (v : Vector3) : String :=
  typedRecordExpr "Vector3" s!"x := {floatExpr v.x}, y := {floatExpr v.y}, z := {floatExpr v.z}"

private def originExpr (o : Origin) : String :=
  typedRecordExpr "Origin" s!"xyz := {vector3Expr o.xyz}, rpy := {vector3Expr o.rpy}"

private def inertiaExpr (i : Inertia) : String :=
  typedRecordExpr "Inertia"
    s!"ixx := {floatExpr i.ixx}, ixy := {floatExpr i.ixy}, ixz := {floatExpr i.ixz}, iyy := {floatExpr i.iyy}, iyz := {floatExpr i.iyz}, izz := {floatExpr i.izz}"

private def inertialExpr (i : Inertial) : String :=
  typedRecordExpr "Inertial"
    s!"origin := {originExpr i.origin}, mass := {floatExpr i.mass}, inertia := {inertiaExpr i.inertia}"

private def optionInertialExpr : Option Inertial → String
  | none => "none"
  | some inertial => s!"(some {inertialExpr inertial} : Option _root_.LeanUrdfTypeProvider.Inertial)"

private def jointTypeExpr : JointType → String
  | .revolute => "_root_.LeanUrdfTypeProvider.JointType.revolute"
  | .continuous => "_root_.LeanUrdfTypeProvider.JointType.continuous"
  | .prismatic => "_root_.LeanUrdfTypeProvider.JointType.prismatic"
  | .fixed => "_root_.LeanUrdfTypeProvider.JointType.fixed"
  | .floating => "_root_.LeanUrdfTypeProvider.JointType.floating"
  | .planar => "_root_.LeanUrdfTypeProvider.JointType.planar"
  | .unknown raw => s!"_root_.LeanUrdfTypeProvider.JointType.unknown {leanStringLit raw}"

private def geometryExpr : Geometry → String
  | .box size => s!"_root_.LeanUrdfTypeProvider.Geometry.box {vector3Expr size}"
  | .cylinder radius length => s!"_root_.LeanUrdfTypeProvider.Geometry.cylinder {floatExpr radius} {floatExpr length}"
  | .sphere radius => s!"_root_.LeanUrdfTypeProvider.Geometry.sphere {floatExpr radius}"
  | .mesh filename none => s!"_root_.LeanUrdfTypeProvider.Geometry.mesh {leanStringLit filename}"
  | .mesh filename (some scale) => s!"_root_.LeanUrdfTypeProvider.Geometry.mesh {leanStringLit filename} (some {vector3Expr scale} : Option _root_.LeanUrdfTypeProvider.Vector3)"
  | .unsupported tag => s!"_root_.LeanUrdfTypeProvider.Geometry.unsupported {leanStringLit tag}"

private def materialExpr (m : Material) : String :=
  let rgba := match m.rgba? with
    | none => "none"
    | some (r, g, b, a) => s!"(some ({floatExpr r}, {floatExpr g}, {floatExpr b}, {floatExpr a}) : Option (Float × Float × Float × Float))"
  typedRecordExpr "Material"
    s!"name := {leanStringLit m.name}, rgba? := {rgba}, texture? := {optionStringExpr m.texture?}"

private def optionMaterialExpr : Option Material → String
  | none => "none"
  | some m => s!"(some {materialExpr m} : Option _root_.LeanUrdfTypeProvider.Material)"

private def visualExpr (v : Visual) : String :=
  typedRecordExpr "Visual"
    s!"name := {optionStringExpr v.name}, origin := {originExpr v.origin}, geometry := {geometryExpr v.geometry}, material? := {optionMaterialExpr v.material?}"

private def collisionExpr (c : Collision) : String :=
  typedRecordExpr "Collision"
    s!"name := {optionStringExpr c.name}, origin := {originExpr c.origin}, geometry := {geometryExpr c.geometry}"

private def arrayExpr (items : Array String) : String :=
  "#[" ++ String.intercalate ", " items.toList ++ "]"

private def linkExpr (link : Link) : String :=
  typedRecordExpr "Link"
    s!"name := {leanStringLit link.name}, inertial? := {optionInertialExpr link.inertial?}, visuals := {arrayExpr (link.visuals.map visualExpr)}, collisions := {arrayExpr (link.collisions.map collisionExpr)}"

private def jointLimitExpr (limit : JointLimit) : String :=
  typedRecordExpr "JointLimit"
    s!"lower? := {optionFloatExpr limit.lower?}, upper? := {optionFloatExpr limit.upper?}, effort? := {optionFloatExpr limit.effort?}, velocity? := {optionFloatExpr limit.velocity?}"

private def optionLimitExpr : Option JointLimit → String
  | none => "none"
  | some limit => s!"(some {jointLimitExpr limit} : Option _root_.LeanUrdfTypeProvider.JointLimit)"

private def dynamicsExpr : Option JointDynamics → String
  | none => "none"
  | some d => s!"(some {typedRecordExpr "JointDynamics" s!"damping := {floatExpr d.damping}, friction := {floatExpr d.friction}"} : Option _root_.LeanUrdfTypeProvider.JointDynamics)"

private def jointExpr (joint : Joint) : String :=
  typedRecordExpr "Joint"
    s!"name := {leanStringLit joint.name}, jointType := {jointTypeExpr joint.jointType}, parent := {leanStringLit joint.parent}, child := {leanStringLit joint.child}, origin := {originExpr joint.origin}, axis := {vector3Expr joint.axis}, limit? := {optionLimitExpr joint.limit?}, dynamics? := {dynamicsExpr joint.dynamics?}, mimic? := {optionStringExpr joint.mimic?}"

private def optionExpr (typeName : String) (f : α → String) : Option α → String
  | none => "none"
  | some x => s!"(some {f x} : Option _root_.LeanUrdfTypeProvider.{typeName})"

private def sensorKindExpr : SensorKind → String
  | .camera => "_root_.LeanUrdfTypeProvider.SensorKind.camera"
  | .depthCamera => "_root_.LeanUrdfTypeProvider.SensorKind.depthCamera"
  | .rgbdCamera => "_root_.LeanUrdfTypeProvider.SensorKind.rgbdCamera"
  | .lidar => "_root_.LeanUrdfTypeProvider.SensorKind.lidar"
  | .imu => "_root_.LeanUrdfTypeProvider.SensorKind.imu"
  | .contact => "_root_.LeanUrdfTypeProvider.SensorKind.contact"
  | .other raw => s!"_root_.LeanUrdfTypeProvider.SensorKind.other {leanStringLit raw}"

private def noiseExpr (n : SensorNoise) : String :=
  typedRecordExpr "SensorNoise" s!"mean := {floatExpr n.mean}, stddev := {floatExpr n.stddev}"

private def stereoExpr (t : StereoSpec) : String :=
  typedRecordExpr "StereoSpec"
    s!"baseline := {floatExpr t.baseline}, disparityNoise := {floatExpr t.disparityNoise}, subpixel := {floatExpr t.subpixel}, minDepth := {floatExpr t.minDepth}, edgeThreshold := {floatExpr t.edgeThreshold}, edgeDropout := {floatExpr t.edgeDropout}, dropout := {floatExpr t.dropout}, realWidth := {t.realWidth}, realHeight := {t.realHeight}"

private def eventsExpr (e : EventCameraSpec) : String :=
  typedRecordExpr "EventCameraSpec"
    s!"contrastOn := {floatExpr e.contrastOn}, contrastOff := {floatExpr e.contrastOff}, sigmaContrast := {floatExpr e.sigmaContrast}, refractory := {floatExpr e.refractory}, cutoffHz := {floatExpr e.cutoffHz}, leakRate := {floatExpr e.leakRate}, leakDecades := {floatExpr e.leakDecades}, shotRate := {floatExpr e.shotRate}, shotBright := {floatExpr e.shotBright}, jitter := {floatExpr e.jitter}, logEps := {floatExpr e.logEps}, minRate := {floatExpr e.minRate}, adaptive := {if e.adaptive then "true" else "false"}, adaptStep := {floatExpr e.adaptStep}, capacity := {e.capacity}"

private def cameraExpr (c : CameraSpec) : String :=
  typedRecordExpr "CameraSpec"
    s!"width := {c.width}, height := {c.height}, hfov := {floatExpr c.hfov}, format := {leanStringLit c.format}, near := {floatExpr c.near}, far := {floatExpr c.far}, noise? := {optionExpr "SensorNoise" noiseExpr c.noise?}, stereo? := {optionExpr "StereoSpec" stereoExpr c.stereo?}"

private def rayAxisExpr (a : RayAxis) : String :=
  typedRecordExpr "RayAxis"
    s!"samples := {a.samples}, resolution := {floatExpr a.resolution}, minAngle := {floatExpr a.minAngle}, maxAngle := {floatExpr a.maxAngle}"

private def rayExpr (r : RaySpec) : String :=
  typedRecordExpr "RaySpec"
    s!"horizontal := {rayAxisExpr r.horizontal}, vertical := {rayAxisExpr r.vertical}, minRange := {floatExpr r.minRange}, maxRange := {floatExpr r.maxRange}, rangeResolution := {floatExpr r.rangeResolution}, noise? := {optionExpr "SensorNoise" noiseExpr r.noise?}"

private def imuExpr (i : ImuSpec) : String :=
  typedRecordExpr "ImuSpec"
    s!"gyroNoise? := {optionExpr "SensorNoise" noiseExpr i.gyroNoise?}, accelNoise? := {optionExpr "SensorNoise" noiseExpr i.accelNoise?}"

private def sensorExpr (s : Sensor) : String :=
  typedRecordExpr "Sensor"
    s!"name := {leanStringLit s.name}, kind := {sensorKindExpr s.kind}, rawType := {leanStringLit s.rawType}, parent := {leanStringLit s.parent}, origin := {originExpr s.origin}, updateRate := {floatExpr s.updateRate}, camera? := {optionExpr "CameraSpec" cameraExpr s.camera?}, ray? := {optionExpr "RaySpec" rayExpr s.ray?}, imu? := {optionExpr "ImuSpec" imuExpr s.imu?}, collisions := ({arrayExpr (s.collisions.map leanStringLit)} : Array String), selfFilter := {leanStringLit s.selfFilter}, events? := {optionExpr "EventCameraSpec" eventsExpr s.events?}"

private def robotExpr (robot : Robot) : String :=
  typedRecordExpr "Robot"
    s!"name := {leanStringLit robot.name}, links := {arrayExpr (robot.links.map linkExpr)}, joints := {arrayExpr (robot.joints.map jointExpr)}, materials := {arrayExpr (robot.materials.map materialExpr)}, sensors := {arrayExpr (robot.sensors.map sensorExpr)}"

private def parseGeneratedCommand (source : String) : CommandElabM Syntax := do
  let env ← getEnv
  match Parser.runParserCategory env `command source "<urdf_type_provider>" with
  | .ok stx => pure stx
  | .error err =>
      throwError "URDF type provider generated invalid Lean command:\n{err}\n\n{source}"

private def elabGeneratedCommand (source : String) : CommandElabM Unit := do
  elabCommand (← parseGeneratedCommand source)

private def renderProviderCommands (nsName : Name) (robot : Robot) : Array String :=
  let linkNames := robot.links.map (fun link => link.name)
  let jointNames := robot.joints.map (fun joint => joint.name)
  let rootLinks := robot.rootLinks
  let linkDecls := Id.run do
    let mut used : List String := []
    let mut out : Array String := #[]
    for link in robot.links do
      let decl := freshName used (sanitizeIdent link.name "link")
      used := decl :: used
      out := out.push s!"def {decl}Link : _root_.LeanUrdfTypeProvider.Link := {linkExpr link}\n"
    out
  let jointDecls := Id.run do
    let mut used : List String := []
    let mut out : Array String := #[]
    for joint in robot.joints do
      let decl := freshName used (sanitizeIdent joint.name "joint")
      used := decl :: used
      out := out.push s!"def {decl}Joint : _root_.LeanUrdfTypeProvider.Joint := {jointExpr joint}\n"
    out
  #[
    "namespace " ++ nsName.toString,
    s!"def model : _root_.LeanUrdfTypeProvider.Robot := {robotExpr robot}",
    s!"abbrev LinkCount : Nat := {robot.links.size}",
    s!"abbrev JointCount : Nat := {robot.joints.size}",
    s!"def linkNames : Array String := {arrayExpr (linkNames.map leanStringLit)}",
    s!"def jointNames : Array String := {arrayExpr (jointNames.map leanStringLit)}",
    s!"def rootLinks : Array String := {arrayExpr (rootLinks.map leanStringLit)}",
    s!"def sensorNames : Array String := {arrayExpr (robot.sensors.map (leanStringLit ·.name))}"
  ] ++ linkDecls ++ jointDecls ++ #["end " ++ nsName.toString]

private def elaborateRobotSyntax (declName : Ident) (doc : Syntax) : CommandElabM Unit := do
  let elem ← documentElementOfSyntax doc
  let robot ←
    match parseRobot elem with
    | .ok robot => pure robot
    | .error msg => throwError msg
  for command in renderProviderCommands declName.getId robot do
    elabGeneratedCommand command

@[command_elab urdfInlineCmd]
def elabUrdfInline : CommandElab := fun stx => do
  match stx with
  | `(urdf_model $declName:ident := $doc:urdfXmlDocument) =>
      elaborateRobotSyntax declName doc.raw
  | _ => throwUnsupportedSyntax

@[command_elab urdfFileCmd]
def elabUrdfFile : CommandElab := fun stx => do
  match stx with
  | `(urdf_type_provider $path:str as $declName:ident) =>
      -- Read with the run-time XML reader, which accepts what real URDF files contain
      -- (comments, exponents, namespaced attributes); the inline form keeps the grammar.
      let sourcePath : System.FilePath := ⟨path.getString⟩
      let robot ← match Robot.parse (← IO.FS.readFile sourcePath) with
        | .ok robot => pure robot
        | .error msg => throwError "{sourcePath}: {msg}"
      for command in renderProviderCommands declName.getId robot do
        elabGeneratedCommand command
  | _ => throwUnsupportedSyntax

end LeanUrdfTypeProvider
