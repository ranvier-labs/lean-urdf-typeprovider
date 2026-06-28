import Lean
import LeanUrdfTypeProvider.Runtime
import LeanUrdfTypeProvider.XmlSyntax

open Lean Elab Command

namespace LeanUrdfTypeProvider

open LeanUrdfTypeProvider.XmlSyntax

syntax (name := urdfInlineCmd)
  "urdf_model " ident " := " urdfXmlDocument : command

syntax (name := urdfFileCmd)
  "urdf_type_provider " str " as " ident : command

private structure XmlAttr where
  name : String
  value : String
  deriving Repr, Inhabited

private structure XmlElement where
  name : String
  attrs : Array XmlAttr := #[]
  children : Array XmlElement := #[]
  deriving Repr, Inhabited

private def attrValue? (elem : XmlElement) (name : String) : Option String :=
  elem.attrs.findSome? fun attr => if attr.name == name then some attr.value else none

private def requiredAttr (elem : XmlElement) (name : String) : Except String String :=
  match attrValue? elem name with
  | some value => pure value
  | none => .error s!"<{elem.name}> is missing required attribute '{name}'"

private def childElems (elem : XmlElement) (name : String) : Array XmlElement :=
  elem.children.filter (fun child => child.name == name)

private def firstChild? (elem : XmlElement) (name : String) : Option XmlElement :=
  elem.children.find? (fun child => child.name == name)

private def parseFloatLit? (s : String) : Option Float :=
  let trimmed := s.trimAscii.toString
  if trimmed.isEmpty then
    none
  else
    let negative := trimmed.startsWith "-"
    let body := if negative then (trimmed.drop 1).toString else trimmed
    let unsigned? :=
      match body.splitOn "." with
      | [whole] =>
          whole.toNat?.map Nat.toFloat
      | [whole, frac] =>
          match whole.toNat?, frac.toNat? with
          | some w, some f =>
              let denom : Float := (Nat.pow 10 frac.length).toFloat
              some (w.toFloat + f.toFloat / denom)
          | _, _ => none
      | _ => none
    unsigned?.map fun x => if negative then -x else x

private def parseFloat (ctx raw : String) : Except String Float :=
  match parseFloatLit? raw with
  | some value => pure value
  | none => .error s!"{ctx}: expected Float, got '{raw}'"

private def fields (raw : String) : Array String :=
  ((raw.splitOn " ").filter (fun part => !part.isEmpty)).toArray

private def parseVector3 (ctx raw : String) : Except String Vector3 := do
  let parts := fields raw
  if parts.size != 3 then
    .error s!"{ctx}: expected three floats, got '{raw}'"
  else
    pure {
      x := ← parseFloat s!"{ctx}.x" parts[0]!
      y := ← parseFloat s!"{ctx}.y" parts[1]!
      z := ← parseFloat s!"{ctx}.z" parts[2]!
    }

private def parseOptionalVector3 (ctx : String) (raw? : Option String) : Except String Vector3 := do
  match raw? with
  | none => pure {}
  | some raw => parseVector3 ctx raw

private def parseOrigin (elem : Option XmlElement) : Except String Origin := do
  match elem with
  | none => pure {}
  | some origin =>
      if origin.name != "origin" then
        .error s!"expected <origin>, got <{origin.name}>"
      pure {
        xyz := ← parseOptionalVector3 "origin.xyz" (attrValue? origin "xyz")
        rpy := ← parseOptionalVector3 "origin.rpy" (attrValue? origin "rpy")
      }

private def parseGeometry (elem : XmlElement) : Except String Geometry := do
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

private def parseVisual (elem : XmlElement) : Except String Visual := do
  let geometry ←
    match firstChild? elem "geometry" with
    | some geometry => parseGeometry geometry
    | none => pure (.unsupported "missing")
  pure {
    name := attrValue? elem "name"
    origin := ← parseOrigin (firstChild? elem "origin")
    geometry := geometry
  }

private def parseCollision (elem : XmlElement) : Except String Collision := do
  let geometry ←
    match firstChild? elem "geometry" with
    | some geometry => parseGeometry geometry
    | none => pure (.unsupported "missing")
  pure {
    name := attrValue? elem "name"
    origin := ← parseOrigin (firstChild? elem "origin")
    geometry := geometry
  }

private def parseInertia (elem : XmlElement) : Except String Inertia := do
  pure {
    ixx := ← parseFloat "inertia.ixx" (← requiredAttr elem "ixx")
    ixy := ← parseFloat "inertia.ixy" (← requiredAttr elem "ixy")
    ixz := ← parseFloat "inertia.ixz" (← requiredAttr elem "ixz")
    iyy := ← parseFloat "inertia.iyy" (← requiredAttr elem "iyy")
    iyz := ← parseFloat "inertia.iyz" (← requiredAttr elem "iyz")
    izz := ← parseFloat "inertia.izz" (← requiredAttr elem "izz")
  }

private def parseInertial (elem : XmlElement) : Except String Inertial := do
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

private def parseLink (elem : XmlElement) : Except String Link := do
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

private def parseLimit (elem : XmlElement) : Except String JointLimit := do
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

private def parseJoint (elem : XmlElement) : Except String Joint := do
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
  }

private def parseRobot (elem : XmlElement) : Except String Robot := do
  if elem.name != "robot" then
    .error s!"expected root <robot>, got <{elem.name}>"
  let robot : Robot := {
    name := ← requiredAttr elem "name"
    links := ← (childElems elem "link").mapM parseLink
    joints := ← (childElems elem "joint").mapM parseJoint
  }
  robot.validateTree
  pure robot

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

private def optionFloatExpr : Option Float → String
  | none => "none"
  | some value => s!"(some ({value} : Float) : Option Float)"

private def typedRecordExpr (typeName fields : String) : String :=
  "({ " ++ fields ++ " } : _root_.LeanUrdfTypeProvider." ++ typeName ++ ")"

private def vector3Expr (v : Vector3) : String :=
  typedRecordExpr "Vector3" s!"x := {v.x}, y := {v.y}, z := {v.z}"

private def originExpr (o : Origin) : String :=
  typedRecordExpr "Origin" s!"xyz := {vector3Expr o.xyz}, rpy := {vector3Expr o.rpy}"

private def inertiaExpr (i : Inertia) : String :=
  typedRecordExpr "Inertia"
    s!"ixx := {i.ixx}, ixy := {i.ixy}, ixz := {i.ixz}, iyy := {i.iyy}, iyz := {i.iyz}, izz := {i.izz}"

private def inertialExpr (i : Inertial) : String :=
  typedRecordExpr "Inertial"
    s!"origin := {originExpr i.origin}, mass := {i.mass}, inertia := {inertiaExpr i.inertia}"

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
  | .cylinder radius length => s!"_root_.LeanUrdfTypeProvider.Geometry.cylinder {radius} {length}"
  | .sphere radius => s!"_root_.LeanUrdfTypeProvider.Geometry.sphere {radius}"
  | .mesh filename none => s!"_root_.LeanUrdfTypeProvider.Geometry.mesh {leanStringLit filename}"
  | .mesh filename (some scale) => s!"_root_.LeanUrdfTypeProvider.Geometry.mesh {leanStringLit filename} (some {vector3Expr scale} : Option _root_.LeanUrdfTypeProvider.Vector3)"
  | .unsupported tag => s!"_root_.LeanUrdfTypeProvider.Geometry.unsupported {leanStringLit tag}"

private def visualExpr (v : Visual) : String :=
  typedRecordExpr "Visual"
    s!"name := {optionStringExpr v.name}, origin := {originExpr v.origin}, geometry := {geometryExpr v.geometry}"

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

private def jointExpr (joint : Joint) : String :=
  typedRecordExpr "Joint"
    s!"name := {leanStringLit joint.name}, jointType := {jointTypeExpr joint.jointType}, parent := {leanStringLit joint.parent}, child := {leanStringLit joint.child}, origin := {originExpr joint.origin}, axis := {vector3Expr joint.axis}, limit? := {optionLimitExpr joint.limit?}"

private def robotExpr (robot : Robot) : String :=
  typedRecordExpr "Robot"
    s!"name := {leanStringLit robot.name}, links := {arrayExpr (robot.links.map linkExpr)}, joints := {arrayExpr (robot.joints.map jointExpr)}"

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
    s!"def rootLinks : Array String := {arrayExpr (rootLinks.map leanStringLit)}"
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
      let sourcePath : System.FilePath := ⟨path.getString⟩
      let source ← IO.FS.readFile sourcePath
      let generated := s!"urdf_model {declName.getId} :=\n{source}\n"
      elabCommand (← parseGeneratedCommand generated)
  | _ => throwUnsupportedSyntax

end LeanUrdfTypeProvider
