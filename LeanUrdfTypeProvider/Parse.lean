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

def parseRobot (elem : XmlElement) : Except String Robot := do
  if elem.name != "robot" then
    .error s!"expected root <robot>, got <{elem.name}>"
  let robot : Robot := {
    name := ← requiredAttr elem "name"
    links := ← (childElems elem "link").mapM parseLink
    joints := ← (childElems elem "joint").mapM parseJoint
    materials := ← (childElems elem "material").mapM parseMaterial
  }
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

end LeanUrdfTypeProvider
