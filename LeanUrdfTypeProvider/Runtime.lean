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

structure Visual where
  name : Option String := none
  origin : Origin := {}
  geometry : Geometry := .unsupported "missing"
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

structure Joint where
  name : String
  jointType : JointType
  parent : String
  child : String
  origin : Origin := {}
  axis : Vector3 := { x := 1.0, y := 0.0, z := 0.0 }
  limit? : Option JointLimit := none
  deriving Repr, BEq, Inhabited

structure Robot where
  name : String
  links : Array Link := #[]
  joints : Array Joint := #[]
  deriving Repr, BEq, Inhabited

def JointType.ofString : String → JointType
  | "revolute" => .revolute
  | "continuous" => .continuous
  | "prismatic" => .prismatic
  | "fixed" => .fixed
  | "floating" => .floating
  | "planar" => .planar
  | raw => .unknown raw

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

def Robot.validateTree (robot : Robot) : Except String Unit := do
  for joint in robot.joints do
    if (robot.findLink? joint.parent).isNone then
      .error s!"joint '{joint.name}' references missing parent link '{joint.parent}'"
    if (robot.findLink? joint.child).isNone then
      .error s!"joint '{joint.name}' references missing child link '{joint.child}'"
  let roots := robot.rootLinks
  if roots.size == 0 then
    .error "robot has no root link"
  else
    pure ()

end LeanUrdfTypeProvider
