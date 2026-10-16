import LeanUrdfTypeProvider
import LeanTest

open LeanUrdfTypeProvider

namespace LeanUrdfTypeProviderTest.Runtime

urdf_type_provider "testdata/realistic.urdf" as Realistic

def near (a b : Float) : Bool := (a - b).abs < 1e-12

@[test]
def testRuntimeLoaderReadsRealisticFile : IO Unit := do
  let robot ← Robot.load "testdata/realistic.urdf"
  LeanTest.assertEqual robot.name "quad & probe"
  LeanTest.assertEqual robot.linkNames #["world", "base", "calf"]
  LeanTest.assertEqual robot.rootLinks #["world"]
  let base := (robot.findLink? "base").get!
  let inertial := base.inertial?.get!
  LeanTest.assertTrue (near inertial.origin.xyz.x 0.021 && near inertial.origin.xyz.y (-0.001) && near inertial.origin.xyz.z 0.5)
  LeanTest.assertTrue (near inertial.inertia.ixy 1.2166e-4 && near inertial.inertia.iyz (-3.12e-5))
  LeanTest.assertTrue (robot.visualColor? base.visuals[0]! == some (0.1, 0.1, 0.1, 1.0))
  let knee := (robot.findJoint? "knee").get!
  LeanTest.assertTrue (knee.jointType == JointType.revolute)
  LeanTest.assertTrue (knee.dynamics? == some { damping := 0.01, friction := 0.2 })
  LeanTest.assertTrue ((robot.findJoint? "floating_base").get!.jointType == JointType.floating)

@[test]
def testProviderUsesRuntimeReader : IO Unit := do
  LeanTest.assertEqual Realistic.jointNames #["floating_base", "knee"]
  LeanTest.assertEqual Realistic.kneeJoint.limit?.get!.lower? (some (-2.7227))
  LeanTest.assertTrue ((← Robot.load "testdata/realistic.urdf") == Realistic.model) "runtime and compile-time models differ"

@[test]
def testFloatLiterals : IO Unit := do
  for (raw, value) in [("1", 1.0), ("-2.5", -2.5), (".5", 0.5), ("3.", 3.0), ("1e-7", 1e-7),
      ("3.12E-05", 3.12e-5), ("+4.0e+2", 400.0), ("-0", 0.0)] do
    LeanTest.assertTrue ((parseFloatLit? raw).any (near · value)) s!"{raw}"
  for raw in ["", "e5", "1e", "1.2.3", "abc", "--1"] do
    LeanTest.assertTrue (parseFloatLit? raw).isNone s!"{raw} should be rejected"

@[test]
def testXmlErrorsAreReported : IO Unit := do
  for bad in ["<robot name=\"a\">", "<robot name=\"a\"></link>", "<robot name=a/>", "<a/><b/>"] do
    match Xml.parse bad with
    | .ok _ => LeanTest.fail s!"accepted {bad}"
    | .error _ => pure ()

end LeanUrdfTypeProviderTest.Runtime
