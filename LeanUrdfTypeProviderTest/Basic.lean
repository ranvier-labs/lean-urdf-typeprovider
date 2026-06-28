import LeanUrdfTypeProvider
import LeanTest

open LeanUrdfTypeProvider

namespace LeanUrdfTypeProviderTest.Basic

urdf_type_provider "testdata/two_link.urdf" as TwoLinkArm

@[test]
def testProviderCountsAndNames : IO Unit := do
  LeanTest.assertEqual TwoLinkArm.model.name "two_link_arm"
  LeanTest.assertEqual TwoLinkArm.model.links.size 2
  LeanTest.assertEqual TwoLinkArm.model.joints.size 1
  LeanTest.assertEqual TwoLinkArm.linkNames #["base_link", "forearm_link"]
  LeanTest.assertEqual TwoLinkArm.jointNames #["shoulder_yaw"]
  LeanTest.assertEqual TwoLinkArm.rootLinks #["base_link"]

@[test]
def testGeneratedLinkAndJointDeclarations : IO Unit := do
  LeanTest.assertEqual TwoLinkArm.base_linkLink.name "base_link"
  LeanTest.assertEqual TwoLinkArm.forearm_linkLink.name "forearm_link"
  LeanTest.assertEqual TwoLinkArm.shoulder_yawJoint.parent "base_link"
  LeanTest.assertEqual TwoLinkArm.shoulder_yawJoint.child "forearm_link"
  LeanTest.assertEqual TwoLinkArm.shoulder_yawJoint.axis
    ({ x := 0.0, y := 0.0, z := 1.0 } : Vector3)

@[test]
def testGeneratedRobotValidatesAsTree : IO Unit := do
  match TwoLinkArm.model.validateTree with
  | Except.ok () => pure ()
  | .error msg => LeanTest.fail s!"expected valid robot tree, got: {msg}"

end LeanUrdfTypeProviderTest.Basic
