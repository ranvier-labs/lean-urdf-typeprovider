import LeanUrdfTypeProvider
import LeanTest

open LeanUrdfTypeProvider

namespace LeanUrdfTypeProviderTest.Sensors

urdf_type_provider "testdata/sensors.urdf" as Sensed

def near (a b : Float) : Bool := (a - b).abs < 1e-9

@[test]
def testUrdfdomSensors : IO Unit := do
  let robot ← Robot.load "testdata/sensors.urdf"
  LeanTest.assertEqual (robot.sensors.map (·.name)) #["front_camera", "planar_scan", "depth", "mid360", "imu", "foot_contact"]
  let cam := (robot.findSensor? "front_camera").get!
  LeanTest.assertTrue (cam.kind == .camera && cam.parent == "head" && near cam.updateRate 20)
  LeanTest.assertTrue (near cam.origin.xyz.x 0.05 && near cam.origin.rpy.y 0.2)
  let c := cam.camera?.get!
  LeanTest.assertTrue (c.width == 640 && c.height == 480 && near c.hfov 1.5708 && c.format == "RGB8" && near c.near 0.01 && near c.far 50)
  let scan := (robot.findSensor? "planar_scan").get!
  LeanTest.assertTrue (scan.kind == .lidar && scan.rawType == "ray")
  let r := scan.ray?.get!
  LeanTest.assertTrue (r.horizontal.samples == 360 && near r.horizontal.minAngle (-3.14159) && r.vertical.samples == 1)

@[test]
def testGazeboSensors : IO Unit := do
  let robot ← Robot.load "testdata/sensors.urdf"
  let depth := (robot.findSensor? "depth").get!
  LeanTest.assertTrue (depth.kind == .depthCamera && depth.parent == "head" && near depth.updateRate 30)
  LeanTest.assertTrue (near depth.origin.xyz.y 0.02)
  let c := depth.camera?.get!
  LeanTest.assertTrue (c.width == 848 && c.height == 480 && near c.hfov 1.5184 && near c.near 0.105 && near c.far 10)
  LeanTest.assertTrue (c.noise? == some { mean := 0, stddev := 0.007 })
  let want : StereoSpec := {
    baseline := 0.055, disparityNoise := 0.1, subpixel := 0.125, minDepth := 0.28,
    edgeThreshold := 0.04, edgeDropout := 0.6, dropout := 0, realWidth := 1280, realHeight := 720 }
  LeanTest.assertTrue (c.stereo? == some want)
  LeanTest.assertEqual depth.selfFilter ""
  let lidar := (robot.findSensor? "mid360").get!
  LeanTest.assertTrue (lidar.kind == .lidar && lidar.rawType == "gpu_lidar" && lidar.parent == "base")
  let r := lidar.ray?.get!
  LeanTest.assertTrue (r.horizontal.samples == 360 && near r.horizontal.maxAngle 6.2832)
  LeanTest.assertTrue (r.vertical.samples == 32 && near r.vertical.minAngle (-0.1222) && near r.vertical.maxAngle 0.9076)
  LeanTest.assertTrue (near r.minRange 0.1 && near r.maxRange 40 && near r.rangeResolution 0.01)
  LeanTest.assertTrue (r.noise?.map (·.stddev) == some 0.02)
  LeanTest.assertEqual lidar.selfFilter "robot"
  let imu := (robot.findSensor? "imu").get!
  LeanTest.assertTrue (imu.kind == .imu && near imu.updateRate 500)
  LeanTest.assertTrue (imu.imu?.get!.gyroNoise?.map (·.stddev) == some 2e-4)
  LeanTest.assertTrue (imu.imu?.get!.accelNoise?.map (·.stddev) == some 1.7e-2)
  let contact := (robot.findSensor? "foot_contact").get!
  LeanTest.assertTrue (contact.kind == .contact && contact.parent == "foot" && contact.collisions == #["foot_collision"])

@[test]
def testProviderCarriesSensors : IO Unit := do
  LeanTest.assertEqual Sensed.sensorNames #["front_camera", "planar_scan", "depth", "mid360", "imu", "foot_contact"]
  LeanTest.assertTrue ((← Robot.load "testdata/sensors.urdf") == Sensed.model) "runtime and compile-time models differ"

@[test]
def testOverlay : IO Unit := do
  let robot ← Robot.loadWithOverlays "testdata/two_link.urdf" #["testdata/sensors_overlay.urdf"]
  let base ← Robot.load "testdata/two_link.urdf"
  LeanTest.assertEqual robot.linkNames base.linkNames
  LeanTest.assertEqual (robot.sensors.map (·.name)) #["base_imu", "tip_camera"]
  let cam := (robot.findSensor? "tip_camera").get!
  LeanTest.assertTrue (cam.parent == "forearm_link" && near cam.origin.xyz.z 0.5 && near cam.origin.rpy.y 1.5708)
  LeanTest.assertTrue (cam.camera?.get!.width == 96)
  -- A sensor on a link the base does not have is rejected.
  let bad := "<robot name=\"x\"><sensor name=\"s\" type=\"imu\"><parent link=\"nowhere\"/></sensor></robot>"
  match (Robot.parseOverlay bad).bind base.merge with
  | .ok _ => LeanTest.fail "accepted a sensor on a missing link"
  | .error _ => pure ()
  -- Overlay sensors replace base sensors of the same name.
  let twice ← IO.ofExcept (robot.merge (← IO.ofExcept (Robot.parseOverlay (← IO.FS.readFile "testdata/sensors_overlay.urdf"))))
  LeanTest.assertEqual (twice.sensors.map (·.name)) #["base_imu", "tip_camera"]

@[test]
def testStereoDefaultsAndSelfFilter : IO Unit := do
  let src := "<robot name=\"r\"><link name=\"a\"/><gazebo reference=\"a\"><sensor name=\"d\" type=\"depth\" self_filter=\"none\">" ++
    "<camera><stereo><baseline>0.06</baseline></stereo></camera></sensor></gazebo></robot>"
  let robot ← IO.ofExcept (Robot.parse src)
  let d := (robot.findSensor? "d").get!
  LeanTest.assertEqual d.selfFilter "none"
  let st := d.camera?.get!.stereo?.get!
  LeanTest.assertTrue (near st.baseline 0.06 && near st.disparityNoise 0.08 && st.realWidth == 848 && near st.minDepth 0.105)
  let bad := "<robot name=\"r\"><link name=\"a\"/><sensor name=\"s\" type=\"imu\" self_filter=\"head\"><parent link=\"a\"/></sensor></robot>"
  match Robot.parse bad with
  | .ok _ => LeanTest.fail "accepted self_filter=head"
  | .error _ => pure ()

@[test]
def testEventCamera : IO Unit := do
  let src := "<robot name=\"r\"><link name=\"a\"/><gazebo reference=\"a\"><sensor name=\"ev\" type=\"dvs\">" ++
    "<update_rate>1000</update_rate><camera><horizontal_fov>1.2</horizontal_fov><image><width>346</width><height>260</height></image>" ++
    "<dvs contrast_on=\"0.25\" contrast_off=\"0.3\" refractory=\"0.001\" adaptive=\"false\"><leak_rate>0.5</leak_rate><capacity>1000</capacity></dvs>" ++
    "</camera></sensor><sensor name=\"plain\" type=\"event_camera\"><camera/></sensor>" ++
    "<sensor name=\"cam\" type=\"camera\"><camera/></sensor></gazebo></robot>"
  let robot ← IO.ofExcept (Robot.parse src)
  let ev := (robot.findSensor? "ev").get!
  LeanTest.assertTrue (ev.kind.isEventCamera && ev.kind == .other "dvs" && near ev.updateRate 1000)
  LeanTest.assertTrue (ev.camera?.get!.width == 346 && ev.camera?.get!.height == 260)
  let e := ev.events?.get!
  LeanTest.assertTrue (near e.contrastOn 0.25 && near e.contrastOff 0.3 && near e.refractory 0.001 && !e.adaptive)
  LeanTest.assertTrue (near e.leakRate 0.5 && e.capacity == 1000 && near e.sigmaContrast 0.03 && near e.cutoffHz 300)
  -- An event camera without a `<dvs>` block gets the defaults; other sensors none.
  let plain := (robot.findSensor? "plain").get!
  LeanTest.assertTrue (plain.kind.isEventCamera && plain.events? == some {})
  LeanTest.assertTrue ((robot.findSensor? "cam").get!.events?.isNone)
  let bad := "<robot name=\"r\"><link name=\"a\"/><sensor name=\"s\" type=\"dvs\"><parent link=\"a\"/><dvs contrast=\"0\"/></sensor></robot>"
  match Robot.parse bad with
  | .ok _ => LeanTest.fail "accepted a zero contrast threshold"
  | .error _ => pure ()

@[test]
def testXmlText : IO Unit := do
  let e ← IO.ofExcept (Xml.parse "<a> <b> 1.5 </b><c><![CDATA[x<y]]> &amp; z</c><!-- no --></a>")
  LeanTest.assertEqual (e.textAt? ["b"]) (some "1.5")
  LeanTest.assertEqual (e.textAt? ["c"]) (some "x<y & z")
  LeanTest.assertEqual e.text ""

end LeanUrdfTypeProviderTest.Sensors
