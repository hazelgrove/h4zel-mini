open Junit_alcotest;

let (suite, _) =
  run_and_report(~and_exit=false, "HazelTests", [Test_Grove.tests]);
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
