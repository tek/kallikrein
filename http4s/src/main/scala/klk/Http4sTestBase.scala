package klk

trait Http4sTestBase[RunF[_], FR]
extends ComposeTest[RunF, FR]
with Http4s[RunF, FR]
