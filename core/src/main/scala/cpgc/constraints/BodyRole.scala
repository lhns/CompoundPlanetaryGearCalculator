package cpgc.constraints

/** Role a `Body` plays in a topology.
  *
  *   - Input:  driving body, angular velocity is user-supplied.
  *   - Output: observed body; the ratio is ω(Input) / ω(Output).
  *   - Fixed:  grounded body, angular velocity = 0. Required to remove the
  *             remaining DOF after the Input pin.
  *   - Free:   velocity is determined by the constraint system, not pinned.
  *   - Absent: the body physically does not exist (a sun-less stage). Only
  *             `Body.S1` and `Body.S2` may be Absent, never a pinned body.
  *
  * Validity: a `Topology` must have exactly one Input, one Output, and one
  * Fixed body. The remaining bodies are Free or Absent. `Absent` is a
  * structural flag, not a role pin; it removes the body from the unknown set.
  */
enum BodyRole:
  case Input, Output, Fixed, Free, Absent
