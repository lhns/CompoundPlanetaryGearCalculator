package cpgc.web

import com.raquo.laminar.api.L.*
import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.web.UiKit.*
import org.scalajs.dom

// Gearbox-type preset picker plus the advanced topology controls (suns, couplings, roles, phasing).
object TopologyPanel:

  // ── Gearbox-type presets ──────────────────────────────────────────────────

  /** Short user-facing label for the picker. */
  private def presetLabel(p: GearboxPreset): String = p match
    case GearboxPreset.SingleStage       => "Single-stage planetary"
    case GearboxPreset.SunDriven         => "Sun-driven compound (Standard)"
    case GearboxPreset.CarrierDriven     => "Carrier-driven compound (CDCP)"
    case GearboxPreset.SunlessCdcp       => "CDCP (sun-less)"
    case GearboxPreset.NoSecondSun       => "No-second-sun compound (NSS)"
    case GearboxPreset.RingToSunCascade  => "Ring-to-sun cascade"
    case GearboxPreset.Ravigneaux        => "Ravigneaux"
    case GearboxPreset.Simpson           => "Simpson"
    case GearboxPreset.Custom            => "Custom (edited below)"

  /** One-line plain-English description shown under the picker. */
  private def presetDescription(p: GearboxPreset): String = p match
    case GearboxPreset.SingleStage       => "One planetary stage. Sun in, carrier out, ring grounded."
    case GearboxPreset.SunDriven         => "Classic stacked-planet compound. Sun in, ring-2 out."
    case GearboxPreset.CarrierDriven     => "Same compound, carrier as input. Smoother low-ratio reductions."
    case GearboxPreset.SunlessCdcp       => "Carrier in, no sun gears at all: the planets mesh only with the two rings (the reference program's CDCP)."
    case GearboxPreset.NoSecondSun       => "Stage-2 has no sun (one fewer mesh → higher efficiency)."
    case GearboxPreset.RingToSunCascade  => "Stage-1 ring drives stage-2 sun. Series-like compound."
    case GearboxPreset.Ravigneaux        => "Stage-1 carrier drives stage-2 sun; rings welded. Automotive AT."
    case GearboxPreset.Simpson           => "Stage-1 carrier drives stage-2 ring; suns welded. Automotive AT."
    case GearboxPreset.Custom            => "Custom topology: adjust the suns, couplings and roles below."

  // ── Topology controls ──────────────────────────────────────────────────────

  /** A `<select>` whose DOM value is forced back to `current()` after each
    * change, so a rejected edit never leaves the wrong option displayed. */
  private def pickSelect(current: () => String)(handle: String => Unit)(mods: Modifier[HtmlElement]*): HtmlElement =
    select(
      mods,
      inContext { (el: HtmlElement) =>
        onChange --> Observer[dom.Event] { _ =>
          val sel = el.ref.asInstanceOf[dom.html.Select]
          handle(sel.value)
          sel.value = current()
        }
      }
    )

  def apply(v: Var[Topology], phased: Var[Boolean]): HtmlElement =
    val stagesSig    = v.signal.map(_.stages)
    val compoundSig  = v.signal.map(t => !TopologyEdit.couplingSelectorsDisabled(t))
    val inputBody    = v.signal.map(_.input)
    val outputBody   = v.signal.map(_.output)
    val fixedBody    = v.signal.map(_.fixed)
    val presetSig    = v.signal.map(TopologyEdit.topologyPreset)
    val stackedSig   = v.signal.map(_.stacked)
    val advancedOpen: Var[Boolean] = Var(false)
    // A derived-Custom topology opens the section so the edits are visible.
    presetSig.foreach { p => if p == GearboxPreset.Custom then advancedOpen.set(true) }(unsafeWindowOwner)

    def sunToggle(stage: Int, text: String): HtmlElement =
      val present = v.signal.map(t => t.roles.get(if stage == 1 then Body.S1 else Body.S2).exists(_ != BodyRole.Absent))
      val reason  = v.signal.map(TopologyEdit.sunToggleDisabledReason(_, stage))
      div(
        label(cls := "topology-toggle",
          cls("disabled") <-- reason.map(_.isDefined),
          titleAttr <-- reason.map(_.getOrElse("")),
          input(typ := "checkbox",
            disabled <-- reason.map(_.isDefined),
            controlled(
              checked <-- present,
              onInput.mapToChecked --> Observer[Boolean] { b =>
                TopologyEdit.setSunPresent(v.now(), stage, b) match
                  case Right(t)  => v.set(t)
                  case Left(msg) => UiKit.notify(s"Cannot change the sun: $msg", ToastLevel.Warn)
              }
            )
          ),
          span(cls := "switch"),
          span(text)
        ),
        div(cls := "field-hint", child.text <-- reason.map(_.fold("")(r => s"Locked: $r")))
      )

    def couplingPicker(slot: CouplingSlot, name: String, tip: String, pick: Topology => Coupling): Modifier[HtmlElement] =
      val sid = freshId("coupling")
      Seq(
        label(cls := "stage-coupling-row", forId := sid, cls("disabled") <-- compoundSig.map(!_), name),
        pickSelect(() => pick(v.now()).toString)(n => v.update(t => TopologyEdit.setCoupling(t, slot, Coupling.valueOf(n))))(
          idAttr := sid, cls := "role-select",
          disabled <-- compoundSig.map(!_),
          titleAttr := tip,
          Coupling.values.toSeq.map { c =>
            option(
              value := c.toString,
              selected <-- v.signal.map(pick(_) == c),
              disabled <-- v.signal.map(TopologyEdit.couplingOptionDisabled(_, slot, c)),
              titleAttr <-- v.signal.map(TopologyEdit.couplingOptionReason(_, slot, c).getOrElse("")),
              Coupling.label(c)
            )
          }
        )
      )

    def rolePicker(name: String, currentSig: Signal[Body], current: Topology => Body, role: BodyRole, tip: String): Modifier[HtmlElement] =
      val rid = freshId("role")
      Seq(
        label(forId := rid, name),
        pickSelect(() => current(v.now()).toString)(n => v.update(TopologyEdit.assignRole(_, Body.valueOf(n), role)))(
          idAttr := rid, cls := "role-select", titleAttr := tip,
          // Bodies that do not exist in this topology (absent suns, stage 2 of a single stage) are greyed out.
          Body.values.toSeq.map { b =>
            option(
              value := b.toString,
              selected <-- currentSig.map(_ == b),
              disabled <-- v.signal.map(t => !t.roles.contains(b) || t.roles.get(b).contains(BodyRole.Absent)),
              Body.label(b)
            )
          }
        )
      )

    div(cls := "topology",
      // ── Gearbox-type preset picker — the primary topology control ─────────
      label(forId := "preset-select", "Gearbox type"),
      pickSelect(() => TopologyEdit.topologyPreset(v.now()).toString) { name =>
        val p = GearboxPreset.valueOf(name)
        if p != GearboxPreset.Custom then v.set(TopologyEdit.presetTopology(p))
      }(
        idAttr := "preset-select", cls := "preset-select",
        titleAttr := "Pick a named gearbox family. Most users don't need to touch the Advanced section.",
        TopologyEdit.selectablePresets.map { p =>
          option(value := p.toString, selected <-- presetSig.map(_ == p), presetLabel(p))
        },
        // Shown only as the derived state of an edited topology; not a choice.
        option(value := GearboxPreset.Custom.toString, disabled := true, hidden <-- presetSig.map(_ != GearboxPreset.Custom),
               selected <-- presetSig.map(_ == GearboxPreset.Custom), presetLabel(GearboxPreset.Custom))
      ),
      div(cls := "topology-derived", child.text <-- presetSig.map(presetDescription)),
      // ── Advanced section — suns, couplings, roles ─────────────────────────
      disclosure("Advanced topology", advancedOpen,
        span(cls := "advanced-customized",
          child.text <-- presetSig.map(p => if p == GearboxPreset.Custom then "  (customized)" else "")),
        label("Stages"),
        div(cls := "topology-segment", roleAttr := "group", ariaLabel := "Number of stages",
          button(
            typ := "button",
            cls("active") <-- stagesSig.map(_ == 1),
            ariaPressed <-- stagesSig.map(s => (s == 1).toString),
            titleAttr := "One planetary stage: sun + planets + ring + carrier.",
            "Single stage",
            onClick.preventDefault --> { _ => v.update(TopologyEdit.setStageCount(_, 1)) }
          ),
          button(
            typ := "button",
            cls("active") <-- stagesSig.map(_ == 2),
            ariaPressed <-- stagesSig.map(s => (s == 2).toString),
            titleAttr := "Two planetary stages; compound reduction.",
            "Compound (2 stages)",
            onClick.preventDefault --> { _ => v.update(TopologyEdit.setStageCount(_, 2)) }
          )
        ),
        sunToggle(1, "Stage 1 has its own sun gear"),
        sunToggle(2, "Stage 2 has its own sun gear"),
        div(cls := "field-hint",
          "Without a sun the planets mesh only with the ring, so the sun-teeth range and its spacing rule do not apply to that stage."),
        couplingPicker(CouplingSlot.First, "Coupling 1",
          "First inter-stage weld. The classic compound uses Carrier 1 ↔ Carrier 2.", _.firstCoupling),
        couplingPicker(CouplingSlot.Second, "Coupling 2",
          "Second inter-stage weld. The classic compound uses stacked planets (P1 ↔ P2).", _.secondCoupling),
        div(cls := "coupling-note",
          child.text <-- v.signal.map { t =>
            if t.stages != 2 then ""
            else
              Seq(Coupling.manufacturingNote(t.firstCoupling), Coupling.manufacturingNote(t.secondCoupling))
                .filter(_.nonEmpty).mkString("  ·  ")
          }
        ),
        // Phased pinions: only meaningful for stacked planets.
        div(
          cls("hidden") <-- stackedSig.map(!_),
          label(cls := "topology-toggle",
            input(typ := "checkbox",
              controlled(checked <-- phased.signal, onInput.mapToChecked --> phased)),
            span(cls := "switch"),
            span("Individually phased pinions")
          ),
          div(cls := "field-hint",
            "Off: every planet is the same one-piece part. On: each planet gets its own twist between the two halves, " +
            "so more tooth combinations assemble, but the N planets are different parts.")
        ),
        rolePicker("Input", inputBody, _.input, BodyRole.Input, "Body driven externally (e.g. by a motor)."),
        rolePicker("Output", outputBody, _.output, BodyRole.Output, "Body whose speed defines the gearbox ratio."),
        rolePicker("Fixed", fixedBody, _.fixed, BodyRole.Fixed, "Body grounded to the housing (ω = 0)."),
        div(cls := "topology-derived legacy-derived",
          child.text <-- v.signal.map(t => s"→ ${topologyLabel(t)}"))
      )
    )

  private def topologyLabel(t: Topology): String =
    val legacy = t.legacyMode.map(_.toString).getOrElse(if t == Topology.sunlessCdcpCompound then "CDCP sun-less" else "custom")
    val suns =
      Seq(Option.when(!t.s1Present)("no sun 1"), Option.when(t.stages == 2 && !t.s2Present)("no sun 2")).flatten
    val sunText = if suns.isEmpty then "" else suns.mkString(", ", ", ", "")
    val pins = s"${Body.label(t.input).toLowerCase} in, ${Body.label(t.output).toLowerCase} out, ${Body.label(t.fixed).toLowerCase} fixed"
    val kind = if t.stages == 1 then "single-stage" else "compound"
    s"$kind: $pins$sunText ($legacy)"
