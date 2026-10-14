package cpgc.web

import com.raquo.laminar.api.L.*
import scala.scalajs.js

// Helpers shared by the panels: HTML attributes, toasts, unique ids and the disclosure widget.
/** Severity of a transient toast notification. */
enum ToastLevel:
  case Info, Warn, Error

/** One transient toast message. `id` is a monotonic sequence used for keyed
  * rendering and dismissal. */
final case class Toast(id: Long, message: String, level: ToastLevel)

object UiKit:

  /** Laminar 17 doesn't expose the HTML `title` attribute under a non-conflicting
    * name (the symbol `title` is the `<title>` element). Bind it explicitly. */
  val titleAttr: HtmlAttr[String] =
    htmlAttr("title", com.raquo.laminar.codecs.StringAsIsCodec)
  val ariaExpanded: HtmlAttr[String] = htmlAttr("aria-expanded", com.raquo.laminar.codecs.StringAsIsCodec)
  val ariaHasPopup: HtmlAttr[String] = htmlAttr("aria-haspopup", com.raquo.laminar.codecs.StringAsIsCodec)
  val ariaPressed: HtmlAttr[String]  = htmlAttr("aria-pressed", com.raquo.laminar.codecs.StringAsIsCodec)
  val ariaLabel: HtmlAttr[String]    = htmlAttr("aria-label", com.raquo.laminar.codecs.StringAsIsCodec)
  val ariaInvalid: HtmlAttr[String]  = htmlAttr("aria-invalid", com.raquo.laminar.codecs.StringAsIsCodec)
  val roleAttr: HtmlAttr[String]     = htmlAttr("role", com.raquo.laminar.codecs.StringAsIsCodec)

  // ── Toast notifications ────────────────────────────────────────────────────
  // A dependency-free transient-message stack for anything that would otherwise
  // die silently in the browser console.

  val toasts: Var[Vector[Toast]] = Var(Vector.empty)
  var toastIdSeq: Long = 0L

  /** Append a toast; auto-dismiss after 5 s. */
  def notify(message: String, level: ToastLevel = ToastLevel.Error): Unit =
    toastIdSeq += 1
    val id = toastIdSeq
    toasts.update(_ :+ Toast(id, message, level))
    js.timers.setTimeout(5000)(toasts.update(_.filterNot(_.id == id)))

  def dismissToast(id: Long): Unit =
    toasts.update(_.filterNot(_.id == id))

  var idSeq = 0
  def freshId(prefix: String): String =
    idSeq += 1
    s"$prefix-$idSeq"

  // ── Disclosure ─────────────────────────────────────────────────────────────

  /** A collapsible section: a `<button aria-expanded>` header over its content. */
  def disclosure(
      title: String, open: Var[Boolean], suffix: Modifier[HtmlElement], content: Modifier[HtmlElement]*
  ): HtmlElement =
    val contentId = freshId("disc")
    div(cls := "advanced-section",
      button(
        cls := "advanced-summary", typ := "button",
        ariaExpanded <-- open.signal.map(_.toString),
        aria.controls := contentId,
        onClick.preventDefault --> { _ => open.update(!_) },
        span(cls := "advanced-arrow", child.text <-- open.signal.map(if _ then "▾" else "▸")),
        s" $title",
        suffix
      ),
      div(idAttr := contentId, cls := "advanced-content",
        cls("hidden") <-- open.signal.map(!_),
        content
      )
    )
