package game

import tyrian.Html.*
import tyrian.*
import cats.effect.IO
import org.scalajs.dom
import scala.concurrent.duration.DurationDouble
import scala.scalajs.js.annotation.*
import scribe.*

enum Msg:
  case StartIndigo extends Msg
  case DoNothing extends Msg
  case RunGame extends Msg
  case RetryIndigo extends Msg
  case NavigateToUrl(url: String) extends Msg
end Msg

final case class TyrianModel(
    bridge: TyrianIndigoBridge[IO, Int, SkeletonGameModel],
    renderUi: Boolean = true
)
object TyrianModel:
  val init: TyrianModel = TyrianModel(TyrianIndigoBridge())
end TyrianModel

object TyrianApp extends TyrianIOApp[Msg, TyrianModel]:

  override def router: Location => Msg = {
    case loc: Location.Internal =>
      scribe.info(s"loc.fullPath: ${loc.fullPath}")
      loc.fullPath.contains("runGame=true") match
        case true =>
          Msg.StartIndigo
        case false =>
          Msg.DoNothing
      end match
    case ext =>
      Msg.NavigateToUrl(ext.href)
  }

  def init(flags: Map[String, String]): (TyrianModel, Cmd[IO, Msg]) =
    (TyrianModel.init, Cmd.Emit(Msg.DoNothing))

  def update(model: TyrianModel): Msg => (TyrianModel, Cmd[IO, Msg]) = {
    // format: off
    case Msg.StartIndigo =>
      println("Starting Indigo")
      println(model)
      val task: IO[Msg] = IO.delay{
        if dom.document.getElementById("indigo-container") == null then
          Msg.RetryIndigo
        else {
          val mySubStrings = dom.window.location.href.split('?')
          val sUserEq1 = mySubStrings(1)
          val sName1 = sUserEq1.slice(5, sUserEq1.length())
          val sUserEq2 = mySubStrings(2)
          val sName2 = sUserEq2.slice(5, sUserEq2.length())

          SkeletonGame(model.bridge.subSystem(IndigoGameId("indigo-container"))).launch(
            "indigo-container",
            Map[String, String](
              "width" -> dom.window.innerWidth.toString,
              "height" -> dom.window.innerHeight.toString,
              "name1" -> sName1,
              "name2" -> sName2
            )
          )
          Msg.DoNothing
        }
      }
      (model.copy(renderUi = false), Cmd.Run(task))

    case Msg.RetryIndigo =>
      scribe.info("Retrying Indigo")
      (model, Cmd.emitAfterDelay(Msg.StartIndigo, 0.5.seconds))

    case Msg.DoNothing =>
      (model, Cmd.None)

    case Msg.NavigateToUrl(url) => {
        dom.console.error("external navigation not supported")
        (model, Cmd.None)
    }
    case Msg.RunGame =>
      (
        model.copy(renderUi = false),
        Nav.loadUrl[IO](s"${dom.window.location}?runGame=true"),
      )
    // format: on
  }

  end update

  def view(model: TyrianModel): Html[Msg] =
    if model.renderUi then
      div(id := "myapp")(
        button(id := "startGame", onClick(Msg.RunGame))("Start Game")
      )
    else div(id := "indigo-container")()
  def subscriptions(model: TyrianModel): Sub[IO, Msg] =
    Sub.None
end TyrianApp
