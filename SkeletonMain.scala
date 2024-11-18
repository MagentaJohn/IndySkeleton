package game

import indigo.*
import indigo.scenes.*
import scala.scalajs.js.annotation.JSExportTopLevel

import org.scalajs.dom
import tyrian.TyrianSubSystem
import cats.effect.IO
import tyrian.TyrianIndigoBridge

case class SkeletonGame(
    tyrianSubSystem: TyrianSubSystem[IO, Int, SkeletonGameModel]
) extends IndigoGame[BootData, StartUpData, SkeletonGameModel, ViewModel]:

  def initialScene(bootData: BootData): Option[SceneName] =
    None

  def scenes(bootData: BootData): NonEmptyList[Scene[StartUpData, SkeletonGameModel, ViewModel]] =
    NonEmptyList(GameScene)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[BootData, SkeletonGameModel]] =
    Outcome {
      BootResult(GameConfig.default, BootData())
        .withAssets(Set.empty)
    }

  def initialModel(startupData: StartUpData): Outcome[SkeletonGameModel] =
    Outcome(SkeletonGameModel("",""))

  def initialViewModel(startupData: StartUpData, model: SkeletonGameModel): Outcome[ViewModel] =
    Outcome(ViewModel())

  def setup(
      bootData: BootData,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[StartUpData]] =
    Outcome(Startup.Success(StartUpData()))

  def updateModel(
      context: FrameContext[StartUpData],
      model: SkeletonGameModel
  ): GlobalEvent => Outcome[SkeletonGameModel] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[StartUpData],
      model: SkeletonGameModel,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[StartUpData],
      model: SkeletonGameModel,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)

final case class BootData()
final case class StartUpData()
final case class Model()
final case class ViewModel()
