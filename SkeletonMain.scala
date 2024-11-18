package game

import indigo.*
import indigo.scenes.*
import scala.scalajs.js.annotation.JSExportTopLevel

import scribe.*
import scribe.format.*

import org.scalajs.dom
import tyrian.TyrianSubSystem
import cats.effect.IO
import tyrian.TyrianIndigoBridge

case class SkeletonGame(
    tyrianSubSystem: TyrianSubSystem[IO, Int, SkeletonGameModel]
) extends IndigoGame[BootData, StartUpData, SkeletonGameModel, ViewModel]:

  Logger.root
    .clearHandlers()
    .withHandler(formatter = Formatter.simple)
    .withMinimumLevel(Level.Debug)
    .replace()

  def initialScene(bootData: BootData): Option[SceneName] =
    Some(SkeletonGameScene.name)

  def scenes(bootData: BootData): NonEmptyList[Scene[StartUpData, SkeletonGameModel, ViewModel]] =
    NonEmptyList(SkeletonGameScene)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[BootData, SkeletonGameModel]] =
    val width = flags("width").toInt
    val height = flags("height").toInt
    val name1:String  = flags("name1")
    val name2:String  = flags("name2")

    Outcome {

      val skeletonBootData: BootData = 
        BootData.create(width, height, name1, name2)

      val config:GameConfig = 
        GameConfig.default.withViewport(skeletonBootData.gameViewPort)

      BootResult(config, skeletonBootData)
        .withAssets(Set.empty)
        .withSubSystems(tyrianSubSystem)
    }

  def initialModel(startupData: StartUpData): Outcome[SkeletonGameModel] =
    scribe.debug("@@@ SubSystem initialModel()")

    val n1 = startupData.skBootData.name1
    val n2 = startupData.skBootData.name1
    val skm = SkeletonGameModel.creation(n1, n2)
    Outcome(skm)

  def initialViewModel(startupData: StartUpData, model: SkeletonGameModel): Outcome[ViewModel] =
    scribe.debug("@@@ SubSystem initialViewModel()")
    Outcome(ViewModel())

  def setup(
      bootData: BootData,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[StartUpData]] =
    val outCome = StartUpData.initialise(bootData)
    outCome

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

final case class BootData(pixelWidth: Int, pixelHeight: Int, firstName: String, secondName: String, viewPort: GameViewport) : 
  val width = pixelWidth
  val height = pixelHeight
  val name1 = firstName
  val name2 = secondName
  val gameViewPort = viewPort
end BootData

object BootData:
  def create(w:Int, h:Int, n1:String, n2:String ): BootData = 
    BootData(w, h, n1, n2, GameViewport(w,h))
end BootData


final case class StartUpData(skBootData: BootData, staticAssets: StaticAssets):
  val bootData = skBootData
  val assets = staticAssets

object StartUpData:

  def initialise(bootData: BootData): Outcome[Startup[StartUpData]] = 
    Outcome(Startup.Success(createStartUpData(bootData)))


  def createStartUpData(skBootData: BootData): StartUpData =
    StartUpData(
      skBootData = skBootData,
      staticAssets = game.StaticAssets()
    )
end StartUpData

final case class StaticAssets()
//final case class Model()
final case class ViewModel()
