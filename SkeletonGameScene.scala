package game

import indigo.*
import indigo.scenes.*

object SkeletonGameScene extends Scene[StartUpData, SkeletonGameModel, ViewModel]:

  type SceneModel     = SkeletonGameModel
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("MainScene")

  val modelLens: Lens[SkeletonGameModel, SkeletonGameModel] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem[SkeletonGameModel]] =
    Set(SSGame("SubSystemPeerJS"))

  def updateModel(
      context: SceneContext[StartUpData],
      model: SceneModel
  ): GlobalEvent => Outcome[SceneModel] =
    _ => Outcome(model)

  def updateViewModel(
      context: SceneContext[StartUpData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: SceneContext[StartUpData],
      model: SceneModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val box1 = Rectangle(Point(5,170), Size(300,50))

    val text1 = TextBox("Status:",100,40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(10,180)

    val box2 = Rectangle(Point(5,240), Size(300,50))

    val text2 = TextBox("TX:",100,40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(10,250)      

    val box3 = Rectangle(Point(5,310), Size(300,50))

    val text3 = TextBox("RX:",100,40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(10,320)


    Outcome(
      SceneUpdateFragment(
        Shape
          .Box(
            Rectangle(0, 0, 60, 60),
            Fill.LinearGradient(Point(0), RGBA.Magenta, Point(45), RGBA.Cyan)
          )
          .withRef(30, 30)
          .moveTo(100, 100)
          .rotateTo(Radians.fromSeconds(context.running * 0.25))
      )
      |+| SceneUpdateFragment(Shape.Box(box1, Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(text1)
      |+| SceneUpdateFragment(Shape.Box(box2, Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(text2)
      |+| SceneUpdateFragment(Shape.Box(box3, Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(text3)
    )
