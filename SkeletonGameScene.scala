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
    )
