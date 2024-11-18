package game

import indigo.*
import indigo.scenes.*
import org.w3c.dom.css.RGBColor

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
  ): GlobalEvent => Outcome[SceneModel] = {
      
      case k: KeyboardEvent.KeyDown =>
        if k.keyCode == Key.KEY_A then Outcome(model).addGlobalEvents(Key_A_Event)
        else if k.keyCode == Key.KEY_B then Outcome(model).addGlobalEvents(Key_B_Event)
        else if k.keyCode == Key.KEY_C then Outcome(model).addGlobalEvents(Key_C_Event)        
        else if k.keyCode == Key.KEY_D then Outcome(model).addGlobalEvents(Key_D_Event)        
        else if k.keyCode == Key.KEY_E then Outcome(model).addGlobalEvents(Key_E_Event)        
        else if k.keyCode == Key.KEY_F then Outcome(model).addGlobalEvents(Key_F_Event)        
        else if k.keyCode == Key.ENTER then Outcome(model).addGlobalEvents(Key_Enter_Event)        
        else if k.keyCode == Key.ESCAPE then Outcome(model).addGlobalEvents(Key_Escape_Event)        
        else 
          Outcome(model)

      case _ =>
        Outcome(model)
    }
//    _ => Outcome(model)

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

    val myColor : RGBA = 
      if (model.playerNo == 1) then RGBA.Cyan
      else RGBA.Green
      end if

    val oppoColor : RGBA = 
      if (model.playerNo == 1) then RGBA.Green
      else RGBA.Cyan
      end if
      

    val label0A = TextBox(model.ourName,300,80)
      .withColor(myColor)
      .withFontSize(Pixels(40))
      .moveTo(200,10)

    val label0B = TextBox("vs",300,80)
      .withColor(myColor)
      .withFontSize(Pixels(40))
      .moveTo(200,50)

    val label0C = TextBox(model.oppoName,300,80)
      .withColor(oppoColor)
      .withFontSize(Pixels(40))
      .moveTo(200,90)


    
    val label1 = TextBox("Status:",100,40)
      .withColor(myColor)
      .withFontSize(Pixels(20))
      .moveTo(10,180)
    val box1 = Rectangle(Point(5,210), Size(300,50))
    val text1 = TextBox(model.status,100,40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(10,220)

    val label2 = TextBox("TX:",100,40)
      .withColor(myColor)
      .withFontSize(Pixels(20))
      .moveTo(10,290)      
    val box2 = Rectangle(Point(5,320), Size(300,50))
    val text2 = TextBox(model.tx,100,40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(10,330)      

    val label3 = TextBox("RX:",100,40)
      .withColor(myColor)
      .withFontSize(Pixels(20))
      .moveTo(10,400)
    val box3 = Rectangle(Point(5,430), Size(300,50))
    val text3 = TextBox(model.rx,100,40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(10,440)


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
      |+| SceneUpdateFragment(label0A)
      |+| SceneUpdateFragment(label0B)
      |+| SceneUpdateFragment(label0C)
      |+| SceneUpdateFragment(label1)
      |+| SceneUpdateFragment(Shape.Box(box1, Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(text1)
      |+| SceneUpdateFragment(label2)
      |+| SceneUpdateFragment(Shape.Box(box2, Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(text2)
      |+| SceneUpdateFragment(label3)
      |+| SceneUpdateFragment(Shape.Box(box3, Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(text3)
    )

case object Key_A_Event extends GlobalEvent
case object Key_B_Event extends GlobalEvent
case object Key_C_Event extends GlobalEvent
case object Key_D_Event extends GlobalEvent
case object Key_E_Event extends GlobalEvent
case object Key_F_Event extends GlobalEvent
case object Key_Enter_Event extends GlobalEvent
case object Key_Escape_Event extends GlobalEvent