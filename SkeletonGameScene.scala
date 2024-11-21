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

  // For the moment we don't need a timer mechansim
  // val timerDivisor = 100  // makes this timer operate in 10ths of a second ... 0 means disabled, +20 = 2 seconds
  // var timerT1: Long = (System.currentTimeMillis() / timerDivisor) + 20 

  val playerGuideA1 = "You are Player1"
  val playerGuideA2 = "You are Player2"
  val playerGuideB = "P to establish PeerJS"
  val playerGuideC = "ENTER to open connection"
  val playerGuideD1 = "A,B,C,D,E or F to send ALPHA,BRAVO,CHARLIE,DELTA,ECHO or FOXTROT"
  val playerGuideD2 = "A,B,C,D,E or F to send alpha,bravo,charlie,delta,echo or foxtrot"
  val playerGuideE = "ESCAPE to close connection"


  def updateModel(
      context: SceneContext[StartUpData],
      model: SceneModel
  ): GlobalEvent => Outcome[SceneModel] = {

      case e: SkeletonUpdate.Info =>
        val newStatus = if (e.st.isEmpty()) then model.status else e.st
        val newTx = if (e.tx.isEmpty()) then model.tx else e.tx
        val newRx = if (e.rx.isEmpty()) then model.rx else e.rx
        Outcome(model.copy(status=newStatus, tx=newTx, rx=newRx))
      
      case k: KeyboardEvent.KeyDown =>
        if k.keyCode == Key.KEY_P then Outcome(model).addGlobalEvents(WebRtcEvent.MakePeerEntity)
        else if k.keyCode == Key.ENTER then Outcome(model).addGlobalEvents(WebRtcEvent.Connect(model.oppoName))
        else if k.keyCode == Key.ESCAPE then Outcome(model).addGlobalEvents(WebRtcEvent.Close())
        else
          val playerNo = model.playerNo
          val msgToSend1 = (k.keyCode) match
            case Key.KEY_A => "ALPHA"
            case Key.KEY_B => "BRAVO"
            case Key.KEY_C => "CHARLIE"
            case Key.KEY_D => "DELTA"
            case Key.KEY_E => "ECHO"
            case Key.KEY_F => "FOXTROT"
            case _ => ""
          if (msgToSend1.isEmpty() == false) then
            if (playerNo == 1) then
              Outcome(model).addGlobalEvents(WebRtcEvent.SendGameData(msgToSend1))
            else
              val msgToSend2 = msgToSend1.toLowerCase()
              Outcome(model).addGlobalEvents(WebRtcEvent.SendGameData(msgToSend2))
          else
            Outcome(model)

      case _ =>
        // For the moment we don't need a timer mechansim
        //if ((System.currentTimeMillis()/timerDivisor) > timerT1) then
        //  timerT1 = (System.currentTimeMillis() / timerDivisor) + 20 
        //  scribe.info("@@@ Timer T1")
        //end if
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

    val myColor : RGBA = if (model.playerNo == 1) then RGBA.Cyan else RGBA.Green
    val oppoColor : RGBA = if (model.playerNo == 1) then RGBA.Green else RGBA.Cyan
    val playerGuideA = if (model.playerNo == 1) then playerGuideA1 else playerGuideA2
    val playerGuideD = if (model.playerNo == 1) then playerGuideD1 else playerGuideD2
          

    val label0A = TextBox(model.ourName,300,80).withColor(myColor).withFontSize(Pixels(40)).moveTo(10,10)
    val label0B = TextBox("vs",300,80).withColor(myColor).withFontSize(Pixels(40)).moveTo(50,50)
    val label0C = TextBox(model.oppoName,300,80).withColor(oppoColor).withFontSize(Pixels(40)).moveTo(10,90)
    val textP1 = TextBox(playerGuideA,400,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(320,10) 
    val textP2 = TextBox(playerGuideB,400,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(320,40) 
    val textP3 = TextBox(playerGuideC,400,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(320,70) 
    val textP4 = TextBox(playerGuideD,800,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(320,100) 
    val textP5 = TextBox(playerGuideE,400,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(320,130) 
  
    val label1 = TextBox("Status:",100,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(10,180)
    val box1 = Rectangle(Point(5,210), Size(300,50))
    val text1 = TextBox(model.status,300,40).withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(10,220)

    val label2 = TextBox("TX:",100,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(10,290)      
    val box2 = Rectangle(Point(5,320), Size(300,50))
    val text2 = TextBox(model.tx,300,40).withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(10,330)      

    val label3 = TextBox("RX:",100,40).withColor(myColor).withFontSize(Pixels(20)).moveTo(10,400)
    val box3 = Rectangle(Point(5,430), Size(300,50))
    val text3 = TextBox(model.rx,300,40).withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(10,440)

    val spinner = Shape.Box(Rectangle(0, 0, 60, 60), Fill.LinearGradient(Point(0), RGBA.Magenta, Point(45), RGBA.Cyan))
          .withRef(30, 30).moveTo(650, 50).rotateTo(Radians.fromSeconds(context.running * 0.25))


    Outcome(
      SceneUpdateFragment.empty
      |+| SceneUpdateFragment(label0A)
      |+| SceneUpdateFragment(label0B)
      |+| SceneUpdateFragment(label0C)
      |+| SceneUpdateFragment(textP1)
      |+| SceneUpdateFragment(textP2)
      |+| SceneUpdateFragment(textP3)
      |+| SceneUpdateFragment(textP4)
      |+| SceneUpdateFragment(textP5)
      |+| SceneUpdateFragment(spinner)
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

object SkeletonUpdate:
  case class Info(st: String, tx: String, rx: String) extends GlobalEvent
end SkeletonUpdate