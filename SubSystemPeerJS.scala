package game

import scala.scalajs.js
import indigo.*
import scribe.*
import java.util.Timer
import indigo.platform.networking.Network
import io.github.quafadas.peerscalajs.Peer
import io.github.quafadas.peerscalajs.DataConnection
import io.circe.syntax.*
import io.circe.parser.decode
import io.circe.syntax._
import indigo.shared.events.NetworkReceiveEvent
import indigo.shared.events.NetworkSendEvent

case class SkeletonNewInformationEvent(ffgm: SkeletonGameModel) extends SubSystemEvent

sealed trait WebRtcEvent extends GlobalEvent 
object WebRtcEvent:
  case object MakePeerEntity extends WebRtcEvent with NetworkReceiveEvent
  case class Connect(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case class ReceivedData(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case class SendGameData(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case class Close() extends WebRtcEvent with NetworkReceiveEvent
end WebRtcEvent

final case class SSGame(initialMessage: String) extends SubSystem[SkeletonGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None
  var tryToConnect : Boolean = false

  var latestDisplayInfo : Option[SkeletonUpdate.Info] = None
  

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = SkeletonGameModel
  val id: SubSystemId = SubSystemId("SubSystemPeerJS")

  //val eventFilter: GlobalEvent => Option[GlobalEvent] = {
  //  case e: GlobalEvent => Some(e)
  //  case null           => None
  //}
 
  val eventFilter: GlobalEvent => Option[WebRtcEvent] = {
    case e: WebRtcEvent => Some(e)
    case _              => None
  }

  // Extra line here, as mandated by indigo's SubSystem.scala. Yet it is not in the examples!!!
  def reference(skeletonGameModel: SkeletonGameModel): SkeletonGameModel = 
    skeletonGameModel

  def initialModel: Outcome[Unit] =
    scribe.debug("@@@ SubSystemPeerJS initialModel")
    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] = {

    case WebRtcEvent.MakePeerEntity =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent MakePeerEntity using " + context.reference.ourName)
      peer = Some(Peer(id = context.reference.ourName))

      peer.foreach(_.on("open",(c: DataConnection) =>
        latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Open","",""))
        scribe.debug("@@@ peer.on open")))
      peer.foreach(_.on("connection",(c: DataConnection) => 
        latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Connection","",""))
        scribe.debug("@@@ peer.on connection")))
      peer.foreach(_.on("disconnected", (c: DataConnection) => 
        latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Disconnected","",""))
        scribe.debug("@@@ peer.on disconnected")))
      peer.foreach(_.on("data", (c: DataConnection) => 
        latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Data","",""))
        scribe.debug("@@@ peer.on data")))
      peer.foreach(_.on("close", (c: DataConnection) => 
        latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Close","",""))
        scribe.debug("@@@ peer.on close")))
      peer.foreach(_.on("error", (c: DataConnection) => 
        latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Error","",""))
        scribe.debug("@@@ peer.on error")))
      Outcome(())

    case WebRtcEvent.Connect(s) =>
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent Connect using $s")

      peer match
        case Some(p) =>
          conn = Some(p.connect(s))

          conn.foreach(_.on("open",(c: DataConnection) => 
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Open","",""))
            scribe.debug("@@@ conn.on open")))
          conn.foreach(_.on("connection",(c: DataConnection) => 
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Connection","",""))
            scribe.debug("@@@ conn.on connection")))
          conn.foreach(_.on("disconnected", (c: DataConnection) => 
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Disconnected","",""))
            scribe.debug("@@@ conn.on disconnected")))
          conn.foreach(_.on("data", (c: DataConnection) => 

            // *************************************************************************************************
            // Simon here is my problem ....
            // If I enable (A) and disable (B) then console shows "@@@ conn.on data" on inbound string
            // If I disable (A) and enable (B) then console shows nothing on inbound string
            // Something is wrong with section (B)
            // Section (C) just as (B) but assumes String instead of js.Object
            // Please note I am testing against https://jmcker.github.io/Peer-to-Peer-Cue-System/receive.html
            // *************************************************************************************************

            // Section (A)
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Data","","WHACKY - PLEASE FIXME"))
            scribe.debug("@@@ conn.on data")

            // Section (B)
            //(data: js.Object) =>
            //  val str = js.JSON.stringify(data)
            //  latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Data","",str))
            //  scribe.debug("@@@ conn.on data")

            // Section (C)
            //(data: String) =>
            //  latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Data","",data))
            //  scribe.debug("@@@ conn.on data")
            

            )
          )
          conn.foreach(_.on("close", (c: DataConnection) => 
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Close","",""))
            scribe.debug("@@@ conn.on close")))
          conn.foreach(_.on("error", (c: DataConnection) => 
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Error","",""))
            scribe.debug("@@@ conn.on error")))

        case None =>
          latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:NO PEER !!!","",""))
          scribe.debug("@@@ SubSystemPeerJS WebRtcEvent Connect NO PEER !!!")          
      Outcome(())


    case WebRtcEvent.SendGameData(s) =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent SendGameData " + s)
      conn.foreach(_.send(s))
      latestDisplayInfo = Some(SkeletonUpdate.Info("",s,""))
      Outcome(())

    case WebRtcEvent.Close() =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent Close")
      conn.foreach(_.close())
      Outcome(())
  

    case _ =>
      latestDisplayInfo match
        case Some(info) =>
          Outcome(()).addGlobalEvents(info)
        case None =>
          Outcome(())
  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame
