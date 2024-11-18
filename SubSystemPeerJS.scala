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
  case object MakePeerConnection extends WebRtcEvent
/*--  
  case class RecievedData(ffgm: FlicFlacGameModel) extends WebRtcEvent
  case class SendGameData(ffgm: FlicFlacGameModel) extends WebRtcEvent
--*/
  case class MakePeerEntity(s0: String) extends WebRtcEvent with NetworkReceiveEvent
  case class Connect(s0: String) extends WebRtcEvent with NetworkReceiveEvent
  case class RecievedData(s1: String) extends WebRtcEvent with NetworkReceiveEvent
  case class SendGameData(s2: String) extends WebRtcEvent with NetworkReceiveEvent
end WebRtcEvent

final case class SSGame(initialMessage: String) extends SubSystem[SkeletonGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None
  var tryToConnect : Boolean = false
  var test1 = 0

  var latestUpdate: Option[SkeletonGameModel] = None

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = SkeletonGameModel
  val id: SubSystemId = SubSystemId("SubSystemPeerJS")

  val eventFilter: GlobalEvent => Option[GlobalEvent] = {
    case e: GlobalEvent => Some(e)
    case null           => None
  }
 
//  val eventFilter: GlobalEvent => Option[WebRtcEvent] = {
//    case e: WebRtcEvent => Some(e)
//    case _              => None
//    }

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

    case WebRtcEvent.MakePeerEntity(s0) =>
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent MakePeerEntity using $s0")
      peer = Some(Peer(id = s0))
      peer.foreach(_.on("open",(c: DataConnection) => scribe.debug("@@@ peer.on open")))
      peer.foreach(_.on("connection",(c: DataConnection) => scribe.debug("@@@ peer.on connection")))
      peer.foreach(_.on("disconnected", (c: DataConnection) => scribe.debug("@@@ peer.on disconnected")))
      peer.foreach(_.on("data", (c: DataConnection) => scribe.debug("@@@ peer.on data")))
      peer.foreach(_.on("close", (c: DataConnection) => scribe.debug("@@@ peer.on close")))
      peer.foreach(_.on("error", (c: DataConnection) => scribe.debug("@@@ peer.on error")))
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent MakePeerEntity EXIT")
      Outcome(())

    case WebRtcEvent.Connect(s0) =>
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent Connect using $s0")

      peer match
        case Some(p) =>
          conn = Some(p.connect(s0))
          conn.foreach(_.on("open",(c: DataConnection) => scribe.debug("@@@ conn.on open")))
          conn.foreach(_.on("connection",(c: DataConnection) => scribe.debug("@@@ conn.on connection")))
          conn.foreach(_.on("disconnected", (c: DataConnection) => scribe.debug("@@@ conn.on disconnected")))
          conn.foreach(_.on("data", (c: DataConnection) => 
            scribe.debug("@@@ conn.on data")
            val data = c.dataChannel.toString()
            scribe.debug(s"@@@ SubSystemPeerJS received data $data")
            )
          )
          conn.foreach(_.on("close", (c: DataConnection) => scribe.debug("@@@ conn.on close")))
          conn.foreach(_.on("error", (c: DataConnection) => scribe.debug("@@@ conn.on error")))

        case None =>
          scribe.debug("@@@ SubSystemPeerJS WebRtcEvent Connect NO PEER !!!")
          
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent Connect EXIT")
      Outcome(())


    case WebRtcEvent.SendGameData(ffgm) =>
//      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.SendGameData")
//      val toSend = ffgm.asJson.noSpaces
//      scribe.debug(s"@@@ sending $toSend")
//      conn.foreach(_.send(js.JSON.parse(toSend)))

      // FIXME experiment ...
      val ourNAME = context.reference.ourName.toUpperCase()
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent sending $ourNAME")
      conn.foreach(_.send(ourNAME))
      scribe.debug(s"@@@ SubSystemPeerJS WebRtcEvent sending EXIT")
      Outcome(())

    case WebRtcEvent.MakePeerConnection =>
      val ourName = context.reference.ourName
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.MakePeerConnection from " + ourName)
      peer = Some(Peer(id = ourName))

      peer.foreach(
        _.on(
          "connection",
          (c: DataConnection) =>
            val peerName = c.peer
            scribe.debug(s"@@@ SubSystemPeerJS We made a connection ${context.reference.ourName} -> ${context.reference.oppoName}")
            //conn = Some(c)
            c.on(
              "data",
              (data: js.Object) =>
                val str = js.JSON.stringify(data)
                scribe.debug(s"@@@ SubSystemPeerJS received data $data")
/*--                
                val ffgm = decode[FlicFlacGameModel](str)
                  .fold(
                    e =>
                      scribe.error(s"Error decoding data: $e")
                      throw new Exception("Error decoding data")
                    ,
                    identitye = Some(ffgm)
--*/                
            )
        )
      )
      peer.foreach { p =>
        scribe.info(s"@@@ SubSystemPeerJS Connecting ${context.reference.ourName} -> ${context.reference.oppoName}")
        val dataConn = p.connect(context.reference.oppoName)

        //conn = Some(dataConn)
        //scribe.info(s"@@@ SubSystemPeerJS conn2:" + conn)

      }
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.MakePeerConnection EXIT")
      Outcome(())

    case _ =>
      test1 = test1 + 1
      if (test1 % 300 == 0) then
        scribe.debug("@@@ SubSystemPeerJS 300 events")
      end if
      Outcome(())

/*        
    case _ =>
      latestUpdate.fold {
        Outcome(())
      } { ffgm =>
        Outcome(()).addGlobalEvents(WebRtcEvent.RecievedData(ffgm))
      }

    case _ =>
      scribe.debug("@@@ SubSystemPeerJS Default Handler")
      latestUpdate.fold {
        Outcome(())
      } { ffgm =>
        Outcome(()).addGlobalEvents(WebRtcEvent.RecievedData(ffgm))
      }
*/

  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame
