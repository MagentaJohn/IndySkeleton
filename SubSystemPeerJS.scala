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
import scala.collection.mutable.Queue
import cats.instances.queue

sealed trait WebRtcEvent extends GlobalEvent
object WebRtcEvent:
  case object MakePeerEntity extends WebRtcEvent // 10
  case class CreatedPeerEntity(peer: Peer) extends WebRtcEvent // 20
  case class Connect(s: String) extends WebRtcEvent with NetworkReceiveEvent // 30
  case class IncomingPeerConnection(conn: DataConnection) extends WebRtcEvent with NetworkReceiveEvent // 40
  case class PeerCreatedConnection(conn: DataConnection) extends WebRtcEvent // 50
  case object ConnectionOpen extends WebRtcEvent with NetworkReceiveEvent // 60
  case class SendData(s: String) extends WebRtcEvent with NetworkReceiveEvent // 70
  case class ReceivedData(s: String) extends WebRtcEvent with NetworkReceiveEvent // 80
  case class Close() extends WebRtcEvent with NetworkReceiveEvent // 90

end WebRtcEvent

final case class SSGame(initialMessage: String)
    extends SubSystem[SkeletonGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None

  var latestDisplayInfo: Option[SkeletonUpdate.Info] = None

  val eventQueue: Queue[WebRtcEvent] = Queue.empty[WebRtcEvent]

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = SkeletonGameModel
  val id: SubSystemId = SubSystemId("SubSystemPeerJS")

  val eventFilter: GlobalEvent => Option[GlobalEvent] = {
    case e: GlobalEvent => Some(e)
    case null           => None
  }

  def reference(skeletonGameModel: SkeletonGameModel): SkeletonGameModel =
    skeletonGameModel

  def initialModel: Outcome[Unit] =
    scribe.debug("@@@-00 SubSystemPeerJS initialModel")
    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] = {

    case WebRtcEvent.MakePeerEntity =>
      scribe.debug("@@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using " + context.reference.ourName)
      val localPeer = Peer(id = context.reference.ourName)

      localPeer.on(
        "open",
        (_: Any) => 
          scribe.debug("@@@-11 localPeer.on open")
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Open", "", ""))
      )

      localPeer.on(
        "connection",
        (c: DataConnection) =>
          scribe.debug("@@@-12 localPeer.connection")
          eventQueue.enqueue(WebRtcEvent.IncomingPeerConnection(c))
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Connection", "", ""))

      )
      localPeer.on(
        "disconnected",
        (_: Any) =>
          scribe.debug("@@@-13 localPeer.on disconnected")
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Disconnected", "", ""))
      )

      localPeer.on(
        "close",
        (_: Any) =>
          scribe.debug("@@@-14 localPeer.on close")
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Close", "", ""))
      )

      localPeer.on(
        "error",
        (e: js.Object) =>
          scribe.error("@@@-19 LocalPeer.on error " + js.JSON.stringify(e))
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Error", "", ""))

      )
      Outcome(()).addGlobalEvents(WebRtcEvent.CreatedPeerEntity(localPeer))

    case WebRtcEvent.CreatedPeerEntity(p) =>
      scribe.debug("@@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity")
      peer = Some(p)
      latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Created", "", ""))
      Outcome(())

    case WebRtcEvent.Connect(s) =>
      val ourname = peer.get.id
      scribe.debug("@@@-30 SubSystemPeerJS WebRtcEvent.Connect remote:" + s + " -> local:" + ourname)

      val connection = peer match
        case Some(p) =>
          val conn = p.connect(s)

          conn.on(
            "open",
            (_: Any) =>
              scribe.debug("@@@-31 Connect.on open")
              eventQueue.enqueue(WebRtcEvent.ConnectionOpen)
              latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Open", "", ""))
          )
          conn.on(
            "close",
            (c: DataConnection) =>
              scribe.debug("@@@-32 Connect.on close")
              latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Close", "", ""))
          )
          conn.on(
            "error",
            (e: js.Object) =>
              scribe.error("@@@-33 Connect.on error" + js.JSON.stringify(e))
              latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Error", "", ""))
          )

          conn

        case None =>
          scribe.fatal("@@@-39 Connect None ... Attempting to connect without a peer")
          latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Error no peer", "", ""))
          val nullDataConnection = new DataConnection()
          nullDataConnection // likely to cause exception        

        Outcome(()).addGlobalEvents(WebRtcEvent.PeerCreatedConnection(connection))

    case WebRtcEvent.IncomingPeerConnection(c) =>
      scribe.debug("@@@-40 SubSystemPeerJS WebRtcEvent.IncomingPeerConnection")
      c.on(
        "data",
        (data: String) =>
          scribe.debug("@@@-41 IncomingPeerConnection.on data " + data)
          eventQueue.enqueue(WebRtcEvent.ReceivedData(data))
          latestDisplayInfo = Some(SkeletonUpdate.Info("", "", data))
          )
      
      c.on(
        "close",
        (c: DataConnection) =>
          scribe.debug("@@@-42 IncomingPeerConnection.on closed ")
          latestDisplayInfo = Some(SkeletonUpdate.Info("IncomingConnection:Closed", "", ""))
      )
      c.on(
        "error",
        (e: js.Object) =>
          scribe.error("@@@-49 IncomingPeerConnection.on error " + js.JSON.stringify(e))
          latestDisplayInfo = Some(SkeletonUpdate.Info("IncomingConnection:Error", "", ""))
      )
      Outcome(()).addGlobalEvents(WebRtcEvent.PeerCreatedConnection(c))

    case WebRtcEvent.PeerCreatedConnection(connLocal: DataConnection) =>
      scribe.debug("@@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection")
      conn = Some(connLocal)
      latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Created", "", ""))
      Outcome(())

    case WebRtcEvent.ConnectionOpen =>
      scribe.debug("@@@-60 SubSystemPeerJS WebRtcEvent.ConnectionOpen")
      conn.foreach { c =>
        c.on(
          "data",
          (data: String) =>
            scribe.debug("@@@-61 ConnectionOpen.on data " + data)
            eventQueue.enqueue(WebRtcEvent.ReceivedData(data))
            latestDisplayInfo = Some(SkeletonUpdate.Info("", "", data))            
        )

        c.on(
          "close",
          (c: DataConnection) =>
            scribe.debug("@@@-62 ConnectionOpen.on close ")
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection Closed", "", ""))
        )
        c.on(
          "error",
          (e: js.Object) =>
            scribe.error("@@@-69 ConnectionOpen.on error " + js.JSON.stringify(e))
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection Error", "", ""))
        )
      }
      Outcome(())

    case WebRtcEvent.SendData(s) =>
      scribe.debug("@@@-70 SubSystemPeerJS WebRtcEvent.SendData")
      conn.foreach { c =>
        scribe.debug("@@@-71 SendData " + s)
        c.send(s)
        latestDisplayInfo = Some(SkeletonUpdate.Info("", s, ""))   
      }
      Outcome(())

    case WebRtcEvent.ReceivedData(s) =>
      scribe.debug("@@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData")
      conn.foreach { c =>
        scribe.debug("@@@-81 ReceiveData " + s)
        latestDisplayInfo = Some(SkeletonUpdate.Info("", "", s)) 
      }  
      Outcome(())

    case WebRtcEvent.Close() =>
      scribe.debug("@@@-90 SubSystemPeerJS WebRtcEvent.Close")
      conn.foreach(_.close())
      latestDisplayInfo = Some(SkeletonUpdate.Info("Close", "", ""))   
      Outcome(())

    case _ =>
      val bEvents = ! eventQueue.isEmpty
      (bEvents, latestDisplayInfo) match
        case (true, Some(info)) =>  // ....................... Both Event(s) and Display Info
          val events = eventQueue.dequeueAll(_ => true)
          latestDisplayInfo = None
          Outcome(()).addGlobalEvents(events*).addGlobalEvents(info)
        case (true, None) => // .............................. Just Event(s)
          val events = eventQueue.dequeueAll(_ => true)
          Outcome(()).addGlobalEvents(events*)
        case (false, Some(info)) => // ......................... Just Display Info
          latestDisplayInfo = None
          Outcome(()).addGlobalEvents(info)
        case (false, None) => // ............................... Neither, idling
          Outcome(())
  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame
