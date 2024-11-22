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
  case object MakePeerEntity extends WebRtcEvent
  case class Connect(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case class ReceivedData(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case class SendGameData(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case class Close() extends WebRtcEvent with NetworkReceiveEvent

  case class CreatedPeerEntity(peer: Peer) extends WebRtcEvent
  case class PeerAttemptConnection(conn: DataConnection, toName: String) extends WebRtcEvent
  case object PeerOpen extends WebRtcEvent with NetworkReceiveEvent
  case class PeerConnection(conn: DataConnection) extends WebRtcEvent with NetworkReceiveEvent
  case class IncomingPeerConnection(conn: DataConnection) extends WebRtcEvent with NetworkReceiveEvent
  case class PeerCreatedConnection(conn: DataConnection) extends WebRtcEvent
  case object PeerDisconnected extends WebRtcEvent with NetworkReceiveEvent
  case object PeerError extends WebRtcEvent with NetworkReceiveEvent
  case object PeerClose extends WebRtcEvent with NetworkReceiveEvent
  case object ConnectionOpen extends WebRtcEvent with NetworkReceiveEvent
  case object ConnectionClose extends WebRtcEvent with NetworkReceiveEvent
  case object ConnectionError extends WebRtcEvent with NetworkReceiveEvent
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
    scribe.debug("@@@ SubSystemPeerJS initialModel")
    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] = {

    case WebRtcEvent.MakePeerEntity =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.MakePeerEntity using " + context.reference.ourName)
      val localPeer = Peer(id = context.reference.ourName)

      localPeer.on(
        "open",
        (_: Any) => 
          scribe.debug("@@@ localPeer.on open")
          eventQueue.enqueue(WebRtcEvent.PeerOpen)
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Open", "", ""))
      )

      localPeer.on(
        "connection",
        (c: DataConnection) =>
          scribe.debug("@@@ localPeer.connection found in queue " + eventQueue.mkString(" "))
          eventQueue.enqueue(WebRtcEvent.IncomingPeerConnection(c))
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Connection", "", ""))

      )
      localPeer.on(
        "disconnected",
        (_: Any) =>
          scribe.debug("@@@ localPeer.on disconnected")
          eventQueue.enqueue(WebRtcEvent.PeerDisconnected)
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Disconnected", "", ""))
      )

      localPeer.on(
        "close",
        (_: Any) =>
          scribe.debug("@@@ localPeer.on close")
          eventQueue.enqueue(WebRtcEvent.PeerClose)
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Close", "", ""))
      )

      localPeer.on(
        "error",
        (e: js.Object) =>
          scribe.error("@@@ LocalPeer.on error " + js.JSON.stringify(e))
          eventQueue.enqueue(WebRtcEvent.PeerError)
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Error", "", ""))

      )
      Outcome(()).addGlobalEvents(WebRtcEvent.CreatedPeerEntity(localPeer))

    case WebRtcEvent.CreatedPeerEntity(p) =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.CreatedPeerEntity")
      peer = Some(p)
      latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Created", "", ""))
      Outcome(())

    case WebRtcEvent.PeerCreatedConnection(connLocal: DataConnection) =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.PeerCreatedConnection")
      conn = Some(connLocal)
      latestDisplayInfo = Some(SkeletonUpdate.Info("Connection:Created", "", ""))
      Outcome(())

    case WebRtcEvent.IncomingPeerConnection(c) =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.IncomingPeerConnection")
      c.on(
        "data",
        (data: js.Dynamic) =>
          scribe.debug("@@@ IncomingPeerConnection.on data " + js.JSON.stringify(data))
          eventQueue.enqueue(WebRtcEvent.ReceivedData(js.JSON.stringify(data)))
          latestDisplayInfo = Some(SkeletonUpdate.Info("", "", js.JSON.stringify(data)))
          )
      
      c.on(
        "close",
        (c: DataConnection) =>
          scribe.debug("@@@ IncomingPeerConnection.on closed ")
          eventQueue.enqueue(WebRtcEvent.ConnectionClose)
          latestDisplayInfo = Some(SkeletonUpdate.Info("IncomingConnection:Closed", "", ""))
      )
      c.on(
        "error",
        (e: js.Object) =>
          scribe.error("@@@ IncomingPeerConnection.on error " + js.JSON.stringify(e))
          eventQueue.enqueue(WebRtcEvent.ConnectionError)
          latestDisplayInfo = Some(SkeletonUpdate.Info("IncomingConnection:Error", "", ""))
      )
      Outcome(()).addGlobalEvents(WebRtcEvent.PeerCreatedConnection(c))

    case WebRtcEvent.ConnectionOpen =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.ConnectionOpen")
      conn.foreach { c =>
        c.on(
          "data",
          (data: js.Dynamic) =>
            scribe.debug("@@@ ConnectionOpen.on data " + js.JSON.stringify(data))
            eventQueue.enqueue(WebRtcEvent.ReceivedData(js.JSON.stringify(data)))
            latestDisplayInfo = Some(SkeletonUpdate.Info("", "", js.JSON.stringify(data)))            
        )

        c.on(
          "close",
          (c: DataConnection) =>
            scribe.debug("@@@ ConnectionOpen.on close ")
            eventQueue.enqueue(WebRtcEvent.ConnectionClose)
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection Closed", "", ""))
        )
        c.on(
          "error",
          (e: js.Object) =>
            scribe.error("@@@ ConnectionOpen.on error " + js.JSON.stringify(e))
            eventQueue.enqueue(WebRtcEvent.ConnectionError)
            latestDisplayInfo = Some(SkeletonUpdate.Info("Connection Error", "", ""))
        )
      }
      Outcome(())

    case WebRtcEvent.Connect(s) =>
      val ourname = peer.get.id
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.Connect from " + ourname + " to " + s)

      val connection = peer match
        case Some(p) =>
          val conn = p.connect(s)

          conn.on(
            "open",
            (_: Any) =>
              scribe.debug("@@@ Connect.on open")
              eventQueue.enqueue(WebRtcEvent.ConnectionOpen)
              latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Open", "", ""))
          )
          conn.on(
            "close",
            (c: DataConnection) =>
              scribe.debug("@@@ Connect.on close")
              eventQueue.enqueue(WebRtcEvent.ConnectionClose)
              latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Close", "", ""))
          )
          conn.on(
            "error",
            (e: js.Object) =>
              scribe.error("Connect.on error" + js.JSON.stringify(e))
              eventQueue.enqueue(WebRtcEvent.ConnectionError)
              latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Error", "", ""))
          )

          conn

        case None =>
          scribe.fatal("Connect None ... Attempting to connect without a peer")
          latestDisplayInfo = Some(SkeletonUpdate.Info("Connect Error no peer", "", ""))
          val nullDataConnection = new DataConnection()
          nullDataConnection // likely to cause exception        

        Outcome(()).addGlobalEvents(WebRtcEvent.PeerCreatedConnection(connection))

    case WebRtcEvent.SendGameData(s) =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.SendGameData")
      conn.foreach { c =>
        scribe.debug("@@@ SendGameData " + s)
        c.send(s)
        latestDisplayInfo = Some(SkeletonUpdate.Info("", s, ""))   
      }
      Outcome(())

    case WebRtcEvent.Close() =>
      scribe.debug("@@@ SubSystemPeerJS WebRtcEvent.Close")
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
