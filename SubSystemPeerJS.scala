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

// case class SkeletonNewInformationEvent(ffgm: SkeletonGameModel)
//     extends SubSystemEvent

sealed trait WebRtcEvent extends GlobalEvent
object WebRtcEvent:
  case object MakePeerEntity extends WebRtcEvent
  case class CreatedPeerEntity(peer: Peer) extends WebRtcEvent
  case class PeerAttemptConnection(conn: DataConnection, toName: String)
      extends WebRtcEvent
  case class Connect(s: String) extends WebRtcEvent with NetworkReceiveEvent
  case object PeerOpen extends WebRtcEvent with NetworkReceiveEvent
  case class PeerConnection(conn: DataConnection)
      extends WebRtcEvent
      with NetworkReceiveEvent

  case class IncomingPeerConnection(conn: DataConnection)
      extends WebRtcEvent
      with NetworkReceiveEvent

  case class PeerCreatedConnection(conn: DataConnection) extends WebRtcEvent

  case object PeerDisconnected extends WebRtcEvent with NetworkReceiveEvent
  case object PeerError extends WebRtcEvent with NetworkReceiveEvent
  case object PeerClose extends WebRtcEvent with NetworkReceiveEvent

  case object ConnectionOpen extends WebRtcEvent with NetworkReceiveEvent
  case object ConnectionClose extends WebRtcEvent with NetworkReceiveEvent
  case object ConnectionError extends WebRtcEvent with NetworkReceiveEvent

  case class ReceivedData(s: String)
      extends WebRtcEvent
      with NetworkReceiveEvent
  case class SendGameData(s: String)
      extends WebRtcEvent
      with NetworkReceiveEvent
  case class Close() extends WebRtcEvent with NetworkReceiveEvent
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

  // val eventFilter: GlobalEvent => Option[WebRtcEvent] = {
  //  case e: WebRtcEvent => Some(e)
  //  case _              => None
  // }

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
      scribe.debug(
        "@@@ SubSystemPeerJS WebRtcEvent MakePeerEntity using " + context.reference.ourName
      )
      scribe.debug(s"ourname : ${context.reference.ourName}")
      val localPeer = Peer(id = context.reference.ourName)

      localPeer.on(
        "open",
        (_: Any) => eventQueue.enqueue(WebRtcEvent.PeerOpen)
      )

      localPeer.on(
        "connection",
        (c: DataConnection) =>
          scribe.debug(s"found in queue ${eventQueue.mkString(" ")}")
          // scribe.debug(s"found in queue ${js.JSON.stringify(c)} ")
          eventQueue.enqueue(WebRtcEvent.IncomingPeerConnection(c))
      )
      localPeer.on(
        "disconnected",
        (_: Any) =>
          eventQueue.enqueue(WebRtcEvent.PeerDisconnected)
          latestDisplayInfo =
            Some(SkeletonUpdate.Info("Peer:Disconnected", "", ""))
          scribe.debug("@@@ peer.on disconnected")
      )

      localPeer.on(
        "close",
        (_: Any) =>
          eventQueue.enqueue(WebRtcEvent.PeerClose)
          latestDisplayInfo = Some(SkeletonUpdate.Info("Peer:Close", "", ""))
          scribe.debug("@@@ peer.on close")
      )

      localPeer.on(
        "error",
        (e: js.Object) =>
          eventQueue.enqueue(WebRtcEvent.PeerError)
          scribe.error(s"PEER ERROR ${js.JSON.stringify(e)}")
      )
      Outcome(()).addGlobalEvents(WebRtcEvent.CreatedPeerEntity(localPeer))

    case WebRtcEvent.CreatedPeerEntity(p) =>
      scribe.debug("Setting peer")
      peer = Some(p)
      Outcome(())

    case WebRtcEvent.PeerCreatedConnection(connLocal: DataConnection) =>
      scribe.debug("Setting connection")
      conn = Some(connLocal)
      Outcome(())

    case WebRtcEvent.IncomingPeerConnection(c) =>
      scribe.debug("Setting connection")
      c.on(
        "data",
        (data: js.Dynamic) =>
          scribe.debug(s"@@@ recieved data ${js.JSON.stringify(data)}")
          eventQueue.enqueue(
            WebRtcEvent.ReceivedData(js.JSON.stringify(data))
          )
      )

      c.on(
        "close",
        (c: DataConnection) =>
          scribe.debug(s"connection closed")
          eventQueue.enqueue(WebRtcEvent.ConnectionClose)
      )
      c.on(
        "error",
        (e: js.Object) =>
          scribe.error(s"CONNECTION ERROR ${js.JSON.stringify(e)}")
          eventQueue.enqueue(WebRtcEvent.ConnectionError)
      )

      Outcome(()).addGlobalEvents(WebRtcEvent.PeerCreatedConnection(c))

    case WebRtcEvent.ConnectionOpen =>
      conn.foreach { c =>
        c.on(
          "data",
          (data: js.Dynamic) =>
            scribe.debug(s"@@@ recieved data ${js.JSON.stringify(data)}")
            eventQueue.enqueue(
              WebRtcEvent.ReceivedData(js.JSON.stringify(data))
            )
        )

        c.on(
          "close",
          (c: DataConnection) =>
            scribe.debug(s"connection closed")
            eventQueue.enqueue(WebRtcEvent.ConnectionClose)
        )
        c.on(
          "error",
          (e: js.Object) =>
            scribe.error(s"CONNECTION ERROR ${js.JSON.stringify(e)}")
            eventQueue.enqueue(WebRtcEvent.ConnectionError)
        )

      }
      Outcome(())

    case WebRtcEvent.Connect(s) =>
      val ourname = peer.get.id
      scribe.debug(
        s"Connect from $ourname to $s"
      )

      val connection = peer match
        case Some(p) =>
          val conn = p.connect(s)

          conn.on(
            "open",
            (_: Any) =>
              scribe.debug(s"connection open")
              eventQueue.enqueue(WebRtcEvent.ConnectionOpen)
          )
          conn.on(
            "close",
            (c: DataConnection) =>
              scribe.debug(s"connection closed")
              eventQueue.enqueue(WebRtcEvent.ConnectionClose)
          )
          conn.on(
            "error",
            (e: js.Object) =>
              scribe.error(s"CONNECTION ERROR ${js.JSON.stringify(e)}")
              eventQueue.enqueue(WebRtcEvent.ConnectionError)
          )

          conn

        case None =>
          // scribe.error("Attempt to connect without a peer")
          throw new Exception("Attempt to connect without a peer")

      Outcome(()).addGlobalEvents(WebRtcEvent.PeerCreatedConnection(connection))

    case WebRtcEvent.SendGameData(s) =>
      conn.foreach { c =>
        scribe.debug("SendGameData " + s)

        c.send(js.Dynamic.literal("data" -> s))
      }
      Outcome(())

    case WebRtcEvent.Close() =>
      scribe.debug("Close")
      conn.foreach(_.close())
      Outcome(())

    case _ =>
      // println(eventQueue.mkString(" "))
      if (eventQueue.isEmpty)
        Outcome(())
      else {
        scribe.debug(s"FOund in queue : ${eventQueue.mkString(",")}")
        val events = eventQueue.dequeueAll(_ => true)
        Outcome(()).addGlobalEvents(events*)
      }

  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame
