package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode


final case class SkeletonGameModel(
    ourName: String,
    oppoName: String,
    playerNo: Int,
    status: String,
    tx: String,
    rx:String
) derives Encoder.AsObject,
      Decoder
object SkeletonGameModel:
  scribe.debug("@@@ Object SkeletonGameModel Start")

  def creation(name1:String, name2:String): SkeletonGameModel =
    scribe.debug("@@@ SkeletonGameModel creation")

    val pNo = 
      if (name1.compareTo(name2) > 0) then 2
      else 1 
      end if

    SkeletonGameModel(
      name1,
      name2,
      pNo,
      "Startup",
      "---",
      "---"
    )
  end creation
  scribe.debug("@@@ Object SkeletonGameModel Finish")

end SkeletonGameModel
