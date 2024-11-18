package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class SkeletonGameModel(
    ourName: String,
    oppoName: String,
) derives Encoder.AsObject,
      Decoder


object SkeletonGameModel:
  scribe.debug("@@@ Object SkeletonGameModel Start")

  def creation(): SkeletonGameModel =
    scribe.debug("@@@ SkeletonGameModel creation")    

    val sOurName = "ABC"
    val sOppoName = "DEF"

    SkeletonGameModel(
      sOurName,
      sOppoName,
    )
  end creation


  scribe.debug("@@@ Object SkeletonGameModel Finish")
end SkeletonGameModel
