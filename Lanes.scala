import org.jbox2d.common.Vec2

object Lanes {

    val pi = 3.14159265f



  /****2 ****
   ****  ****
   ****  ****
            3
   1
   ****  ****
   ****  ****
   **** 0****
  */
   val LANE_OFFSET = 1.5f

    val spawnpoints = 
      Array(new Vec2(LANE_OFFSET,-90f),
            new Vec2(-90f,-LANE_OFFSET),
            new Vec2(-LANE_OFFSET,90f),
            new Vec2(90f,LANE_OFFSET))

    val spawnvels = 
      Array(new Vec2(0f,5f),
            new Vec2(5f,0f),
            new Vec2(0f,-5f),
            new Vec2(-5f,0f))

    val spawnangles = 
      Array(pi / 2.0f,
            0f,
            3f * pi / 2f,
            pi
      )

    val goals = 
      Array((new Vec2(LANE_OFFSET,90f), 5.0f),
            (new Vec2(90f,-LANE_OFFSET), 5f),
            (new Vec2(-LANE_OFFSET,-90f), 5f),
            (new Vec2(-90f,LANE_OFFSET), 5f))





  def world2lane(worldVec: Vec2, laneNum: Int) : Float = {
    laneNum match {
      case 0 => 
      case _ => 
          
    }
    return 0f;
  }

  def land2world(laneX : Float, laneNum: Int) : Vec2 = {
    return new Vec2(0f,0f)
  }



}
