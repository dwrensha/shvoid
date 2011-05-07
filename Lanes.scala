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
   val LANE_START = 90f

    val spawnpoints = 
      Array(new Vec2(LANE_OFFSET,-LANE_START),
            new Vec2(-LANE_START,-LANE_OFFSET),
            new Vec2(-LANE_OFFSET,LANE_START),
            new Vec2(LANE_START,LANE_OFFSET))

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
      Array((new Vec2(LANE_OFFSET,LANE_START), 5.0f),
            (new Vec2(LANE_START,-LANE_OFFSET), 5f),
            (new Vec2(-LANE_OFFSET,-LANE_START), 5f),
            (new Vec2(-LANE_START,LANE_OFFSET), 5f))


  /****2 ****
   ****  ****
   ****  ****
            3
   1
   ****  ****
   ****  ****
   **** 0****
  */


  def world2lane(v: Vec2, laneNum: Int) : Float = {
    laneNum match {
      case 0 => 
        v.y
      case 1 => 
        v.x
      case 2 => 
        -v.y
      case 3 => 
        -v.x
      case _ => 
        throw new Error("bad lane number: " + laneNum)
    }
  }



  def land2world(laneX : Float, laneNum: Int) : Vec2 = {
    laneNum match {
      case 0 => 
        new Vec2(LANE_OFFSET,  laneX )
      case 1 => 
        new Vec2(laneX, -LANE_OFFSET)
      case 2 => 
        new Vec2(-LANE_OFFSET,  -laneX )
      case 3 => 
        new Vec2(-laneX, LANE_OFFSET)
      case _ => 
        throw new Error("bad lane number: " + laneNum)
    }
  }



}
