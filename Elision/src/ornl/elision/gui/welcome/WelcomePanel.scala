package ornl.elision.gui.welcome

import java.awt._
import java.awt.geom.Rectangle2D
import scala.util.Random
import scala.collection.mutable.ListBuffer

import ornl.elision.gui._

import sage2D._
import sage2D.sprites._

/** Eva's welcome mode visualization. All it does is display the Eva logo and visualize a quadtree structure for demonstrating collision detections between sprites. */
class WelcomePanel(game : GamePanel) extends Level(game, null) {
	// background = new Color(0xffffff)
	// preferredSize = new Dimension(640,480)
	
    /** The collection of BallSprites used in the demo. */
	val balls = new ListBuffer[BallSprite]
	for(i <- 1 to 100) {
		val x = Random.nextInt(640)
		val y = Random.nextInt(480)
		balls += new BallSprite(x, y, Random.nextDouble()*64 + 4)
	}
    
    /** A renderable wrapper for sage2D's quadtree. */
	val quadSprite = new QuadTreeSprite(null)
	
    /** The image for Eva's logo */
    val evaIcon = sage2D.images.ImageLoader.loadPath("EvaIcon.png")
    
    /** evaIcon's pixel width */
    var iconWidth = 0.0
    
    /** evaIcon's pixel height */
    var iconHeight = 0.0
    
    def loadData : Unit = {}
    
    def clean : Unit = {}
    
    
    
	override def logic : Unit = {
		// create a new quadtree
		val quadTree = new QuadTree(0,0,game.size.width,game.size.height,0)
		quadSprite.quadTree = quadTree
		
		// populate the quadtree
		for(ball <- balls) quadTree.insert(ball)
		
		for(ball <- balls) ball.collisionList = quadTree.query(ball)
		
		for(ball <- balls) {
		//	println("\n" + ball.collisionList + "\n")
			ball.isColliding = false
			for(sprite <- ball.collisionList) sprite match {
				case otherBall : BallSprite => 
					if(ball.collision(otherBall)) ball.isColliding = true
				case _ =>
			}
			ball.move(game)
		}
	}
	
	def render(g : Graphics2D) : Unit = {
		// store affine transforms for later use
		val origTrans = g.getTransform
		
		for(ball <- balls) {
			ball.render(g)
		}
		
		quadSprite.render(g)
		
        iconWidth = evaIcon.getWidth(game.peer)
        iconHeight = evaIcon.getHeight(game.peer)
        
        g.translate(game.size.width/2, game.size.height/2)
        g.scale(2.0, 2.0)
        g.translate(0-iconWidth/2, 0-iconHeight/2)
        g.setColor(Color.BLACK)
        g.fillRect(-1, -1, iconWidth.toInt + 2, iconHeight.toInt + 2)
        g.drawImage(evaIcon, 0, 0, null)
        
		// restore the original transform
		g.setTransform(origTrans)
		
		// display HUD information
		g.setColor(new Color(0x000000))
		g.drawString("" + game.timer.fpsCounter, 10,32)
	}
	
	// start()
   // timer.setDelay(6)
   mainGUI.consolePanel.console.emitln("Welcome to Eva - Elision Visualization Assistant!")
   mainGUI.consolePanel.console.emitln("------------------------------------------------------")
   mainGUI.consolePanel.console.emitln("Please select a mode from the Mode menu.")
}

/** 
 * A circle that bounces around the edges of its containing panel. 
 * @param _x    The sprite's initial x coordinate.
 * @param _y    The sprite's initial y coordinate.
 * @param radius    The circle's radius.
 */
class BallSprite(_x : Double, _y : Double, val radius : Double) extends Sprite(_x, _y) {
	
    /** The circle's diameter*/
    var dia = 2 * radius
    
    /** The circle's x velocity */
	var dx = Random.nextDouble()*6 - 3
    
    /** The circle's y velocity */
	var dy = Random.nextDouble()*6 - 3
	
    /** If true, the ball will be colored greenish. Otherwise, it will be drawn redish. */
	var isColliding = false
	
	override def draw(g : Graphics2D) : Unit = {
		g.translate(0-radius,0-radius)
		g.setColor( if(isColliding) new Color(0xaaffaa) else new Color(0xffaaaa))
		g.fillOval(0, 0, dia.toInt, dia.toInt)
	}
	
	override def getCollisionBox : Rectangle2D = {
		new Rectangle2D.Double(x-radius, y-radius, dia, dia)
	}
	
    /** Moves the ball and causes it to bounce if it hits the edge of its panel. */
	def move(panel : GamePanel) : Unit = {
		x += dx
		y += dy
		
		if(x < 0 && dx < 0) {
            x = 0
            dx *= -1
        }
		if(y < 0 && dy < 0) {
            y = 0
            dy *= -1
        }
		if(x > panel.size.width && dx > 0) { 
            x = panel.size.width
            dx *= -1
        }
		if(y > panel.size.height && dy > 0) {
            y = panel.size.height
            dy *= -1
        }
	}
	
    /** Tests for a collision between this and another BallSprite. */
	def collision(other : BallSprite) : Boolean = {
		if(this == other) 
			false
		else
			(GameMath.dist(this.x, this.y, other.x, other.y) < this.radius + other.radius)
	}
}


/** A renderable wrapper for sage2D's Quadtree structure. */
class QuadTreeSprite(var quadTree : QuadTree) extends Sprite(0,0) {
	override def draw(g: Graphics2D) : Unit = {
		g.setColor(new Color(0xaa0000))
		
		recDraw(g,quadTree)
	}
	
    /** Recursively draws quadrants of the quadtree. */
	def recDraw(g: Graphics2D, quad : QuadTree) : Unit = {
		g.drawRect(quad.minX.toInt, quad.minY.toInt, (quad.maxX-quad.minX).toInt, (quad.maxY-quad.minY).toInt)
		for(subQuad <- quad.quads) {
			if(subQuad != null) recDraw(g,subQuad)
		}
	}
}


