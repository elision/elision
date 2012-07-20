package ornl.elision.gui.welcome

import java.awt._
import java.awt.geom.Rectangle2D
import util.Random
import collection.mutable.ListBuffer

import sage2D._


class WelcomePanel extends GamePanel {
	background = new Color(0xffffff)
	preferredSize = new Dimension(640,480)
	
	val balls = new ListBuffer[BallSprite]
	for(i <- 1 to 100) {
		val x = Random.nextInt(640)
		val y = Random.nextInt(480)
		balls += new BallSprite(x, y, Random.nextDouble()*64 + 4)
	}
	val quadSprite = new QuadTreeSprite(null)
	
    val evaIcon = ImageLoader.loadPath("EvaIcon.png")
    var iconWidth = 0.0
    var iconHeight = 0.0
    
	def timerLoop : Unit = {
		// create a new quadtree
		val quadTree = new QuadTree(0,0,this.size.width,this.size.height)
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
			ball.move(this)
		}
	}
	
	def mainPaint(g : Graphics2D) : Unit = {
		// store affine transforms for later use
		val origTrans = g.getTransform
		
		for(ball <- balls) {
			ball.render(g)
		}
		
		quadSprite.render(g)
		
        iconWidth = evaIcon.getWidth(this.peer)
        iconHeight = evaIcon.getHeight(this.peer)
        
        g.translate(size.width/2, size.height/2)
        g.scale(2.0, 2.0)
        g.translate(0-iconWidth/2, 0-iconHeight/2)
        g.setColor(Color.BLACK)
        g.fillRect(-1, -1, iconWidth.toInt + 2, iconHeight.toInt + 2)
        g.drawImage(evaIcon, 0, 0, null)
        
		// restore the original transform
		g.setTransform(origTrans)
		
		// display HUD information
		g.setColor(new Color(0x000000))
		g.drawString("" + timer.fpsCounter, 10,32)
	}
	
	def loadingPaint(g : Graphics2D) : Unit = {
	
	}
	
	start()
   // timer.setDelay(6)
   System.out.println("Welcome to Eva - Elision Visualization Assistant!")
   System.out.println("------------------------------------------------------")
   System.out.println("Please select a mode from the Mode menu.")
}

class BallSprite(val _x : Double, val _y : Double, val radius : Double) extends Sprite(_x, _y) {
	var dia = 2 * radius
	var dx = Random.nextDouble()*6 - 3
	var dy = Random.nextDouble()*6 - 3
	
	var isColliding = false
	
	override def draw(g : Graphics2D) : Unit = {
		g.translate(0-radius,0-radius)
		g.setColor( if(isColliding) new Color(0xaaffaa) else new Color(0xffaaaa))
		g.fillOval(0, 0, dia.toInt, dia.toInt)
	}
	
	override def getCollisionBox : Rectangle2D = {
		new Rectangle2D.Double(x-radius, y-radius, dia, dia)
	}
	
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
	
	def collision(other : BallSprite) : Boolean = {
		if(this == other) 
			false
		else
			(GameMath.dist(this.x, this.y, other.x, other.y) < this.radius + other.radius)
	}
}



class QuadTreeSprite(var quadTree : QuadTree) extends Sprite(0,0) {
	override def draw(g: Graphics2D) : Unit = {
		g.setColor(new Color(0xaa0000))
		
		recDraw(g,quadTree)
	}
	
	def recDraw(g: Graphics2D, quad : QuadTree) : Unit = {
		g.drawRect(quad.minX.toInt, quad.minY.toInt, (quad.maxX-quad.minX).toInt, (quad.maxY-quad.minY).toInt)
		for(subQuad <- quad.quads) {
			if(subQuad != null) recDraw(g,subQuad)
		}
	}
}


