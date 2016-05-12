package ornl.elision.gui.copypaste

import swing.TextComponent
import javax.swing.JPopupMenu
import javax.swing.JMenuItem
import java.awt._
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.datatransfer.Clipboard
import java.awt.datatransfer.ClipboardOwner
import java.awt.datatransfer.Transferable
import java.awt.datatransfer.StringSelection
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.UnsupportedFlavorException
import javax.swing.text.JTextComponent


/** A disposable cut-copy-paste popup menu for text components. */
class TextRClickMenu(val textComp : TextComponent) extends JPopupMenu with ActionListener {
    
    val cutItem = new JMenuItem("Cut")
    add(cutItem)
    cutItem.addActionListener(this)
    cutItem.setEnabled(hasSelection)
    
    
    val copyItem = new JMenuItem("Copy")
    add(copyItem)
    copyItem.addActionListener(this)
    copyItem.setEnabled(hasSelection)
    
    
    val pasteItem = new JMenuItem("Paste")
    add(pasteItem)
    pasteItem.addActionListener(this)
    pasteItem.setEnabled(hasPaste)
    
    
    def actionPerformed(e : ActionEvent) : Unit = {
        val source = e.getSource
        if(textComp == null) 
            return
        
        if(source == cutItem) {
            textComp.cut
        }
        if(source == copyItem) {
            textComp.copy
        }
        if(source == pasteItem) {
            textComp.paste
        }
    }
    
    
    def hasSelection : Boolean = {
        (textComp != null && textComp.selected != null && textComp.selected != "")
    }
    
    
    def hasPaste : Boolean = {
        EvaClipboard.hasString && textComp.editable
    }
    
    
    
}



// The owner of all of Eva's clipboard data. This does the under-the-hood work for the copy-paste system.
object EvaClipboard extends ClipboardOwner {
    
    def lostOwnership(clipboard : Clipboard, contents : Transferable) : Unit = {
        // do nothing
    }
    
    def setString(str : String) : Unit = {
        val strSel = new StringSelection(str)
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        clipboard.setContents(strSel, this)
    }
    
    
    def hasString : Boolean = {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val contents = clipboard.getContents(null)
        
        (contents != null && contents.isDataFlavorSupported(DataFlavor.stringFlavor))
    }
    
    def getString : String = {
        if(!hasString)
          return null
        
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val contents = clipboard.getContents(null)
        
        contents.getTransferData(DataFlavor.stringFlavor) match {
          case str : String => 
            str
          case _ =>
            ""
        }

    }
    
}



