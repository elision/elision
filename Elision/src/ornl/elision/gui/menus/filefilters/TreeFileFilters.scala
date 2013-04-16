/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/

package ornl.elision.gui.menus.filefilters


/** A FileFilter that only accepts .treexml files */
class TreeXMLFileFilter extends javax.swing.filechooser.FileFilter  {
    def accept(f : java.io.File) : Boolean = {
        if(f.isDirectory) return true
        
        val name = f.getName
        val lastDot = name.lastIndexOf('.')
        val ext = name.drop(lastDot+1)
        if(ext == "treexml") true
        else false
    }
    
    def getDescription : String = {
        "Eva treexml files"
    }
}

/** A FileFilter that only accepts .treejson files */
class TreeJSONFileFilter extends javax.swing.filechooser.FileFilter  {
    def accept(f : java.io.File) : Boolean = {
        if(f.isDirectory) return true
        
        val name = f.getName
        val lastDot = name.lastIndexOf('.')
        val ext = name.drop(lastDot+1)
        if(ext == "treejson") true
        else false
    }
    
    def getDescription : String = {
        "Eva treejson files"
    }
}


/** A FileFilter that only accepts .treexml and .treejson files */
class TreeFileFilter extends javax.swing.filechooser.FileFilter  {
    def accept(f : java.io.File) : Boolean = {
        if(f.isDirectory) return true
        
        val name = f.getName
        val lastDot = name.lastIndexOf('.')
        val ext = name.drop(lastDot+1)
        if(ext == "treejson" || ext == "treexml") true
        else false
    }
    
    def getDescription : String = {
        "Eva treejson and treexml files"
    }
}
