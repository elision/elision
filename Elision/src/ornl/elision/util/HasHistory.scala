/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package ornl.elision.util

/**
 * Indicate that it a history is maintained.  Limited access to the history
 * is granted by implementing this trait.
 * 
 * @author Stacy Prowell (prowellsj@ornl.gov)
 */
trait HasHistory {  
  /**
   * Add a line to the history, if one is being maintained.  If the processor
   * maintains a history it should override this to enable adding the given
   * line to the history, if that is desired.  This is used by the system to
   * add informational lines to the history.  The default implementation does
   * nothing.
   * 
   * @param line  The line to add to the history.
   */
  def addHistoryLine(line: String) {}
  
  /**
   * Get an iterator over the history.  By default this returns the empty
   * iterator, so override this to return the appropriate iterator if your
   * processor supports history.
   */
  def getHistoryIterator: Iterator[String] = Set().iterator
  
  /**
   * Get a history entry by its index.
   * 
   * @param index The index of the history item.
   * @return  The entry, or `None` if the index does not exist in the history.
   */
  def getHistoryEntry(index: Int): Option[String] = None
  
  /**
   * Get the file that holds the persistent history.  By default this returns
   * the string `(no history file)`, so you should override this if you have
   * a history file.
   */
  def getHistoryFilename: String = "(no history file)"
  
  /** 
   * Moves the history to the previous entry and returns that entry.
   * The default implementation returns None.
   */
  def getPreviousHistoryEntry : Option[String] = None
  
  /** 
   * Moves the history to the next entry and returns that entry.
   * The default implementation returns None.
   */
  def getNextHistoryEntry : Option[String] = None
  
  /**
   * Replace an existing line in the history.  If the line does not exist,
   * nothing should be done.  This is an optional operation; the default
   * implementation does nothing.
   * 
   * @param index The index of the history item.
   * @param line  The line to replace the history item.
   */
  def replaceHistoryLine(index: Int, line: String) {}
}
