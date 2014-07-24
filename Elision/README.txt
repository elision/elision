      _ _     _
  ___| (_)___(_) ___  _ __
 / _ \ | / __| |/ _ \| '_ \
|  __/ | \__ \ | (_) | | | |
 \___|_|_|___/_|\___/|_| |_|
The Elision Term Rewriter

Copyright (c) 2013 by UT-Battelle, LLC.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

Collection of administrative costs for redistribution of the source code or
binary form is allowed. However, collection of a royalty or other fee in excess
of good faith amount for cost recovery for such redistribution is prohibited.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Contents
========
Prerequisites
Building
Running the REPL
Eclipse
Acknowledgments
License for JLine


README
======
This is the distribution of the Elision term rewriter source code.  This short
file describes how to build the rewriter, how to run the REPL, and how to set
up your environment to edit the source in Eclipse.

Visit the Elision web site at:
http://elision.github.com/

Elision contains JLine, a Java library for command line interaction.  See the
end of this file for license details.  For more information, visit:
http://jline.sourceforge.net/


Prerequisites
=============
Elision requires Scala 2.9.2 or later to build.  Be careful with later versions
of Scala, as things do tend to change.  Java 7 or later is also a prerequisite,
as is Apache Ant 1.8 or later.

1.) Install the Java SDK.  Visit http://java.oracle.com/ to download the
    correct version for your platform.

2.) Install Scala.  Visit http://www.scala-lang.org/ to download the Scala
    distribution.

3.) Set the environment variable SCALA_HOME to point to the root folder
    of your Scala installation (the folder that contains the bin and lib
    folders).

4.) Install Apach Ant.  Visit http://ant.apache.org/ to download the Ant
    distribution.


Building
========
To build the Elision jar file, cd to the root folder of the Elision distribution
and run the command:
  ant

This will build the jar file.  If you want to build the API documentation, do
the following:
  ant docs


Running the REPL
================
Elision comes with a REPL (Read Evaluate Print Loop).  You can start it a number
of ways.  The simplest is to use the script:
  elision.sh
  
For Windows users there is an elision.bat that can be used.

The scripts run the REPL out of the bin folder, and do not require the jar to
be built.

Alternately you can execute the jar file with the scala command:
  scala latest/elision.jar
  
A jar that contains Scala is also built.  This can be run with the command:
  java -jar latest/elision-all.jar


Eclipse
=======
The root folder contains an Eclipse project.  Before you attempt to import it
into Eclipse install the Scala IDE.  Visit http://scala-ide.org/ for the latest
version, documentation, etc.

Once you have installed the Scala IDE plugin, do the following.

1.) Start Eclipse.  You will probably be prompted to run the Scala Setup
    Diagnostics.  Click Yes to run them.  Make sure you open Eclipse in the
    correct workspace where you dropped the Elision distribution.
2.) Go to the workbench.  Right-click in the package explorer and choose New
    and then Project.  In the New Project wizard expand Scala Wizards and
    select Scala Project.  Click Next.
3.) In the New Scala Project dialog, enter Elision as the Project name.  If you
    have done everything correctly, you should see a note at the bottom of the
    dialog telling you the wizard is about to automatically configure the
    project based on the existing source.  This is what you want, so click
    Finish.
4.) You may be asked if you want to switch to the Scala perspective.  You do,
    so click Yes.

At this point Elision should automatically build, and you are ready to start
working with the code.


Acknowledgments
===============
Thanks to Oak Ridge National Laboratory and the Department of Energy for
supporting this work.

Thanks to Github for hosting this open source project.

Thanks to Headway Software for providing licenses for structure101.  Find
out more about this great software here: http://www.headwaysoftware.com.


LICENSE for JLine
=================
JLine is released under the BSD "2 clause" license.  It can be found at:
http://www.opensource.org/licenses/bsd-license.php
and is reproduced below in its entirety.

Copyright (c) 2002-2006, Marc Prud'hommeaux <mwp1@cornell.edu>
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with
the distribution.

Neither the name of JLine nor the names of its contributors
may be used to endorse or promote products derived from this
software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
OF THE POSSIBILITY OF SUCH DAMAGE.
